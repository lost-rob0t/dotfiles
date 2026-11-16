"""Private Zara tools for Google Lyria music generation through OpenRouter.

The plugin intentionally keeps credentials and mutable output outside Git/Nix.
Set OPENROUTER_API_KEY in Zara's existing owner-private environment file.
"""

from __future__ import annotations

import base64
import json
import mimetypes
import os
import re
import time
import unicodedata
from pathlib import Path
from typing import Any, Dict, Iterable, Optional
from urllib.error import HTTPError, URLError
from urllib.request import Request, urlopen

from langchain_core.tools import tool


OPENROUTER_BASE_URL = "https://openrouter.ai/api/v1"
CLIP_MODEL = "google/lyria-3-clip-preview"
PRO_MODEL = "google/lyria-3-pro-preview"
DEFAULT_TIMEOUT_SECONDS = 180.0
DEFAULT_MAX_AUDIO_BYTES = 64 * 1024 * 1024
MAX_REFERENCE_IMAGE_BYTES = 10 * 1024 * 1024
SUPPORTED_IMAGE_MIME_TYPES = {"image/jpeg", "image/png", "image/webp"}


def _render(value: Any) -> str:
    return json.dumps(value, ensure_ascii=False, indent=2, sort_keys=True)


def _env_truthy(name: str) -> bool:
    return os.getenv(name, "").strip().lower() in {"1", "true", "yes", "on"}


def _timeout_seconds() -> float:
    raw = os.getenv("LYRIA_TIMEOUT_SECONDS", str(DEFAULT_TIMEOUT_SECONDS))
    try:
        timeout = float(raw)
    except ValueError:
        return DEFAULT_TIMEOUT_SECONDS
    return min(max(timeout, 5.0), 600.0)


def _max_audio_bytes() -> int:
    raw = os.getenv("LYRIA_MAX_AUDIO_BYTES", str(DEFAULT_MAX_AUDIO_BYTES))
    try:
        size = int(raw)
    except ValueError:
        return DEFAULT_MAX_AUDIO_BYTES
    return min(max(size, 1024 * 1024), 256 * 1024 * 1024)


def _output_dir() -> Path:
    raw = os.getenv("LYRIA_OUTPUT_DIR", "~/Music/Zara")
    return Path(raw).expanduser()


def _slugify(value: str) -> str:
    normalized = unicodedata.normalize("NFKD", value).encode("ascii", "ignore").decode("ascii")
    slug = re.sub(r"[^a-zA-Z0-9]+", "-", normalized).strip("-").lower()
    return slug[:64]


def _output_path(title: str, model_kind: str, extension: str) -> Path:
    directory = _output_dir()
    directory.mkdir(parents=True, exist_ok=True, mode=0o700)
    slug = _slugify(title) or f"lyria-{model_kind}"
    stamp = time.strftime("%Y%m%d-%H%M%S")
    base = directory / f"{stamp}-{slug}.{extension}"
    if not base.exists():
        return base
    for index in range(1, 1000):
        candidate = directory / f"{stamp}-{slug}-{index}.{extension}"
        if not candidate.exists():
            return candidate
    raise RuntimeError("could not allocate a unique output filename")


def _write_private(path: Path, data: bytes) -> None:
    fd = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
    try:
        with os.fdopen(fd, "wb") as handle:
            handle.write(data)
    except Exception:
        path.unlink(missing_ok=True)
        raise


def _reference_image_part(image_path: str) -> Optional[Dict[str, Any]]:
    raw = image_path.strip()
    if not raw:
        return None
    path = Path(raw).expanduser().resolve()
    if not path.is_file():
        raise ValueError(f"reference image does not exist: {path}")
    size = path.stat().st_size
    if size > MAX_REFERENCE_IMAGE_BYTES:
        raise ValueError(f"reference image exceeds {MAX_REFERENCE_IMAGE_BYTES} bytes")
    mime_type = mimetypes.guess_type(path.name)[0] or "application/octet-stream"
    if mime_type not in SUPPORTED_IMAGE_MIME_TYPES:
        raise ValueError("reference image must be JPEG, PNG, or WebP")
    encoded = base64.b64encode(path.read_bytes()).decode("ascii")
    return {
        "type": "image_url",
        "image_url": {"url": f"data:{mime_type};base64,{encoded}"},
    }


def _prompt_text(
    prompt: str,
    *,
    lyrics: str,
    instrumental: bool,
    duration_seconds: int,
) -> str:
    clean_prompt = prompt.strip()
    if not clean_prompt:
        raise ValueError("prompt is required")
    if duration_seconds < 0 or duration_seconds > 180:
        raise ValueError("duration_seconds must be between 0 and 180")

    parts = [clean_prompt]
    if instrumental:
        parts.append("Instrumental only. No vocals, sung lyrics, or spoken word.")
    if lyrics.strip():
        parts.append(f"Lyrics:\n{lyrics.strip()}")
    if duration_seconds:
        parts.append(f"Target duration: about {duration_seconds} seconds.")
    return "\n\n".join(parts)


def _message_content(prompt: str, image_path: str) -> Any:
    image = _reference_image_part(image_path)
    if image is None:
        return prompt
    return [{"type": "text", "text": prompt}, image]


def _provider_policy() -> Dict[str, Any]:
    provider: Dict[str, Any] = {"data_collection": "deny"}
    if _env_truthy("LYRIA_REQUIRE_ZDR"):
        provider["zdr"] = True
    return provider


def _extract_stream_audio(lines: Iterable[bytes], max_bytes: int) -> tuple[bytes, str]:
    audio_chunks: list[str] = []
    transcript_chunks: list[str] = []
    base64_chars = 0
    max_base64_chars = ((max_bytes + 2) // 3) * 4
    saw_done = False

    for raw_line in lines:
        line = raw_line.decode("utf-8", errors="strict").strip()
        if not line.startswith("data:"):
            continue
        data = line[len("data:") :].strip()
        if not data:
            continue
        if data == "[DONE]":
            saw_done = True
            break

        event = json.loads(data)
        if isinstance(event, dict) and isinstance(event.get("error"), dict):
            message = str(event["error"].get("message") or "unknown provider stream error")
            raise RuntimeError(f"OpenRouter music generation failed: {message}")

        choices = event.get("choices") if isinstance(event, dict) else None
        if not isinstance(choices, list) or not choices:
            continue
        first = choices[0]
        if not isinstance(first, dict):
            continue
        delta = first.get("delta")
        if not isinstance(delta, dict):
            continue
        audio = delta.get("audio")
        if not isinstance(audio, dict):
            continue

        chunk = audio.get("data")
        if isinstance(chunk, str) and chunk:
            base64_chars += len(chunk)
            if base64_chars > max_base64_chars:
                raise ValueError(f"generated audio exceeds {max_bytes} bytes")
            audio_chunks.append(chunk)
        transcript = audio.get("transcript")
        if isinstance(transcript, str) and transcript:
            transcript_chunks.append(transcript)

    if not saw_done:
        raise RuntimeError("OpenRouter music stream ended before completion")
    if not audio_chunks:
        raise RuntimeError("OpenRouter music generation returned no audio")

    try:
        audio_bytes = base64.b64decode("".join(audio_chunks), validate=True)
    except (ValueError, base64.binascii.Error) as error:
        raise RuntimeError("OpenRouter returned malformed base64 audio") from error
    if len(audio_bytes) > max_bytes:
        raise ValueError(f"generated audio exceeds {max_bytes} bytes")
    return audio_bytes, "".join(transcript_chunks)


def _http_error_message(error: HTTPError) -> str:
    try:
        body = error.read(8192).decode("utf-8", errors="replace")
        payload = json.loads(body)
        if isinstance(payload, dict):
            nested = payload.get("error")
            if isinstance(nested, dict) and nested.get("message"):
                return str(nested["message"])
    except Exception:
        pass
    return f"HTTP {error.code}"


def _generate(
    *,
    model: str,
    model_kind: str,
    prompt: str,
    title: str,
    lyrics: str,
    instrumental: bool,
    image_path: str,
    duration_seconds: int,
) -> str:
    api_key = os.getenv("OPENROUTER_API_KEY", "").strip()
    if not api_key:
        return _render({"ok": False, "error": "OPENROUTER_API_KEY is not configured"})

    try:
        prompt_text = _prompt_text(
            prompt,
            lyrics=lyrics,
            instrumental=instrumental,
            duration_seconds=duration_seconds,
        )
        content = _message_content(prompt_text, image_path)
    except (OSError, ValueError) as error:
        return _render({"ok": False, "error": str(error)})

    body = {
        "model": model,
        "messages": [{"role": "user", "content": content}],
        "modalities": ["text", "audio"],
        "audio": {"format": "mp3"},
        "provider": _provider_policy(),
        "stream": True,
    }
    request = Request(
        f"{OPENROUTER_BASE_URL}/chat/completions",
        data=json.dumps(body, ensure_ascii=False).encode("utf-8"),
        headers={
            "Accept": "text/event-stream",
            "Authorization": f"Bearer {api_key}",
            "Content-Type": "application/json",
        },
        method="POST",
    )

    try:
        with urlopen(request, timeout=_timeout_seconds()) as response:
            audio_bytes, transcript = _extract_stream_audio(response, _max_audio_bytes())
        path = _output_path(title, model_kind, "mp3")
        _write_private(path, audio_bytes)
    except HTTPError as error:
        return _render({"ok": False, "error": _http_error_message(error), "status": error.code})
    except URLError as error:
        return _render({"ok": False, "error": f"OpenRouter connection failed: {error.reason}"})
    except (OSError, TimeoutError, ValueError, RuntimeError, json.JSONDecodeError) as error:
        return _render({"ok": False, "error": str(error)})

    result: Dict[str, Any] = {
        "ok": True,
        "model": model,
        "path": str(path),
        "bytes": len(audio_bytes),
        "mime_type": "audio/mpeg",
    }
    if transcript:
        result["transcript"] = transcript
    return _render(result)


@tool("lyria_generate_clip")
def lyria_generate_clip(
    prompt: str,
    title: str = "",
    lyrics: str = "",
    instrumental: bool = False,
    image_path: str = "",
    duration_seconds: int = 30,
) -> str:
    """Generate a short music clip with Google Lyria through OpenRouter and save it locally."""
    return _generate(
        model=CLIP_MODEL,
        model_kind="clip",
        prompt=prompt,
        title=title,
        lyrics=lyrics,
        instrumental=instrumental,
        image_path=image_path,
        duration_seconds=duration_seconds,
    )


@tool("lyria_generate_song")
def lyria_generate_song(
    prompt: str,
    title: str = "",
    lyrics: str = "",
    instrumental: bool = False,
    image_path: str = "",
    duration_seconds: int = 0,
) -> str:
    """Generate a full song with Google Lyria through OpenRouter and save it locally."""
    return _generate(
        model=PRO_MODEL,
        model_kind="song",
        prompt=prompt,
        title=title,
        lyrics=lyrics,
        instrumental=instrumental,
        image_path=image_path,
        duration_seconds=duration_seconds,
    )


def register_tools(_prolog_engine=None):
    """Entry point used by Zara's dynamic plugin loader."""
    return [lyria_generate_clip, lyria_generate_song]
