#!/usr/bin/env python3
"""Fetch current weather for the Qtile Outrun bar using Open-Meteo."""

from __future__ import annotations

import json
import os
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path
from typing import Any

GEOCODE_URL = "https://geocoding-api.open-meteo.com/v1/search"
FORECAST_URL = "https://api.open-meteo.com/v1/forecast"
CACHE_SECONDS = 300
REQUEST_TIMEOUT = 5

WEATHER_ICONS = {
    0: "󰖙", 1: "󰖕", 2: "󰖐", 3: "󰖐", 45: "󰖑", 48: "󰖑",
    51: "󰖗", 53: "󰖗", 55: "󰖗", 56: "󰖗", 57: "󰖗",
    61: "󰖖", 63: "󰖖", 65: "󰖖", 66: "󰖖", 67: "󰖖",
    71: "󰼶", 73: "󰼶", 75: "󰼶", 77: "󰼶", 80: "󰖖",
    81: "󰖖", 82: "󰖖", 85: "󰼶", 86: "󰼶", 95: "󰙾",
    96: "󰙾", 99: "󰙾",
}


def cache_path() -> Path:
    root = Path(os.environ.get("XDG_CACHE_HOME", "~/.cache")).expanduser()
    return root / "qtile" / "weather.json"


def request_json(url: str) -> dict[str, Any]:
    request = urllib.request.Request(url, headers={"User-Agent": "qtile-weather/1"})
    try:
        with urllib.request.urlopen(request, timeout=REQUEST_TIMEOUT) as response:
            payload = json.load(response)
    except (urllib.error.URLError, urllib.error.HTTPError, TimeoutError) as exc:
        raise RuntimeError("weather unavailable") from exc
    if not isinstance(payload, dict):
        raise RuntimeError("weather payload malformed")
    return payload


def configured_coordinates() -> tuple[float, float, str] | None:
    latitude = os.environ.get("WEATHER_LATITUDE")
    longitude = os.environ.get("WEATHER_LONGITUDE")
    if latitude and longitude:
        try:
            return float(latitude), float(longitude), os.environ.get("WEATHER_LABEL", "")
        except ValueError as exc:
            raise RuntimeError("weather coordinates malformed") from exc
    return None


def geocode_location(location: str) -> tuple[float, float, str]:
    query = urllib.parse.urlencode({"name": location, "count": 1, "language": "en", "format": "json"})
    payload = request_json(f"{GEOCODE_URL}?{query}")
    results = payload.get("results", [])
    if not isinstance(results, list) or not results:
        raise RuntimeError("weather location not found")
    result = results[0]
    try:
        latitude = float(result["latitude"])
        longitude = float(result["longitude"])
    except (KeyError, TypeError, ValueError) as exc:
        raise RuntimeError("weather geocode malformed") from exc
    label = str(result.get("name") or location)
    return latitude, longitude, label


def resolve_location() -> tuple[float, float, str]:
    configured = configured_coordinates()
    if configured:
        return configured
    location = os.environ.get("WEATHER_LOCATION", "").strip()
    if not location:
        raise RuntimeError("set WEATHER_LOCATION")
    return geocode_location(location)


def weather_url(latitude: float, longitude: float) -> str:
    metric = os.environ.get("WEATHER_UNITS", "imperial").casefold() == "metric"
    params = {
        "latitude": latitude,
        "longitude": longitude,
        "current": "temperature_2m,apparent_temperature,precipitation,weather_code,wind_speed_10m",
        "timezone": "auto",
        "temperature_unit": "celsius" if metric else "fahrenheit",
        "wind_speed_unit": "kmh" if metric else "mph",
        "precipitation_unit": "mm" if metric else "inch",
    }
    return f"{FORECAST_URL}?{urllib.parse.urlencode(params)}"


def parse_current(payload: dict[str, Any], label: str = "") -> dict[str, Any]:
    current = payload.get("current", {})
    if not isinstance(current, dict):
        raise RuntimeError("weather current payload malformed")
    try:
        temperature = float(current["temperature_2m"])
        apparent = float(current["apparent_temperature"])
        wind = float(current["wind_speed_10m"])
        code = int(current["weather_code"])
    except (KeyError, TypeError, ValueError) as exc:
        raise RuntimeError("weather current payload malformed") from exc
    return {"label": label, "temperature": temperature, "apparent": apparent, "wind": wind, "code": code}


def render(weather: dict[str, Any]) -> str:
    icon = WEATHER_ICONS.get(int(weather["code"]), "󰖐")
    metric = os.environ.get("WEATHER_UNITS", "imperial").casefold() == "metric"
    degree = "C" if metric else "F"
    wind_unit = "km/h" if metric else "mph"
    location = f"{weather['label']} " if weather.get("label") else ""
    return (
        f"{icon} {location}{weather['temperature']:.0f}°{degree} "
        f"feels {weather['apparent']:.0f}° · {weather['wind']:.0f} {wind_unit}"
    )


def read_cache(path: Path) -> dict[str, Any] | None:
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError, TypeError):
        return None
    return payload if isinstance(payload, dict) else None


def fetch_weather() -> dict[str, Any]:
    latitude, longitude, label = resolve_location()
    return parse_current(request_json(weather_url(latitude, longitude)), label)


def status() -> tuple[dict[str, Any], bool]:
    path = cache_path()
    cache = read_cache(path)
    now = time.time()
    if cache and now - float(cache.get("fetched_at", 0)) < CACHE_SECONDS:
        return cache["weather"], False
    try:
        weather = fetch_weather()
    except RuntimeError:
        if cache and isinstance(cache.get("weather"), dict):
            return cache["weather"], True
        raise
    path.parent.mkdir(parents=True, exist_ok=True)
    temp = path.with_suffix(".tmp")
    temp.write_text(json.dumps({"fetched_at": now, "weather": weather}, separators=(",", ":")), encoding="utf-8")
    temp.replace(path)
    return weather, False


def main() -> int:
    try:
        weather, stale = status()
    except RuntimeError as exc:
        print(f"󰖐 {exc}")
        return 1
    text = render(weather)
    print(f"{text}{' ~' if stale else ''}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
