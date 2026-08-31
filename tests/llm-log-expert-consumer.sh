#!/usr/bin/env bash
set -euo pipefail

module="nix/home-manager/mods/llm.nix"
old_revision="94b79dccd9010b429eff0472c74391fa50b4db0a"

fail() {
  printf 'RED: %s\n' "$*" >&2
  exit 1
}

grep -Fq 'dataDir = "${config.home.homeDirectory}/Documents/AI/proxy";' "$module" \
  || fail 'raw capture path moved from ~/Documents/AI/proxy'

grep -Fq 'openai = "https://api.openai.com";' "$module" \
  || fail 'OpenAI upstream routing changed'
grep -Fq 'openrouter = "https://openrouter.ai";' "$module" \
  || fail 'OpenRouter upstream routing changed'
grep -Fq 'anthropic = "https://api.anthropic.com";' "$module" \
  || fail 'Anthropic upstream routing changed'
grep -Fq 'chatgpt = "https://chatgpt.com";' "$module" \
  || fail 'ChatGPT upstream routing changed'

if grep -Fq "llmLogRevision = \"${old_revision}\";" "$module"; then
  fail 'llm-log pin still targets the pre-expert consumer revision'
fi

grep -Eq 'expert[[:space:]]*=[[:space:]]*\{' "$module" \
  || fail 'services.llm-log.expert consumer configuration is absent'
grep -Eq 'enable[[:space:]]*=[[:space:]]*true;' "$module" \
  || fail 'expert consumer is not enabled'
grep -Eq 'require[[:space:]]*=[[:space:]]*false;' "$module" \
  || fail 'initial expert rollout must remain fail-open'

if grep -Eq 'expert[^\n]*dataDir[^\n]*Documents/AI/proxy|Documents/AI/proxy[^\n]*expert' "$module"; then
  fail 'expert mutable state must not share the raw capture corpus'
fi

printf 'llm-log expert consumer contract satisfied\n'
