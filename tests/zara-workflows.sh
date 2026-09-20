#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
config="$repo_root/.config/zarathushtra/config.pl"
workflow_module="$repo_root/nix/home-manager/mods/zara-workflows.nix"
default_module="$repo_root/nix/home-manager/mods/default.nix"
desktop="$repo_root/nix/home-manager/systems/desktop/home.nix"
nixos_networking="$repo_root/nix/nixos/systems/flake/networking.nix"
updater="$repo_root/nix/home-manager/files/zarathushtra/bin/zara-system-update"
emacs_plugin_config="$repo_root/nix/home-manager/files/zarathushtra/plugins/zara-emacs/config.toml"
doom_config_org="$repo_root/.doom.d/config.org"
doom_config_el="$repo_root/.doom.d/config.el"
doom_packages_org="$repo_root/.doom.d/packages.org"
doom_packages_el="$repo_root/.doom.d/packages.el"

fail() {
  printf 'zara-workflows: %s\n' "$*" >&2
  exit 1
}

[[ -f "$config" ]] || fail "missing Home Manager-owned Zara base config"
[[ -f "$workflow_module" ]] || fail "missing Zara workflow Home Manager module"
[[ -f "$updater" ]] || fail "missing structured Zara system-update helper"
[[ -f "$emacs_plugin_config" ]] || fail "missing zara-emacs plugin configuration"

grep -Fq './zara-workflows.nix' "$default_module" || fail "workflow module is not imported"
grep -Fq 'inputs.qwen3-tts.homeManagerModules.default' "$default_module" || fail "Qwen3-TTS Home Manager module is not imported"
grep -Fq 'workflows.enable = true;' "$desktop" || fail "desktop profile does not enable Zara workflows"
grep -Fq 'services.qwen3-tts = {' "$desktop" || fail "desktop profile does not configure Qwen3-TTS"
grep -Fq 'enable = true;' "$desktop" || fail "desktop profile must enable the validated Qwen3-TTS Vulkan service"
grep -Fq 'autoStart = true;' "$desktop" || fail "desktop profile must start Qwen3-TTS at login"
grep -Fq 'backend = "vulkan";' "$desktop" || fail "desktop profile must use the Vulkan GPU backend"
! grep -Fq 'rocm' "$desktop" || fail "desktop profile must not reference the retired ROCm backend"
! grep -Fq '"qwen3-tts.service"' "$desktop" || fail "Zara must not pull Qwen3-TTS into the login transaction"
grep -Fq 'nixManaged = false;' "$desktop" || fail "desktop profile must keep mutable Zara config outside Home Manager"
grep -Fq '"zara-emacs"' "$desktop" || fail "desktop profile does not install zara-emacs"
grep -Fq '"zara-emacs/config.toml"' "$desktop" || fail "desktop profile does not install zara-emacs config"
grep -Fq 'server_name = "doom"' "$emacs_plugin_config" || fail "zara-emacs is not bound to the Doom server"
grep -Fq 'notes_root = "~/Documents/Notes/org"' "$emacs_plugin_config" || fail "zara-emacs notes root drifted"
grep -Fq '".config/zarathushtra/config.pl"' "$workflow_module" || fail "workflow module does not own base config.pl"
! grep -Fq '".config/zarathushtra/config.local.pl"' "$workflow_module" || fail "Home Manager must not own mutable config.local.pl"

grep -Fq 'github:lost-rob0t/Qwen3-TTS_server' "$repo_root/flake.nix" || fail "Qwen3-TTS flake input is missing"
grep -Fq '"qwen3-tts": "qwen3-tts"' "$repo_root/flake.lock" || fail "Qwen3-TTS flake input is not locked"

native_zara_sha='26a93071840ed8e32cb24e5e0874011ce82f2fa9'
for file in "$doom_packages_org" "$doom_packages_el"; do
  grep -Fq "$native_zara_sha" "$file" || fail "Doom Zara package is not pinned to the reviewed native client"
done
for file in "$doom_config_org" "$doom_config_el"; do
  grep -Fq '(zara-native-mode 1)' "$file" || fail "Doom does not enable zara-native-mode"
  grep -Fq "(zara-gptel-register-tools)" "$file" || fail "Doom does not register Zara gptel tools"
  grep -Fq ':prefix ("y z" . "Zara")' "$file" || fail "Doom Zara leader map is missing"
done

! grep -Fq -- '--endpoint tcp://' "$desktop" || fail "local Zara must use its owner-private IPC default"
! grep -Fq -- '--security-dir' "$desktop" || fail "local Zara must not require CURVE setup"
! grep -Fq -- '--security-init' "$desktop" || fail "local Zara must not require security enrollment"
! grep -Fq '7731 # Zara authenticated ZARA/1 listener' "$nixos_networking" || fail "Arch Zara listener must not be modeled as NixOS firewall state"

grep -Fq 'search_engine("https://search.brave.com/search?q=~w").' "$config" || fail "Brave Search is not configured"
grep -Fq 'app_mapping(browser, ["brave"]).' "$config" || fail "normal Brave browser workflow missing"
grep -Fq 'app_mapping(scratch,' "$config" || fail "Emacs scratch workflow missing"
grep -Fq 'org-roam-dailies-goto-today' "$config" || fail "org-roam daily workflow missing"
grep -Fq 'zara-dictate' "$config" || fail "org-roam daily does not enter Zara dictation"
grep -Fq 'app_mapping(thunderbird, ["thunderbird"]).' "$config" || fail "Thunderbird workflow missing"
grep -Fq 'app_mapping(tor, ["torbrowser-launcher"]).' "$config" || fail "Tor Browser workflow missing"
grep -Fq 'app_mapping(feishin, ["feishin"]).' "$config" || fail "Feishin workflow missing"
grep -Fq 'https://music.youtube.com/' "$config" || fail "YouTube Music workflow missing"

for alias in "app_mapping('4chan'," "app_mapping('4'," 'app_mapping(fourchan,' 'app_mapping(four,' 'app_mapping(chan,' 'app_mapping(pol,' 'app_mapping(politically,' 'app_mapping(politically_incorrect,'; do
  grep -Fq "$alias" "$config" || fail "missing 4chan /pol/ alias: $alias"
done
[[ "$(grep -Fc 'https://boards.4chan.org/pol/' "$config")" -ge 8 ]] || fail "4chan variants do not all target /pol/"

grep -Fq 'verb_intent(latest, open, 1).' "$config" || fail "latest-PR compatibility phrase missing"
grep -Fq 'app_mapping(prs,' "$config" || fail "latest-PR compatibility mapping missing"
grep -Fq 'verb_intent(magit, open, 1).' "$config" || fail "Magit project workflow missing"
grep -Fq 'app_mapping(dotfiles,' "$config" || fail "dotfiles Magit alias missing"
grep -Fq 'verb_intent(update, open, 1).' "$config" || fail "update-system phrase mapping missing"
grep -Fq 'app_mapping(system, ["zara-system-update"]).' "$config" || fail "update-system helper mapping missing"

grep -Fq 'pkexec /usr/bin/pacman -Syu' "$updater" || fail "system update does not cross polkit boundary"
grep -Fq 'git -C "$DOTFILES" pull --ff-only' "$updater" || fail "system update dotfiles pull is not fast-forward-only"
grep -Fq 'home-manager switch --flake' "$updater" || fail "system update does not apply Home Manager"
! grep -Eqi '(sudo -S|NOPASSWD|password=|echo .+\| *sudo)' "$updater" || fail "system update contains forbidden password/sudo bypass"

printf 'zara-workflows: ok\n'
