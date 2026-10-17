{ lib, ... }:
{
  imports = [ ../desktop/home.nix ];

  node-red.enable = true;

  homeManagerUpdater = {
    enable = true;
    hostName = "flake";
  };

  # Restore the local voice assistant on this host only. The shared desktop
  # profile and hunter02 remain disabled while their runtime is unverified.
  zara = {
    enable = lib.mkForce true;
    workflows.enable = lib.mkForce true;
    server.enable = lib.mkForce true;
    desktop.enable = lib.mkForce true;
    wake.enable = lib.mkForce true;
  };

  # The configured Zara voice is a local Qwen clone. Keep synthesis off the
  # GPU while the existing driver incident remains unresolved.
  services.qwen3-tts = {
    enable = true;
    autoStart = true;
    backend = "cpu";
    port = 7860;
  };

  nixGl.enable = true;
}
