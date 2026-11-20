{ ... }:
{
  imports = [ ../desktop/home.nix ];

  node-red.enable = true;

  # direnv + nix-direnv, declaratively. Replaces the hand-rolled
  # ~/.config/direnv/direnvrc that sourced a nix-direnv path that was
  # never installed (every direnv load printed a warning since ~Sep).
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  starintelBackup.enable = true;

  gaming.enable = true;

  homeManagerUpdater = {
    enable = true;
    hostName = "flake";
  };

  zara.server = {
    remoteEndpoint = "tcp://0.0.0.0:6060";
    securityDir = "/home/unseen/.local/state/zarathushtra/security";
  };

  nixGl.enable = true;
}
