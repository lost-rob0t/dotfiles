{ ... }:
{
  imports = [ ../desktop/home.nix ];

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
