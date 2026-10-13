{ ... }:
{
  imports = [ ../desktop/home.nix ];

  node-red.enable = true;

  homeManagerUpdater = {
    enable = true;
    hostName = "flake";
  };

  # Zara is temporarily disabled by the shared desktop profile. Keep the
  # host-specific listener dormant until the runtime is deliberately restored.
  # zara.server = {
  #   remoteEndpoint = "tcp://0.0.0.0:6060";
  #   securityDir = "/home/unseen/.local/state/zarathushtra/security";
  # };

  nixGl.enable = true;
}
