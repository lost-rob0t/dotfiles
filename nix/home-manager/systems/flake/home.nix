{ ... }:
{
  imports = [ ../desktop/home.nix ];

  homeManagerUpdater = {
    enable = true;
    hostName = "flake";
  };

  nixGl.enable = true;
}
