{ ... }:
{
  imports = [ ../desktop/home.nix ];

  home.sessionVariables.CHEMACS_PROFILE = "desktop-native";

  homeManagerUpdater = {
    enable = true;
    hostName = "flake";
  };

  nixGl.enable = true;
}
