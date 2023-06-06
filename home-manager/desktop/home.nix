{ inputs, outputs, lib, config, pkgs, ... }:
{

  imports = [
    ../global/global.nix
    ./programs.nix
    ./services.nix
  ];

  nixpkgs = {

    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = (_: true);
    };
#      overlays = [ (final: prev: {
#          lispPackages = prev.lispPackages // {
#
#            nyxt = prev.lispPackages.nyxt.overrideAttrs (oldAttrs:
#          {
#            version = "3.0.0";
#            src = prev.fetchFromGitHub {
#              owner = "atlas-engineer";
#              repo = "nyxt";
#              rev = "e12cc3a8141a0e11290b527035a356cba2e05219";
#              sha256 = "sha256-GdTOFu5yIIL9776kfbo+KS1gHH1xNCfZSWF5yHUB9U8=";
#            };
#          });
#        };
#        })];

  };
  home = {
  username = "unseen";
  homeDirectory = "/home/unseen";
  stateVersion = "22.05";
  };
  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

}
