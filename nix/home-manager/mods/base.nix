{ lib, pkgs, config, ... }:

{
  options = {
    base.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable the base module which provides coreutils";
    };
  };

  config = lib.mkIf config.base.enable {
    home.packages = with pkgs; [
      nixpkgs-fmt
      git
      stow # until i have nix handles it
      grc
      htop
      jq
      fzf
      rar
      zip
      coreutils-full
      progress
      starship
      curl

    ];
  };
}
