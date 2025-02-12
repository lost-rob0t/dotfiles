{ config, lib, pkgs, ... }:

{
  fonts = {
    packages = with pkgs; [
      nerd-fonts.hack
    ];
  };
}
