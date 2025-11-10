{ config, lib, pkgs, ... }:

{
  fonts = {
    packages = with pkgs; [
      nerd-fonts.hack
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-color-emoji
      liberation_ttf
      fira-code
      fira-code-symbols
      mplus-outline-fonts.githubRelease
      dina-font
      proggyfonts
    ];
  };
    environment.systemPackages = with pkgs; [
      fontfinder
    ];
}
