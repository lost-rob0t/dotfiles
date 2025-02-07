{ config, lib, pkgs, ... }:
let
  nyxt = pkgs.nyxt.overrideAttrs (oldAttrs: {
    postFixup = ''
      wrapProgram $out/bin/nyxt \
        --set-default WEBKIT_FORCE_SANDBOX 0
    '';
  });
in
{
  home.packages = with pkgs; [
    # TODO sort into categories
    vlc
    obs-studio
    #libreoffice
    cht-sh
    vim
    ntfy # send notifications
    # Security
    # rice
    starship
  ];
  programs = {
      gpg = {
          enable = true;
      };
 emacs = {
  enable = true;
  extraPackages = epkgs: [
      epkgs.vterm
      epkgs.direnv
      epkgs.lsp-pyright
      epkgs.pylint
      epkgs.w3m
      epkgs.pandoc
      pkgs.nodePackages.bash-language-server
      pkgs.roswell
      epkgs.xclip
      pkgs.aspell
      pkgs.aspellDicts.en
      pkgs.libnotify # for alert.el
      pkgs.xdotool # for emacs everywhere
      pkgs.ffmpegthumbnailer # Video thumbnails
      pkgs.imagemagick #photo thumbnails
      pkgs.mediainfo #audio previews
      pkgs.mpv # for bongo
      pkgs.pyright
      pkgs.python310
      pkgs.pylint
      pkgs.python310Packages.flake8

                         ];
};
  };
}
