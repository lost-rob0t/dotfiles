{ config, lib, pkgs, ... }:
let
  nyxt = pkgs.nyxt.overrideAttrs (oldAttrs: {
    postFixup = ''
      wrapProgram $out/bin/nyxt \
        --set-default WEBKIT_FORCE_SANDBOX 0
    '';
  });
  name = "N545PY";
  email = "nsaspy@airmail.cc";
  doom-emacs = pkgs.callPackage (builtins.fetchTarball {
    url = https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz;
  }) {
      doomPrivateDir = ./doom.d;  # Directory containing your config.el init.el
      # and packages.el files
  };

in
{
  home.file.".emacs.d/init.el".text = ''
      (load "default.el")
      '';
  home.packages = [
    pkgs.vlc
    pkgs.obs-studio
    pkgs.libreoffice
    pkgs.filezilla
    pkgs.nerdfonts
    pkgs.w3m
    pkgs.cht-sh
    pkgs.kleopatra
    pkgs.gimp
    pkgs.feh
    pkgs.bookworm
    pkgs.tlp
    nyxt
    pkgs.webkitgtk
    pkgs.remmina
    pkgs.freerdp
    pkgs.sqlitebrowser
    pkgs.brave
    pkgs.ripgrep
    ((pkgs.emacsPackagesFor doom-emacs).emacsWithPackages (epkgs: [
      epkgs.vterm
      epkgs.ac-ispell
      epkgs.direnv
      epkgs.lsp-pyright
      epkgs.pylint
      epkgs.w3m
      epkgs.pandoc
      epkgs.xclip
      pkgs.ripgrep
      pkgs.fd
    ]))


  ];
  programs = {
      git = {
        enable = true;
        userName = "${name}";
        userEmail = "${email}";
      };
  emacs = {
    enable = true;
    extraPackages = epkgs: [ epkgs.vterm ];
};
  };


}
