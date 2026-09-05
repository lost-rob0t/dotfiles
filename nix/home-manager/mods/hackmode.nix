{ config, lib, pkgs, ... }:

let
  cfg = config.hackmode;
  hackmodeInit = ../../../emacs/hackmode/init.el;
  hackmacsDir = "${config.xdg.dataHome}/hackmacs";
  hackmodeStateDir = "${config.xdg.dataHome}/hackmode";
  hackmodeRoot = "${config.home.homeDirectory}/Documents/hackmode";

  hackmacs = pkgs.writeShellApplication {
    name = "hackmacs";
    runtimeInputs = [
      config.emacs.package
      pkgs.coreutils
    ];
    text = ''
      state_dir=${lib.escapeShellArg hackmacsDir}
      mkdir -p "$state_dir"

      exec ${config.emacs.package}/bin/emacs -Q \
        --eval "(setq user-emacs-directory (file-name-as-directory \"$state_dir\"))" \
        --eval '(setq server-name "hack")' \
        --load "$state_dir/init.el" \
        "$@"
    '';
  };
in
{
  options.hackmode.enable = lib.mkEnableOption "Hackmode workstation integration";

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = config.emacs.enable;
        message = "hackmode requires emacs.enable = true";
      }
    ];

    home.packages = with pkgs; [
      hackmacs
      bind
      nmap
      socat
      whois
    ];

    home.sessionVariables = {
      HACKMODE_ROOT = hackmodeRoot;
      HACKMODE_STATE_DIR = hackmodeStateDir;
    };

    # Chemacs can select the Hackmode profile without making the dotfiles tree
    # itself Emacs' writable user directory. Package caches and straight.el
    # checkouts therefore stay under XDG data rather than dirtying the repo.
    home.file.".emacs-profiles.el" = {
      source = ../../../.emacs-profiles.el;
      force = true;
    };

    xdg.dataFile."hackmacs/init.el".text = ''
      ;; Managed by Home Manager. The literate Hackmode source remains canonical.
      (load "${hackmodeInit}" nil 'nomessage)
    '';

    # Existing shell helpers read these files at startup. Seed them so a fresh
    # Home Manager activation does not emit errors before the first operation is
    # selected.
    home.activation.hackmodeState = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      state_dir=${lib.escapeShellArg hackmodeStateDir}
      root=${lib.escapeShellArg hackmodeRoot}

      $DRY_RUN_CMD ${pkgs.coreutils}/bin/mkdir -p "$state_dir" "$root"
      $DRY_RUN_CMD ${pkgs.coreutils}/bin/touch \
        "$state_dir/current-op" \
        "$state_dir/op-path"
    '';
  };
}
