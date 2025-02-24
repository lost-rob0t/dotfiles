{ lib, pkgs, config, ... }: {
  options = with lib; {
    imports = [ ./display.nix ];
    desktop.wm = {

      qtile = {
        pkg = mkOption {
          type = types.package;
          default = pkgs.python313Packages.qtile;
          description = "What version of qtile to use.";
        };
        enable = mkEnableOption "Enable Qtile Tiling WM";
        extraPkgs = mkOption {
          type = types.listOf types.package;
          default = [];
          description = "List of extra pkgs to be added to this qtile config.";
        };
      };
    };
  };
  config = with lib; mkIf (config.desktop.wm.enable && config.desktop.sessionType == "x11") {
  system.nixos.tags = ["desktop-qtile-x11"];
  nixpkgs.overlays = [ (self: super: {
      qtile = super.qtile.unwrapped.override (old: {
        propagatedBuildInputs = (old.propagatedBuildInputs or [ ]) ++ (config.desktop.fonts) ++ (config.desktop.qtile.extraPkgs or [ ]) ++ (with self.python3Packages; [
          requests
          pkgs.sxhkd
          pkgs.j4-dmenu-desktop
          pkgs.dmenu
          pkgs.brave
          pkgs.firefox
          pkgs.emacs
          pkgs.conky
          pkgs.emojione # wttr widget emojis
        ]);
      });
    })];
  services = {
    xserver = {
      enable = mkDefault true;
      windowManager = {
        session = [{
          name = "qtile";
          start = ''
            ${config.desktopEnv.qtile.pkg}/bin/qtile start -b ${(config.desktop)} \
            --config  /home/unseen/.config/qtile/config.py &
            waitPID=$!
          '';
        }];
      };
    };
  };
  };

}
