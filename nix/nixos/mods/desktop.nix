{ lib, pkgs, config, ... }: {
  options = with lib; {
    desktopEnv = {
      enable = mkEnableOption "Enable Common desktop setup";
      sessionType = mkOption {
        type = types.enum [ "x11" "wayland" ];
        default = "x11";
        description = "Session type to use (x11 or wayland)";
      };
      qtile = {
        enable = mkEnableOption "Enable Qtile window manager";
        package = mkOption {
          type = types.package;
          default = pkgs.python3Packages.qtile;
          description = "Qtile package to use";
        };
        extraPackages = mkOption {
          type = types.listOf types.package;
          default = with pkgs; [
            sxhkd
            j4-dmenu-desktop
            dmenu
            brave
            firefox
            emacs
            conky
            emojione
          ];
          description = "Additional packages to install for Qtile environment";
        };
      };
    };
  };

  config = with lib; mkMerge [
    (mkIf config.desktopEnv.enable {
      programs.gnupg.agent = {
        enable = true;
        enableSSHSupport = true;
      };
      
      environment.systemPackages = with pkgs; [
        (aspellWithDicts (ds: with ds; [
          en
          en-computers
          en-science
        ]))
      ] ++ (optionals config.desktopEnv.qtile.enable config.desktopEnv.qtile.extraPackages);
    })

    (mkIf (config.desktopEnv.enable && config.desktopEnv.qtile.enable) {
      system.nixos.tags = [ "desktop-qtile-${config.desktopEnv.sessionType}" ];
      
      services.xserver = {
        enable = true;
        windowManager.session = lib.singleton {
          name = "qtile";
          start = ''
            ${config.desktopEnv.qtile.package}/bin/qtile start -b ${config.desktopEnv.sessionType} \
            --config /home/unseen/.config/qtile/config.py &
            waitPID=$!
          '';
        };
      };
    })
  ];
}