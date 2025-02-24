{ lib, pkgs, config, ... }: {
  options = with lib; {
    desktop = {
      enable = mkEnableOption "Enable Common desktop setup";
      sessionType = mkOption {
        type = types.enum [ "x11" "wayland" ];
        default = "x11";
        description = "Session type to use (x11 or wayland)";
      };
      bluetooth = {
        enable = mkEnableOption "Enable bluetooth support";
      };
      fonts = { };
      wm = {
        qtile = {
          enable = mkEnableOption "Enable Qtile window manager";
        };
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
    (mkIf config.desktop.enable {
      programs.gnupg.agent = {
        enable = true;
        enableSSHSupport = true;
      };
      
      desktop.fonts.enable = mkDefault true;
      desktop.bluetooth.enable = mkDefault true;
      desktop.wm.qtile.enable = mkDefault true;
      programs.baseUtils.enable = true;

      environment.systemPackages = with pkgs; [
        (aspellWithDicts (ds: with ds; [
          en
          en-computers
          en-science
        ]))
      ] ++ (optionals config.desktop.qtile.enable config.desktop.qtile.extraPackages);
    })

    (mkIf (config.desktop.enable && config.desktop.qtile.enable) {
      system.nixos.tags = [ "desktop-qtile-${config.desktop.sessionType}" ];
      
      services.xserver = {
        enable = true;
        windowManager.session = lib.singleton {
          name = "qtile";
          start = ''
            ${config.desktop.qtile.package}/bin/qtile start -b ${config.desktop.sessionType} \
            --config /home/unseen/.config/qtile/config.py &
            waitPID=$!
          '';
        };
      };
    })
  ];
}