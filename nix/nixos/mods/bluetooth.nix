{ lib, pkgs, config, ... }: {
  options = with lib; {
    desktopEnv = {
      bluetooth = {
        enable = mkEnableOption "Enable bluetooth and start blueman services ensuring bluze";
        settings = mkOption {
          type = types.attr;
          default = {};
          description = "Extra Config to pass to /etc/bluetooth/main.conf";
        };
        network = mkOption {
          type = types.attr;
          default = {};
          description = "Extra Config to pass to /etc/bluetooth/network.conf";
        };
        pkg = mkOption {
          type = types.package;
          default = pkgs.bluez;
          description = "Bluetooth software stack for bluetooth.";
        };
      };
    };

  };
  config = with lib; mkIf config.desktopEnv.bluetooth.enable {

    services = {
      blueman.enable = true;
    };
    hardware.bluetooth = {
      pkg = config.desktopEnv.bluetooth.pkg;
      network = config.desktopEnv.bluetooth.network;
      settings = config.desktopEnv.bluetooth.settings;
    };
  enviroment.systemPackages =  [
      blueman
      bluez-tools ];
  };

}
