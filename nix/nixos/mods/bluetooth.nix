{ lib, pkgs, config, ... }: {
  options = with lib; {
    desktop = {
      bluetooth = {
        settings = mkOption {
          type = types.attrs;
          default = {};
          description = "Extra Config to pass to /etc/bluetooth/main.conf";
        };
        network = mkOption {
          type = types.attrs;
          default = {};
          description = "Extra Config to pass to /etc/bluetooth/network.conf";
        };
        package = mkOption {
          type = types.package;
          default = pkgs.bluez;
          description = "Bluetooth software stack for bluetooth.";
        };
      };
    };
  };

  config = with lib; mkIf config.desktop.bluetooth.enable {
    hardware.bluetooth = {
      enable = true;
      powerOnBoot = mkDefault true;
      package = config.desktop.bluetooth.package;
      settings = config.desktop.bluetooth.settings;
      network = config.desktop.bluetooth.network;
    };

    services.blueman.enable = true;

    environment.systemPackages = with pkgs; [
      blueman
      bluez-tools
    ];
  };
}