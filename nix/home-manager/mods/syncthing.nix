{ lib, pkgs, config, ... }: {
  options = with lib; {
    desktop = {
      sync = {
        enable = mkEnableOption "Enable Syncthing";
        # dataDir = mkOption {
        #   type = types.string;
        #   default = "${config.home.homeDirectory}/sync";
        #   description = "Directory where new folders will be added.";
        # };
      };
    };
  };

  config = with lib; mkIf config.desktop.sync.enable {
    services.syncthing = {

      enable = true;
      #user = config.home.username;
      # Not needed in home-manager? dataDir = config.desktop.sync.dataDir; # Default folder for new synced folders
    };

  };

}
