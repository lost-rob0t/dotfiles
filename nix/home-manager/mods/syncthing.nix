{ lib, pkgs, config, ... }: {
  options = with lib; {
    desktop = {
      sync = {
        enable = mkEnableOption "Enable Syncthing";
        defaultPath = mkOption {
          type = types.string;
          default = "${config.home.homeDirectory}/sync";
          description = "Directory where new folders will be added.";
        };
      };
    };
  };

  config = with lib; mkIf config.desktop.sync.enable {
    services.syncthing = {
      enable = true;
      user = config.home.username;
      dataDir = config.desktop.sync.defaultPath; # Default folder for new synced folders
      #configDir = "/home/unseen/Documents/.config/syncthing"; # Folder for Syncthing's settings and keys
    };

  };

}
