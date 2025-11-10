{ lib, pkgs, config, ... }: {
  options = with lib; {
    security = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Enable Some security";
      };
    };
  };
  config = lib.mkIf config.security.enable  {
    home.packages = with pkgs; [
    opensnitch-ui
    lxqt.lxqt-policykit
    tor-browser
    gnome-keyring
    ];
  };

}
