{ lib, pkgs, config, ... }: {
  options = with lib; {
    services = {
      ai.enable = mkEnableOption "Enable Local AI services.";
      ai = {

      };
    };
  };
  config = with lib; {};

}
