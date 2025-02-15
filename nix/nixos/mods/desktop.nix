{ lib, pkgs, config, ... }: {
  imports = [ ./qtile.nix ];
  options = with lib; {
    desktopEnv = {
      enable = mkEnableOption "Enable Common desktop setup";
    };
  };
  config = with lib; {
    programs.gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
   programs.baseUtils.enable = true;
   system.enviroment.packages = [
    (aspellWithDicts
        (dicts: with dicts; [ en en-computers en-science  ]))


   ];
  };

}
