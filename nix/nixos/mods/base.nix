{ lib, pkgs, config, ... }: {
  options = with lib; {
    programs = {
      baseUtils.enable = mkEnableOption "Enable the base common utils";
    };};
  config = lib.mkIf config.programs.baseUtils.enable {

    enviroment.systemPackages = with pkgs; [
      wget
      gnupg
      curl
      coreutils-full
      bash
      zip
      unzip
      p7zip
      rar
      pmutils
      git
      ripgrep
      htop
    ];

  };

}
