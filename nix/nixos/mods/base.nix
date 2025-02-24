{ lib, pkgs, config, ... }: {
  options = with lib; {
    programs = {
      baseUtils.enable = mkEnableOption "Enable the base common utils";
    };};
  config = lib.mkIf config.programs.baseUtils.enable {

    environment.systemPackages = with pkgs; [
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
      fd # Why does Doom look in /run/current-ssytem ? its even in the path god dammit
    ];

  };

}
