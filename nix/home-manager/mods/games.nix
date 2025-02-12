{ lib, pkgs, inputs, config, ... }: {
  options = with lib; {
    gaming =
      {
        enable = mkOption {
          type = types.bool;
          description = "Adds some gaming packages.";
          default = false;
      };
      };
  };
  config = with lib; {
    home.packages = [
      lutris
      steam-run-native
      winePackages.stagingFull
      wineWowPackages.staging
      winetricks
      faudio
      qjoypad
      sunshine
      inputs.mousetrap.defaultPackage.x86_64-linux
    ];
  };

}
