{
  lib,
  pkgs,
  inputs,
  config,
  ...
}:
{
  options = with lib; {
    gaming = {
      enable = mkOption {
        type = types.bool;
        description = "Adds gaming packages with the Steam client and Moonlight/Sunshine game streaming.";
        default = false;
      };

      steam.enable = mkOption {
        type = types.bool;
        description = "Installs the Steam client (unfree). Home Manager master removed programs.steam, so this uses the nixpkgs FHS package. Terraria and War Thunder are Steam-distributed titles.";
        default = true;
      };

      moonlight.enable = mkOption {
        type = types.bool;
        description = "Installs the Moonlight Qt game streaming client.";
        default = true;
      };
    };
  };

  config =
    with lib;
    mkMerge [
      (mkIf config.gaming.enable {
        home.packages =
          with pkgs;
          [
            lutris
            steam-run
            wineWow64Packages.staging
            winetricks
            faudio
            qjoypad
            sunshine
          ]
          ++ [ inputs.mousetrap.packages.x86_64-linux.default ];
      })

      (mkIf (config.gaming.enable && config.gaming.moonlight.enable) {
        home.packages = with pkgs; [ moonlight-qt ];
      })

      (mkIf (config.gaming.enable && config.gaming.steam.enable) {
        home.packages = with pkgs; [
          steam
          protontricks
        ];
      })
    ];
}
