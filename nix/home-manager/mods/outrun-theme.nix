{ lib, ... }:

let
  inherit (lib) mkEnableOption mkOption types;
  color =
    description: default:
    mkOption {
      type = types.strMatching "^#[0-9a-fA-F]{6}$";
      inherit default description;
    };
in
{
  options.outrunTheme = {
    enable = mkEnableOption "the shared Doom/Qtile Outrun theme";

    name = mkOption {
      type = types.str;
      default = "outrun";
      description = "Theme identifier used by supported applications.";
    };

    fonts = {
      ui = mkOption {
        type = types.str;
        default = "Noto Sans";
        description = "UI font used by graphical Outrun themes.";
      };
      code = mkOption {
        type = types.str;
        default = "Hack Nerd Font";
        description = "Code font used by graphical Outrun themes.";
      };
    };

    palette = {
      deepBackground = color "Deep purple-black background." "#170c32";
      background = color "Raised purple background." "#202146";
      muted = color "Muted mauve text and border color." "#92406e";
      orange = color "Outrun orange." "#fba922";
      cyan = color "Neon cyan." "#2de2e6";
      foreground = color "Primary foreground." "#f3f4f5";
      pink = color "Neon pink primary accent." "#f6019d";
      green = color "Success green." "#62ff00";
      red = color "Error and removal red." "#dd546e";
      purple = color "Deep neon purple." "#9700cc";
      electricBlue = color "Electric blue informational accent." "#00b8ff";
      violet = color "Violet secondary accent." "#6c5ce7";
      yellow = color "Warm yellow." "#ffe66d";
      ice = color "Ice-blue highlight." "#7df9ff";
      hotOrange = color "Hot orange warning accent." "#ff6b35";
    };
  };
}
