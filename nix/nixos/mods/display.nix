{ lib, pkgs, config, ... }: {
  options = with lib; {
    desktopEnv = {
      sessionType = mkOption {
        type = types.string;
        default = "x11";
        description = "Which display backend to use, one of x11 or wayland.";
      };
    };
  };
  config = with lib; {};

}
