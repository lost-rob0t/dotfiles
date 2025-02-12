{ lib, pkgs, config, ... }: {
  imports = [
    ./python-shell.nix
  ];
  options = with lib; {
    desktop = {
    dev = {
      python.enable = mkOption  {
        type = lib.types.bool;
        default = true;
        description = "Configure system for python development.";
      };
      python.pkg = mkOption {
        type = lib.types.package;
        default = pkgs.python313;
        description = "Version of python to use.";
      };
    };
  };
  };
  config = with lib; mkIf config.dev.nim.enable {
    home.packages  = with pkgs; [
      config.dev.python.pkg
      pythonFHS
    ];
  };

}
