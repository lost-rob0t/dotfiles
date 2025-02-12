{ lib, pkgs, config, ... }: {
  options = with lib; {
    dev = {
      nim.enable = mkOption  {
        type = types.bool;
        default = true;
        description = "Configure system for nim development.";
      };
      nim.sslPkg = mkOption {
        type = types.package;
        default = pkgs.openssl.out;
        description = "Package that implements TLS for using with nim";
      };

  };
  };
  config = with lib; mkIf config.dev.nim.enable {
    home.packages  = with pkgs; [
        nimble
        nim-unwrapped-2
        nimlsp

    ];
  };

}
