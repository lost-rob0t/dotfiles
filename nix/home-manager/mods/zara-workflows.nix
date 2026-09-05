{ lib, pkgs, config, ... }:

let
  cfg = config.zara.workflows;
  systemUpdate = pkgs.writeShellApplication {
    name = "zara-system-update";
    runtimeInputs = [
      pkgs.git
      pkgs.libnotify
      pkgs.polkit
    ];
    text = builtins.readFile ../files/zarathushtra/bin/zara-system-update;
  };
in
{
  options.zara.workflows = {
    enable = lib.mkEnableOption "operator Zara desktop/voice workflow configuration";
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = config.zara.enable;
        message = "zara.workflows.enable requires zara.enable.";
      }
    ];

    # Home Manager owns only the non-secret provisioned/base layer. Zara's
    # config.local.pl remains mutable, private, and deliberately unmanaged.
    home.file.".config/zarathushtra/config.pl" = {
      source = ../files/zarathushtra/config.pl;
      force = true;
    };

    home.packages = [ systemUpdate ];
  };
}
