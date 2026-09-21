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
  keyringExpertKb = pkgs.runCommand "zara-keyring-expert-kb" { } ''
    install -Dm0644 ${../../../.zara/experts/keyring/kb/expert.pl} \
      $out/share/zara/keyring/expert.pl
  '';
  pairPython = pkgs.python3.withPackages (p: [ p.pyzmq ]);
  zaraPair = pkgs.writeShellApplication {
    name = "zara-pair";
    runtimeInputs = [
      pairPython
      pkgs.swi-prolog
      pkgs.libsecret
      pkgs.systemd
    ];
    text = ''
      export ZARA_KEYRING_EXPERT_KB="''${ZARA_KEYRING_EXPERT_KB:-${keyringExpertKb}/share/zara/keyring/expert.pl}"
      exec ${pairPython}/bin/python3 ${../files/zarathushtra/bin/zara-pair} "$@"
    '';
  };
  adbPair = pkgs.writeShellApplication {
    name = "zara-adb-pair";
    runtimeInputs = [
      pairPython
      pkgs.android-tools
      pkgs.iproute2
    ];
    text = ''
      exec ${pairPython}/bin/python3 ${../files/zarathushtra/bin/zara-adb-pair} "$@"
    '';
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
      source = ../../../.config/zarathushtra/config.pl;
      force = true;
    };

    # localhost pairing autosync: materialize daemon-client CURVE credentials
    # from the platform keyring into secrets-daemon-clients.env after the
    # daemon's live security admin is up. Keyring backend selection is owned
    # by the dotfiles keyring expert; this service fails closed without one.
    # PartOf keeps the env file re-synced whenever the daemon restarts and its
    # advertised endpoint moves.
    systemd.user.services.zara-pair = {
      Unit = {
        Description = "Zara localhost pairing autosync";
        After = [ "zara-server.service" ];
        Wants = lib.optional config.zara.server.enable "zara-server.service";
        PartOf = lib.optionals config.zara.server.enable [ "zara-server.service" ];
      };
      Service = {
        Type = "oneshot";
        ExecStart = "${zaraPair}/bin/zara-pair";
        RemainAfterExit = true;
      };
      Install.WantedBy = [ "graphical-session.target" ];
    };

    home.packages = [ systemUpdate zaraPair adbPair ];
  };
}
