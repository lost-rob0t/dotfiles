{config, lib, pkgs, ...}:

with lib;

let
  cfg = config.services.ai;
in {
  options.services.ai = {
    enable = mkEnableOption "AI services (Ollama and Open WebUI)";

    ollama = {
      enable = mkOption {
        type = types.bool;
        default = config.services.ai.enable;
        description = "Whether to enable Ollama service";
      };

      port = mkOption {
        type = types.port;
        default = 11434;
        description = "Port for Ollama to listen on";
      };

      homeDir = mkOption {
        type = types.str;
        default = "/home/ollama";
        description = "Home directory for Ollama user and data";
      };
    };

    openWebui = {
      enable = mkOption {
        type = types.bool;
        default = config.services.ai.enable;
        description = "Whether to enable Open WebUI";
      };

      port = mkOption {
        type = types.port;
        default = 3000;
        description = "Port for Open WebUI to listen on";
      };
    };
  };

  config = mkIf cfg.enable {
    users.users.ollama = {
      isSystemUser = true;
      group = "ollama";
      home = cfg.ollama.homeDir;
      createHome = true;
      description = "Ollama service user";
    };

    users.groups.ollama = {};

    services.ollama = mkIf cfg.ollama.enable {
      enable = true;
      port = cfg.ollama.port;
      user = "ollama";
      group = "ollama";
      home = cfg.ollama.homeDir;
    };

    systemd.services.open-webui = mkIf cfg.openWebui.enable {
      description = "Open WebUI for Ollama";
      wantedBy = ["multi-user.target"];
      after = ["network.target" "ollama.service"];
      requires = ["ollama.service"];

      serviceConfig = {
        ExecStart = "${pkgs.open-webui}/bin/open-webui";
        Environment = [
          "OLLAMA_API_BASE_URL=http://localhost:${toString cfg.ollama.port}"
          "PORT=${toString cfg.openWebui.port}"
        ];
        Restart = "always";
        User = "ollama";
        Group = "ollama";
        WorkingDirectory = cfg.ollama.homeDir;
      };
    };

    environment.systemPackages = with pkgs; [
      ollama
      open-webui
    ];

    # Open firewall ports if needed
    networking.firewall = {
      allowedTCPPorts = [
        cfg.ollama.port
        cfg.openWebui.port
      ];
    };
  };
}
