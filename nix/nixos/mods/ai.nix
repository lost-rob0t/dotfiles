{config, lib, pkgs, ...}:

with lib;

let
  cfg = config.services.ai;
in {
  options.services.ai = {
    enable = mkEnableOption "AI services and tools";

    gpuAcceleration = {
      enable = mkEnableOption "Enable GPU acceleration for AI workloads";
      
      cuda = {
        enable = mkOption {
          type = types.bool;
          default = config.services.ai.gpuAcceleration.enable;
          description = "Enable CUDA support";
        };
      };
      
      rocm = {
        enable = mkOption {
          type = types.bool;
          default = false;
          description = "Enable ROCm support for AMD GPUs";
        };
      };
    };

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

      models = mkOption {
        type = types.listOf types.str;
        default = ["llama2" "codellama" "mistral"];
        description = "List of models to pre-download";
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

    tools = {
      enable = mkOption {
        type = types.bool;
        default = config.services.ai.enable;
        description = "Whether to install AI development tools";
      };

      python = {
        enable = mkOption {
          type = types.bool;
          default = config.services.ai.tools.enable;
          description = "Install Python AI/ML tools";
        };
        
        packages = mkOption {
          type = types.listOf types.package;
          default = with pkgs.python3Packages; [
            torch
            tensorflow
            transformers
            numpy
            pandas
            scikit-learn
            jupyter
          ];
          description = "Python packages for AI development";
        };
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      users.users.ollama = {
        isSystemUser = true;
        group = "ollama";
        home = cfg.ollama.homeDir;
        createHome = true;
        description = "Ollama service user";
      };

      users.groups.ollama = {};

      # System optimizations for AI workloads
      boot.kernel.sysctl = {
        "vm.max_map_count" = 1048576;
        "vm.swappiness" = 1;
      };

      environment.systemPackages = with pkgs; [
        ollama
        open-webui
      ] ++ (optionals cfg.tools.enable [
        python3
        git-lfs
        cmake
        gcc
      ]);
    })

    (mkIf cfg.gpuAcceleration.enable {
      hardware.opengl = {
        enable = true;
        driSupport = true;
        driSupport32Bit = true;
      };
    })

    (mkIf cfg.gpuAcceleration.cuda.enable {
      hardware.nvidia = {
        package = config.boot.kernelPackages.nvidiaPackages.stable;
        modesetting.enable = true;
      };
    })

    (mkIf cfg.ollama.enable {
      services.ollama = {
        enable = true;
        port = cfg.ollama.port;
        user = "ollama";
        group = "ollama";
        home = cfg.ollama.homeDir;
      };

      systemd.services.ollama-models = {
        description = "Pre-download Ollama models";
        after = ["ollama.service"];
        wantedBy = ["multi-user.target"];
        script = with pkgs; ''
          ${lib.concatMapStrings (model: ''
            ${ollama}/bin/ollama pull ${model}
          '') cfg.ollama.models}
        '';
        serviceConfig = {
          Type = "oneshot";
          User = "ollama";
          Group = "ollama";
        };
      };
    })

    (mkIf cfg.openWebui.enable {
      systemd.services.open-webui = {
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
    })

    (mkIf cfg.tools.python.enable {
      environment.systemPackages = cfg.tools.python.packages;
    })

    (mkIf cfg.enable {
      networking.firewall = {
        allowedTCPPorts = [
          cfg.ollama.port
          cfg.openWebui.port
        ];
      };
    })
  ];
}