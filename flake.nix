{
  description = "NixOS configuration and installer for logos";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    disko = {
      url = "github:nix-community/disko/latest";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-auto-research = {
      url = "github:lost-rob0t/emacs-auto-research/8f69c306a07bbf63998755ee6d5d759913289168";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    skills.url = "github:lost-rob0t/skills";
    chatgpt-desktop = {
      url = "github:lost-rob0t/chatgpt-desktop";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    zara = {
      url = "github:lost-rob0t/zara";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bixby-studio = {
      url = "github:lost-rob0t/bixby-studio";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mousetrap = {
      url = "github:lost-rob0t/Mousetrap";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      disko,
      home-manager,
      emacs-auto-research,
      skills,
      chatgpt-desktop,
      zara,
      bixby-studio,
      mousetrap,
    }:
    let
      system = "x86_64-linux";
      lib = nixpkgs.lib;
      pkgs = nixpkgs.legacyPackages.${system};
      prologMcp = pkgs.callPackage ./nix/packages/prolog-mcp.nix { };
      braveMcp = pkgs.callPackage ./nix/packages/brave-mcp { };

      sharedArgs = {
        inherit self disko;
      };
      homeArgs = {
        inherit self;
        inputs = self.inputs;
        outputs = self.outputs;
      };

      installLogos = pkgs.writeShellApplication {
        name = "install-logos";
        runtimeInputs = [
          disko.packages.${system}.disko-install
          pkgs.coreutils
          pkgs.gnugrep
          pkgs.gnused
          pkgs.jq
          pkgs.openssl
          pkgs.util-linux
        ];
        text = ''
          export LOGOS_DEFAULT_FLAKE='${self.outPath}#logos'
          export LOGOS_SOURCE='${self.outPath}'
          export DISKO_INSTALL='${disko.packages.${system}.disko-install}/bin/disko-install'
          exec '${pkgs.bash}/bin/bash' '${./scripts/install-logos.sh}' "$@"
        '';
      };

      installLogosGui = pkgs.writeShellApplication {
        name = "install-logos-gui";
        runtimeInputs = [
          installLogos
          pkgs.coreutils
          pkgs.jq
          pkgs.lxqt.qterminal
          pkgs.util-linux
          pkgs.zenity
        ];
        text = ''
          export LOGOS_DEFAULT_FLAKE='${self.outPath}#logos'
          exec '${pkgs.bash}/bin/bash' '${./scripts/install-logos-gui.sh}' "$@"
        '';
      };

      logos = lib.nixosSystem {
        inherit system;
        specialArgs = sharedArgs;
        modules = [
          disko.nixosModules.disko
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = homeArgs;
            home-manager.users.unseen = import ./nix/home-manager/systems/logos/home.nix;
          }
          ./nix/nixos/hosts/logos
        ];
      };

      logosIso = lib.nixosSystem {
        inherit system;
        specialArgs = sharedArgs;
        modules = [ ./nix/nixos/installer/logos-iso.nix ];
      };

      homeConfigurations = {
        "unseen@logos" = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = homeArgs;
          modules = [
            ./nix/home-manager/systems/logos/home.nix
          ];
        };
        "unseen@flake" = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = homeArgs;
          modules = [
            ./nix/home-manager/systems/flake/home.nix
          ];
        };
        "unseen@desktop" = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = homeArgs;
          modules = [
            ./nix/home-manager/systems/desktop/home.nix
          ];
        };
        "unseen@hunter02" = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = homeArgs;
          modules = [
            ./nix/home-manager/systems/hunter02/home.nix
          ];
        };
      };

      flash-logos = pkgs.writeShellApplication {
        name = "flash-logos";
        runtimeInputs = with pkgs; [
          fzf
          util-linux
          jq
          coreutils
        ];
        text = builtins.readFile ./scripts/flash-logos.sh;
      };

      desktopHome = homeConfigurations."unseen@desktop";
      aiClientThemeCheck =
        assert !builtins.hasAttr ".codex/config.toml" desktopHome.config.home.file;
        assert !builtins.hasAttr ".codex/config.yaml" desktopHome.config.home.file;
        assert desktopHome.config.programs.opencode.enable;
        assert desktopHome.config.programs.opencode.package == pkgs.opencode;
        assert desktopHome.config.programs.opencode.enableMcpIntegration;
        assert desktopHome.config.programs.opencode.tui.theme == "outrun";
        assert
          desktopHome.config.programs.opencode.settings.provider.openai.options.baseURL
          == "http://127.0.0.1:8787/openai/v1";
        assert desktopHome.config.programs.codex.enable;
        assert !desktopHome.config.programs.codex.enableMcpIntegration;
        assert desktopHome.config.programs.codex.package.version == pkgs.codex.version;
        assert desktopHome.config.programs.chatgpt-desktop.enable;
        pkgs.runCommand "ai-client-theme-check"
          {
            nativeBuildInputs = [ pkgs.jq ];
            opencodeTheme =
              desktopHome.config.home.file."${desktopHome.config.xdg.configHome}/opencode/themes/outrun.json".source;
            codexTheme = desktopHome.config.home.file.".codex/themes/outrun.tmTheme".source;
            chatgptTheme = desktopHome.config.home.file.".codex/themes/outrun-desktop.json".source;
          }
          ''
            jq -e '
              .defs.deepBackground == "#170c32" and
              .defs.background == "#202146" and
              .defs.pink == "#f6019d" and
              .defs.cyan == "#2de2e6" and
              .theme.primary == "pink" and
              .theme.background == "deepBackground"
            ' "$opencodeTheme" >/dev/null

            grep -Fq '<string>#170c32</string>' "$codexTheme"
            grep -Fq '<string>#f6019d</string>' "$codexTheme"
            grep -Fq '<string>#2de2e6</string>' "$codexTheme"

            jq -e '
              .appearanceTheme == "dark" and
              .appearanceDarkChromeTheme.surface == "#170c32" and
              .appearanceDarkChromeTheme.accent == "#f6019d" and
              .appearanceDarkChromeTheme.ink == "#f3f4f5" and
              .appearanceDarkChromeTheme.semanticColors.diffAdded == "#62ff00" and
              .appearanceDarkChromeTheme.semanticColors.diffRemoved == "#dd546e"
            ' "$chatgptTheme" >/dev/null

            touch "$out"
          '';
      codexConfigPatchCheck =
        let
          python = pkgs.python3.withPackages (pythonPackages: [ pythonPackages.tomlkit ]);
          fixture = pkgs.writeText "codex-config-fixture.toml" ''
            # Existing user configuration must survive theme updates.
            model = "gpt-test"

            [projects."/tmp/example"]
            trust_level = "trusted"
          '';
          patch = pkgs.writeText "codex-config-patch.json" ''
            {
              "desktop": {
                "appearanceTheme": "dark",
                "appearanceDarkChromeTheme": {
                  "accent": "#f6019d",
                  "contrast": 64,
                  "fonts": { "code": "Hack Nerd Font", "ui": "Noto Sans" },
                  "ink": "#f3f4f5",
                  "opaqueWindows": false,
                  "semanticColors": {
                    "diffAdded": "#62ff00",
                    "diffRemoved": "#dd546e",
                    "skill": "#2de2e6"
                  },
                  "surface": "#170c32"
                }
              },
              "tui": { "theme": "outrun" }
            }
          '';
        in
        pkgs.runCommand "codex-config-patch-check" { nativeBuildInputs = [ python ]; } ''
          install -m 600 ${fixture} config.toml
          python3 ${./nix/home-manager/files/apply-codex-config.py} config.toml ${patch}

          grep -Fq '# Existing user configuration must survive theme updates.' config.toml
          grep -Fq 'model = "gpt-test"' config.toml
          grep -Fq 'trust_level = "trusted"' config.toml
          grep -Fq 'appearanceTheme = "dark"' config.toml
          grep -Fq 'theme = "outrun"' config.toml

          cp config.toml first-pass.toml
          python3 ${./nix/home-manager/files/apply-codex-config.py} config.toml ${patch}
          cmp first-pass.toml config.toml
          touch "$out"
        '';
    in
    {
      nixosConfigurations = {
        inherit logos;
        logos-iso = logosIso;
      };

      inherit homeConfigurations;

      packages.${system} = {
        default = logos.config.system.build.toplevel;
        logos = logos.config.system.build.toplevel;
        logos-iso = logosIso.config.system.build.isoImage;
        install-logos = installLogos;
        install-logos-gui = installLogosGui;
        prolog-mcp = prologMcp;
        brave-mcp = braveMcp;
        inherit flash-logos;
        unseen-home = homeConfigurations."unseen@logos".activationPackage;
      };

      apps.${system} = {
        install-logos = {
          type = "app";
          program = "${installLogos}/bin/install-logos";
        };
        install-logos-gui = {
          type = "app";
          program = "${installLogosGui}/bin/install-logos-gui";
        };
      };

      checks.${system} = {
        logos = logos.config.system.build.toplevel;
        install-logos = installLogos;
        prolog-mcp = prologMcp;
        brave-mcp = braveMcp;
        unseen-home = homeConfigurations."unseen@logos".activationPackage;
        unseen-flake-home = homeConfigurations."unseen@flake".activationPackage;
        unseen-desktop-home = homeConfigurations."unseen@desktop".activationPackage;
        unseen-hunter02-home = homeConfigurations."unseen@hunter02".activationPackage;
        ai-client-theme = aiClientThemeCheck;
        codex-config-patch = codexConfigPatchCheck;
      };

      formatter.${system} = pkgs.nixfmt-rfc-style;
    };
}