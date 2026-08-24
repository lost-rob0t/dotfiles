{
  description = "NixOS configuration and installer for logos";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    disko = {
      url = "github:nix-community/disko/latest";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    skills.url = "github:lost-rob0t/skills";
    chatgpt-desktop = {
      url = "github:lost-rob0t/chatgpt-desktop";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    zara.url = "github:lost-rob0t/zara";
    org-vector.url = "github:lost-rob0t/org-vector";
    bixby-studio.url = "github:lost-rob0t/bixby-studio";
    mousetrap.url = "github:lost-rob0t/Mousetrap";
  };

  outputs =
    {
      self,
      nixpkgs,
      disko,
      home-manager,
      skills,
      chatgpt-desktop,
      zara,
      org-vector,
      bixby-studio,
      mousetrap,
    }:
    let
      system = "x86_64-linux";
      lib = nixpkgs.lib;
      pkgs = nixpkgs.legacyPackages.${system};
      prologMcp = pkgs.callPackage ./nix/packages/prolog-mcp.nix { };

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
        unseen-home = homeConfigurations."unseen@logos".activationPackage;
        unseen-flake-home = homeConfigurations."unseen@flake".activationPackage;
        unseen-desktop-home = homeConfigurations."unseen@desktop".activationPackage;
        unseen-hunter02-home = homeConfigurations."unseen@hunter02".activationPackage;
      };

      formatter.${system} = pkgs.nixfmt-rfc-style;
    };
}
