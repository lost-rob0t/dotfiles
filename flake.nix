{
  description = "NixOS configuration and installer for logos";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      disko,
    }:
    let
      system = "x86_64-linux";
      lib = nixpkgs.lib;
      pkgs = nixpkgs.legacyPackages.${system};

      installLogos = pkgs.writeShellApplication {
        name = "install-logos";
        runtimeInputs = [
          disko.packages.${system}.disko-install
          pkgs.coreutils
          pkgs.gnugrep
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
        modules = [
          disko.nixosModules.disko
          ./nix/nixos/hosts/logos
        ];
      };

      logosIso = lib.nixosSystem {
        inherit system;
        specialArgs = { inherit self; };
        modules = [ ./nix/nixos/installer/logos-iso.nix ];
      };
    in
    {
      nixosConfigurations = {
        inherit logos;
        logos-iso = logosIso;
      };

      packages.${system} = {
        default = logos.config.system.build.toplevel;
        logos = logos.config.system.build.toplevel;
        logos-iso = logosIso.config.system.build.isoImage;
        install-logos = installLogos;
        install-logos-gui = installLogosGui;
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
      };

      formatter.${system} = pkgs.nixfmt-rfc-style;
    };
}
