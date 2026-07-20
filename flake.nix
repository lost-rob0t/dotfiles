{
  description = "NixOS configuration and installer for logos";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    disko = {
      url = "github:nix-community/disko/latest";
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
      pkgs = nixpkgs.legacyPackages.${system};

      sharedArgs = {
        inherit self disko;
      };

      logos = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = sharedArgs;
        modules = [
          disko.nixosModules.disko
          ./nix/nixos/hosts/logos
        ];
      };

      logosIso = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = sharedArgs;
        modules = [ ./nix/nixos/installer/logos-iso.nix ];
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

      packages.${system} = {
        default = logos.config.system.build.toplevel;
        logos = logos.config.system.build.toplevel;
        logos-iso = logosIso.config.system.build.isoImage;
        inherit flash-logos;
      };

      checks.${system} = {
        logos = logos.config.system.build.toplevel;
        logos-iso = logosIso.config.system.build.isoImage;
      };
      formatter.${system} = pkgs.nixfmt-rfc-style;
    };
}
