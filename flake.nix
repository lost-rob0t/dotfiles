{
  description = "NixOS configurations, installer, and Home Manager dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-25.11";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    hardware.url = "github:nixos/nixos-hardware";
    mousetrap.url = "github:lost-rob0t/Mousetrap";
    nix-pre-commit.url = "github:jmgilman/nix-pre-commit";
    zara.url = "github:lost-rob0t/zara";
    org-vector.url = "github:lost-rob0t/org-vector";
    bixby-studio.url = "github:lost-rob0t/org-vector";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      home-manager,
      ...
    }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      specialArgs = {
        inherit inputs;
        outputs = self;
      };

      logos = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [ ./nix/nixos/hosts/logos ];
      };

      logosIso = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = { inherit self; };
        modules = [ ./nix/nixos/installer/logos-iso.nix ];
      };

      flake = nixpkgs.lib.nixosSystem {
        inherit system specialArgs;
        modules = [
          ./nix/nixos/mods/default.nix
          ./nix/nixos/systems/flake/flake.nix
        ];
      };

      flakeNvidia = nixpkgs.lib.nixosSystem {
        inherit system specialArgs;
        modules = [
          ./nix/nixos/mods/default.nix
          ./nix/nixos/systems/flake/flake.nix
          ./nix/nixos/systems/flake/flake-nvidia.nix
        ];
      };
    in
    {
      nixosConfigurations = {
        inherit logos flake;
        logos-iso = logosIso;
        flake-nvidia = flakeNvidia;
      };

      homeConfigurations = {
        "unseen@flake" = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = specialArgs;
          modules = [ ./nix/home-manager/systems/desktop/home.nix ];
        };

        "unseen@hunter02" = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = specialArgs;
          modules = [ ./nix/home-manager/systems/hunter02/home.nix ];
        };
      };

      packages.${system} = {
        default = logos.config.system.build.toplevel;
        logos = logos.config.system.build.toplevel;
        logos-iso = logosIso.config.system.build.isoImage;
      };

      checks.${system}.logos = logos.config.system.build.toplevel;
      formatter.${system} = pkgs.nixfmt-rfc-style;

      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [
          openssl
          pkg-config
          sbcl
          ecl
        ];

        shellHook = ''
          export LD_LIBRARY_PATH=${
            nixpkgs.lib.makeLibraryPath [
              pkgs.openssl
              pkgs.file
            ]
          }:$LD_LIBRARY_PATH
        '';
      };
    };
}
