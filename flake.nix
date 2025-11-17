{
  # thanks to the kind work of Misterio77 for making a working example for me to use
  # https://github.com/Misterio77/nix-starter-configs/blob/main/standard/flake.nix
  description = "My Nixos Config";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.11";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    hardware.url = "github:nixos/nixos-hardware";
    mousetrap.url = "github:lost-rob0t/Mousetrap";
    nix-pre-commit.url = "github:jmgilman/nix-pre-commit";
    zara.url = "github:lost-rob0t/zara";
    org-vector.url = "github:lost-rob0t/org-vector";
    bixby-studio.url = "github:lost-rob0t/org-vector";
  };

  outputs = { self,
              org-vector,
              zara,
              nixpkgs,
              nixpkgs-stable,
              home-manager,
              nixos-hardware,
              bixby-studio,
              ... }@inputs:
    let
      inherit (self) outputs;
      forAllSystems = nixpkgs.lib.genAttrs [
        "aarch64-linux"
        "i686-linux"
        "x86_64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      npkgs = nixpkgs.legacyPackages.x86_64-linux;
    in
    rec {
      # Your custom packages
      # Accessible through 'nix build', 'nix shell', etc
      # Devshell for bootstrapping
      # Acessible through 'nix develop' or 'nix-shell' (legacy)
      #devShells = forAllSystems (system:
      #  let pkgs = nixpkgs.legacyPackages.${system};
      #  in import ./shell.nix { inherit pkgs; }
      #);

      # Your custom packages and modifications, exported as overlays
      #overlays = import ./overlays { inherit inputs; };
      # Reusable nixos modules you might want to export
      # These are usually stuff you would upstream into nixpkgs
      #nixosModules = import ./modules/nixos;
      # Reusable home-manager modules you might want to export
      # These are usually stuff you would upstream into home-manager
      #homeManagerModules = import ./modules/home-manager;

      # NixOS configuration entrypoint
      # Available through 'nixos-rebuild --flake .#your-hostname'
      nixosConfigurations = {
        flake = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs outputs; };
          modules = [
            ./nix/nixos/mods/default.nix
            ./nix/nixos/systems/flake/flake.nix
            #./nix/nixos/systems/flake/flake-qtile.nix
            #./nix/nixos/systems/flake/flake-budgie.nix
          ];
        };
        # flake-qtile = nixpkgs.lib.nixosSystem {
        #   specialArgs = { inherit inputs outputs; };
        #   modules = [
        #     # > Our main nixos configuration file <
        #     ./nix/nixos/systems/flake/flake.nix
        #     ./nix/nixos/systems/flake/flake-qtile.nix

        #   ];
        # };
        # flake-budgie = nixpkgs.lib.nixosSystem {
        #   specialArgs = { inherit inputs outputs; };
        #   modules = [
        #     # > Our main nixos configuration file <
        #     ./nix/nixos/systems/flake/flake.nix
        #     ./nix/nixos/systems/flake/flake-budgie.nix

        #   ];
        # };

      };
      # Standalone home-manager configuration entrypoint
      # Available through 'home-manager --flake .#your-username@your-hostname'
      homeConfigurations = {
        "unseen@flake" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux; # Home-manager requires 'pkgs' instance
          extraSpecialArgs = { inherit inputs outputs; };
          modules = [
            ./nix/home-manager/systems/desktop/home.nix
            #./nix/home-manager/global/global.nix
            # TODO Import all the stuff HERE
          ];
        };

        "unseen@hunter02" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux; # Home-manager requires 'pkgs' instance
          extraSpecialArgs = { inherit inputs outputs; };
          modules = [
            ./nix/home-manager/systems/hunter02/home.nix
          ];
        };
      };

      devShell.x86_64-linux =
        nixpkgs.legacyPackages.x86_64-linux.mkShell {
          buildInputs = with nixpkgs.legacyPackages.x86_64-linux; [
            openssl
            pkg-config
            roswell
            sbcl
            ecl
          ];

          shellHook = ''
            export LD_LIBRARY_PATH=${nixpkgs.legacyPackages.x86_64-linux.lib.makeLibraryPath([nixpkgs.legacyPackages.x86_64-linux.openssl])}:${nixpkgs.legacyPackages.x86_64-linux.lib.makeLibraryPath([nixpkgs.legacyPackages.x86_64-linux.file])}
          '';
        };
    };
}
