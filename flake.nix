{
  # thanks to the kind work of Misterio77 for making a working example for me to use
  # https://github.com/Misterio77/nix-starter-configs/blob/main/standard/flake.nix
  description = "My Nixos Config";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.11";
    # Also see the 'unstable-packages' overlay at 'overlays/default.nix'.

    # Home manager
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # Emacs Overlay
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    hardware.url = "github:nixos/nixos-hardware";
    # For games
    mousetrap.url = "github:lost-rob0t/Mousetrap";
    # Shameless plug: looking for a way to nixify your themes and make
    # everything match nicely? Try nix-colors!
    # nix-colors.url = "github:misterio77/nix-colors";
    nix-pre-commit.url = "github:jmgilman/nix-pre-commit";
    #nimble.url = "github:lost-rob0t/flake-nimble";
  };

  outputs = { self, nixpkgs, nixpkgs-stable, home-manager, nixos-hardware, ... }@inputs:
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
      # Acessible through 'nix build', 'nix shell', etc
      #packages = forAllSystems (system:
      #let pkgs = nixpkgs.legacyPackages.${system};
      #in import ./pkgs { inherit pkgs; }
      #);
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
            #./nix/nixos/mods/default.nix
            ./nix/nixos/systems/flake/flake.nix
            ./nix/nixos/systems/flake/flake-qtile.nix
            ./nix/nixos/systems/flake/flake-budgie.nix
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
