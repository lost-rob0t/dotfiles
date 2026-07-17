{
  description = "NixOS configuration for logos";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";
    in
    {
      nixosConfigurations.logos = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [ ./nix/nixos/hosts/logos ];
      };

      packages.${system}.default = self.nixosConfigurations.logos.config.system.build.toplevel;
      checks.${system}.logos = self.packages.${system}.default;
    };
}
