{
  description = "NixOS configuration and installer for logos";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};

      logos = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [ ./nix/nixos/hosts/logos ];
      };

      logosIso = nixpkgs.lib.nixosSystem {
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
      };

      checks.${system}.logos = logos.config.system.build.toplevel;
      formatter.${system} = pkgs.nixfmt-rfc-style;
    };
}
