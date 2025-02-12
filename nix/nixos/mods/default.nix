{ pkgs, config, ... }: {
  imports = [
    ./base.nix
    ./bluetooth.nix
    ./smb.nix
    ./desktop.nix ];

  config = {
    nix = {
    package = pkgs.nixVersions.stable; # or versioned attributes like nix_2_7
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    };
    nixpkgs.config.allowUnfree = true;


  };
}
