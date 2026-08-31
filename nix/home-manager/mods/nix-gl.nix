{ lib, pkgs, config, ... }:

let
  nixGlSetup = pkgs.writeShellApplication {
    name = "nix-gl-setup";
    runtimeInputs = with pkgs; [
      coreutils
      diffutils
      glibc
    ];
    text = builtins.readFile ../../../scripts/nix-gl-setup;
  };
in
{
  options.nixGl = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Provision /run/opengl-driver for Nix GUI apps on non-NixOS hosts.

        Installs the nix-gl-setup helper. Run it once with root:

          pkexec --disable-internal-agent nix-gl-setup
      '';
    };
  };

  config = lib.mkIf config.nixGl.enable {
    home.packages = [ nixGlSetup ];

    # Non-fatal drift check: the tmpfiles rule must keep pointing at this
    # user's nix profile or Nix GUI apps lose hardware acceleration.
    home.activation.nixGlDriverCheck = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      if ! ${pkgs.gnugrep}/bin/grep -qF "${config.home.homeDirectory}/.nix-profile" /etc/tmpfiles.d/nix-opengl-driver.conf 2>/dev/null; then
        echo "nix-gl: /etc/tmpfiles.d/nix-opengl-driver.conf is missing or stale; run: pkexec --disable-internal-agent nix-gl-setup" >&2
      fi
    '';
  };
}
