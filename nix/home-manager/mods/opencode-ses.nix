# Generated from scripts/opencode-ses.org.
{ config, lib, pkgs, ... }:
let
  cfg = config.opencode.ses;
  runtime = [ pkgs.python3 pkgs.bash pkgs.fzf config.opencode.package ];
  tools = pkgs.runCommand "opencode-ses-tools" {
    nativeBuildInputs = [ pkgs.makeWrapper ];
  } ''
    mkdir -p "$out/bin" "$out/lib/dotfiles"
    cp ${../../../.local/lib/dotfiles/opencode_ses.py} "$out/lib/dotfiles/opencode_ses.py"
    cp ${../../../.local/bin/opencode-ses} "$out/bin/opencode-ses"
    chmod +x "$out/bin/opencode-ses"
    patchShebangs "$out/bin"
    wrapProgram "$out/bin/opencode-ses" --prefix PATH : ${lib.escapeShellArg (lib.makeBinPath runtime)}
  '';
in
{
  options.opencode.ses = {
    enable = lib.mkEnableOption "fzf OpenCode session resume" // { default = true; };
  };
  config = lib.mkIf (config.opencode.enable && cfg.enable) {
    home.packages = [ tools ];
  };
}
