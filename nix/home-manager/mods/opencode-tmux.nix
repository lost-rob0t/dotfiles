# Generated from scripts/opencode-tmux.org.
{ config, lib, pkgs, ... }:
let
  cfg = config.opencode.tmux;
  runtime = [ pkgs.python3 pkgs.bash pkgs.tmux pkgs.fzf pkgs.coreutils config.opencode.package ];
  tools = pkgs.runCommand "opencode-tmux-tools" {
    nativeBuildInputs = [ pkgs.makeWrapper ];
  } ''
    mkdir -p "$out/bin" "$out/lib/dotfiles"
    cp ${../../../.local/lib/dotfiles/opencode_tmux.py} "$out/lib/dotfiles/opencode_tmux.py"
    cp ${../../../.local/lib/dotfiles/ezf.el} "$out/lib/dotfiles/ezf.el"
    cp ${../../../.local/bin/opencode-tmux} "$out/bin/opencode-tmux"
    cp ${../../../.local/bin/opencode-pick} "$out/bin/opencode-pick"
    cp ${../../../.local/bin/ezf} "$out/bin/ezf"
    cp ${../../../.local/bin/ezf.sh} "$out/bin/ezf.sh"
    chmod +x "$out"/bin/*
    patchShebangs "$out/bin"
    for command in opencode-tmux opencode-pick ezf ezf.sh; do
      wrapProgram "$out/bin/$command" --prefix PATH : ${lib.escapeShellArg (lib.makeBinPath runtime)}
    done
  '';
in
{
  options.opencode.tmux = {
    enable = lib.mkEnableOption "named OpenCode tmux sessions and hardened EZF" // { default = true; };
    picker = lib.mkOption {
      type = lib.types.enum [ "auto" "fzf" "ezf" ];
      default = "auto";
      description = "Session picker; auto uses EZF inside Emacs and fzf elsewhere.";
    };
  };
  config = lib.mkIf (config.opencode.enable && cfg.enable) {
    home.packages = [ tools ];
    home.sessionVariables.OPENCODE_TMUX_PICKER = cfg.picker;
  };
}
