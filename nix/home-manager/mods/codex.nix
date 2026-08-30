{ config, lib, inputs, ... }:

let
  cfg = config.codex;

  skillsSource = inputs.skills;
  availableSkills = builtins.attrNames (
    lib.filterAttrs (_: entryType: entryType == "directory") (
      builtins.readDir "${skillsSource}/skills"
    )
  );
in
{
  options.codex = {
    enable = lib.mkEnableOption "codex CLI integration with declaratively managed skills";

    skills = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = availableSkills;
      description = ''
        Skills from the skills flake input to link into ~/.codex/skills.
        Defaults to every skill directory in the pinned input; local-only
        skills that are not part of the input are left untouched.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home.file = lib.listToAttrs (
      map (name: {
        name = ".codex/skills/${name}";
        value = { source = "${skillsSource}/skills/${name}"; };
      }) cfg.skills
    );
  };
}
