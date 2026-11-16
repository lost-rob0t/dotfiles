{
  config,
  lib,
  ...
}:
let
  cfg = config.opencodeSecurity;
in
{
  options.opencodeSecurity.enable = lib.mkEnableOption "destructive-shell security hooks for OpenCode (rmguard plugin plus ask/deny rules)";

  config = lib.mkIf cfg.enable {
    opencode = {
      hookPlugins.rmguard = ../../../opencode/plugins/rmguard/index.ts;

      shellPermissions = [
        {
          action = "shell";
          resource = "rm -r *";
          effect = "ask";
        }
        {
          action = "shell";
          resource = "rm -R *";
          effect = "ask";
        }
        {
          action = "shell";
          resource = "rm -rf *";
          effect = "ask";
        }
        {
          action = "shell";
          resource = "rm -fr *";
          effect = "ask";
        }
        {
          action = "shell";
          resource = "rm --recursive *";
          effect = "ask";
        }
        {
          action = "shell";
          resource = "sudo rm *";
          effect = "ask";
        }
        {
          action = "shell";
          resource = "find * -delete";
          effect = "ask";
        }
        {
          action = "shell";
          resource = "find * -delete *";
          effect = "ask";
        }
        {
          action = "shell";
          resource = "find * -exec rm *";
          effect = "ask";
        }
        {
          action = "shell";
          resource = "chmod -R *";
          effect = "ask";
        }
        {
          action = "shell";
          resource = "chown -R *";
          effect = "ask";
        }
        {
          action = "shell";
          resource = "shred *";
          effect = "ask";
        }
        {
          action = "shell";
          resource = "dd *";
          effect = "ask";
        }
        {
          action = "shell";
          resource = "mkfs*";
          effect = "ask";
        }
      ];

      denyPolicies =
        map
          (r: {
            action = "tool.use";
            resource = r;
            effect = "deny";
          })
          [
            "shell:rm -rf ~*"
            "shell:rm -rf $HOME*"
            "shell:rm -rf /home/*"
            "shell:rm -fr ~*"
            "shell:rm -fr /home/*"
            "shell:sudo rm -rf ~*"
            "shell:sudo rm -rf /home/*"
            "shell:rm -rf /etc*"
            "shell:rm -rf /usr*"
            "shell:rm -rf /var*"
            "shell:rm -rf /nix*"
            "shell:rm -rf /boot*"
            "shell:dd * of=/dev/s??"
            "shell:dd * of=/dev/s??*"
            "shell:dd * of=/dev/nvme*"
            "shell:dd * of=/dev/mapper/*"
          ];
    };
  };
}
