{ lib, pkgs, config, ... }:

{
  options = {
    services.cifsMount = {
      enable = lib.mkEnableOption "Enable CIFS mounts (public and per-user)";
      server = lib.mkOption {
        type = lib.types.str;
        default = "storage.lost.system";
        description = "The CIFS server IP address or hostname";
      };
    };
  };

  config = lib.mkIf config.services.cifsMount.enable {
    fileSystems = lib.mkMerge [
      {
        "/share" = {
          device = "//${config.services.cifsMount.server}/";
          fsType = "cifs";
          options = [
            "x-systemd.automount"
            "noauto"
            "x-systemd.idle-timeout=60"
            "x-systemd.device-timeout=5s"
            "x-systemd.mount-timeout=5s"
            "credentials=/etc/nixos/smb-secrets"
          ];
        };
      }
      (lib.concatMapAttrs (userName: user: {
          "${user.home}/mnt/share" = {
            device = "//${config.services.cifsMount.server}/";
            fsType = "cifs";
            options = let
              automountOpts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
            in [
              automountOpts
              "credentials=${user.home}/.config/smb.creds"
              "uid=${toString user.uid}"
              "gid=${toString user.gid}"
            ];
          };
        }) config.users.users)
    ];
  };
}
