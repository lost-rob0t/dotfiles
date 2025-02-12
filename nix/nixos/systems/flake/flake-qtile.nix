{ inputs, config, lib, pkgs, ... }:

{
  system.nixos.tags = ["qtile"];
  services = {
    xserver = {
      windowManager = {
        session = [{
          name = "qtile";
          start = ''
            ${pkgs.python3Packages.qtile}/bin/qtile start -b x11 \
            --config  /home/unseen/.config/qtile/config.py &
            waitPID=$!
          '';
        }];
      };
    };
  };


}
