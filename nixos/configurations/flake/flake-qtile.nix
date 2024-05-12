{ inputs, config, lib, pkgs, ... }:

{
  system.nixos.tags = ["qtile"];
  services = {
    xserver = {
      windowManager = {
        session = [{
          name = "qtile";
          start = ''
            ${inputs.nixpkgs-stable.legacyPackages.x86_64-linux.qtile}/bin/qtile start -b x11 \
            --config  /home/unseen/.config/qtile/config.py &
            waitPID=$!
          '';
        }];
      };
    };
  };


}
