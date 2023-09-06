{ config, lib, pkgs, ... }:

{
  programs.firejail = {
    enable = true;
    wrappedBinaries = {

      brave = {
        executable = "${lib.getBin pkgs.brave}/bin/brave";
        profile = "${pkgs.firejail}/etc/firejail/brave.profile";
      };
      thunderbird = {
        executable = "${lib.getBin pkgs.thunderbird}/bin/thunderbird";
        profile = "${pkgs.firejail}/etc/firejail/thunderbird.profile";
      };
    };
  };
}
