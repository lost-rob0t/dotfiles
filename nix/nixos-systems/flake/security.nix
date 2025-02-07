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
  security.pam.loginLimits = [{
    # Do i still need this?
    domain = "*";
    type = "soft";
    item = "nofile";
    value = "80192";
  }];
  services = {
    opensnitch = {
      enable = false;
};
    };

  security.sudo.extraRules = [
     { users = [ "unseen"];
       commands = [ { command = "${pkgs.pmutils}/bin/pm-hibernate"; options = [ "SETENV" "NOPASSWD" ]; } ];}];
}
