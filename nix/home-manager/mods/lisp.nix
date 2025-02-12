{ lib, pkgs, config, ... }: {
  options = with lib; {
    dev = {
      common-lisp.enable = mkOption  {
        type = lib.types.bool;
        default = true;
        description = "Configure system for common lisp development.";
      };};};
  config = with lib; mkIf config.dev.common-lisp.enable {
    home.packages  = with pkgs; [
      sbcl
      roswell
      sbclPackages.qlot-cli

    ];
  };

}
