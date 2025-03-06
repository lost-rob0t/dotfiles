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
      roswell
      sbcl
      sbclPackages.qlot-cli
      openssl
      pkg-config
    ];
     home.sessionVariables = {
      LD_LIBRARY_PATH = lib.mkIf pkgs.stdenv.isLinux "${pkgs.openssl.out}/lib:$LD_LIBRARY_PATH";
    };
  };

}
