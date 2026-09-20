{ pkgs }:

let
  emacs = pkgs.callPackage ./emacs/kb/default.nix { };
  library = pkgs.runCommand "dotfiles-zara-expert-library"
    { nativeBuildInputs = [ pkgs.swi-prolog ]; }
    ''
      set -eu
      swipl -q -f none -s ${./git/tests/expert-tests.pl}
      swipl -q -f none -s ${./home-manager/tests/expert-tests.pl}
      swipl -q -f none -s ${./zara/tests/expert-tests.pl}
      swipl -q -f none -s ${./sysadmin/tests/expert-tests.pl}
      mkdir -p "$out/share/zara"
      cp -R ${./.} "$out/share/zara/experts"
    '';
in
{
  inherit emacs library;
}
