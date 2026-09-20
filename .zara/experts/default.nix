{ pkgs }:

let
  emacs = pkgs.callPackage ./emacs/kb/default.nix { };
  library = pkgs.runCommand "dotfiles-zara-expert-library"
    {
      src = ../..;
      nativeBuildInputs = [ pkgs.swi-prolog ];
    }
    ''
      set -eu
      cp -R "$src" source
      chmod -R u+w source
      cd source

      swipl -q -f none -s .zara/experts/git/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/home-manager/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/zara/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/sysadmin/tests/expert-tests.pl

      mkdir -p "$out/share/zara/dotfiles/.prolog"
      cp -R .zara "$out/share/zara/dotfiles/.zara"
      cp -R .prolog/kb "$out/share/zara/dotfiles/.prolog/kb"
    '';
in
{
  inherit emacs library;
}
