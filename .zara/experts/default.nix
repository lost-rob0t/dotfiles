{ pkgs }:

let
  emacs = pkgs.callPackage ./emacs/kb/default.nix { };
  library = pkgs.runCommand "dotfiles-zara-expert-library"
    {
      src = ../..;
      nativeBuildInputs = [ pkgs.swi-prolog pkgs.bash pkgs.nix ];
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
      swipl -q -f none -s .zara/experts/javascript/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/typescript/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/java/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/kotlin/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/bash/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/nix/tests/expert-tests.pl

      bash -n .zara/experts/bash/tests/fixtures/valid.sh
      export NIX_STATE_DIR="$TMPDIR/nix-state"
      mkdir -p "$NIX_STATE_DIR"
      nix-instantiate --parse .zara/experts/nix/tests/fixtures/valid.nix >/dev/null

      mkdir -p "$out/share/zara/dotfiles/.prolog"
      cp -R .zara "$out/share/zara/dotfiles/.zara"
      cp -R .prolog/kb "$out/share/zara/dotfiles/.prolog/kb"
    '';
in
{
  inherit emacs library;
}
