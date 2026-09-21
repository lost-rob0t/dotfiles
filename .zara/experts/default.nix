{ pkgs }:

let
  emacs = pkgs.callPackage ./emacs/kb/default.nix { };
  library = pkgs.runCommand "dotfiles-zara-expert-library"
    {
      src = ../..;
      nativeBuildInputs = [
        pkgs.swi-prolog
        pkgs.bash
        pkgs.nix
        pkgs.sbcl
        pkgs.emacs-nox
      ];
    }
    ''
      set -eu
      cp -R "$src" source
      chmod -R u+w source
      cd source

      swipl -q -f none -s .zara/experts/git/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/home-manager/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/dotfiles/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/zara/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/sysadmin/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/javascript/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/typescript/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/java/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/kotlin/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/bash/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/nix/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/prolog/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/python/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/nim/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/lisp/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/common-lisp/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/emacs-lisp/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/mara/tests/router-tests.pl
      swipl -q -f none -s .zara/experts/todo/tests/expert-tests.pl
      swipl -q -f none -s .zara/experts/roam/tests/expert-tests.pl

      bash -n .zara/experts/bash/tests/fixtures/valid.sh
      export NIX_STATE_DIR="$TMPDIR/nix-state"
      mkdir -p "$NIX_STATE_DIR"
      nix-instantiate --parse .zara/experts/nix/tests/fixtures/valid.nix >/dev/null

      sbcl --noinform --disable-debugger \
        --script .zara/experts/common-lisp/tests/reader-check.lisp \
        .zara/experts/common-lisp/tests/fixtures/valid.lisp
      if sbcl --noinform --disable-debugger \
          --script .zara/experts/common-lisp/tests/reader-check.lisp \
          .zara/experts/common-lisp/tests/fixtures/missing-paren.lisp; then
        echo "broken Common Lisp fixture unexpectedly parsed" >&2
        exit 1
      fi

      export HOME="$TMPDIR/home"
      mkdir -p "$HOME"
      ZARA_ELISP_FIXTURE=.zara/experts/emacs-lisp/tests/fixtures/valid.el \
        emacs --batch -Q -l .zara/experts/emacs-lisp/tests/reader-check.el
      if ZARA_ELISP_FIXTURE=.zara/experts/emacs-lisp/tests/fixtures/missing-paren.el \
          emacs --batch -Q -l .zara/experts/emacs-lisp/tests/reader-check.el; then
        echo "broken Emacs Lisp fixture unexpectedly parsed" >&2
        exit 1
      fi
      emacs --batch -Q --eval \
        '(progn (setq byte-compile-error-on-warn t) (byte-compile-file ".zara/experts/emacs-lisp/tests/fixtures/valid.el"))'
      emacs --batch -Q -l lisp/mara/mara-tools.el --eval \
        '(unless (and (fboundp (quote mara-tools--todo-snapshot)) (fboundp (quote mara-tools-register-zara-adapters))) (kill-emacs 1))'

      mkdir -p "$out/share/zara/dotfiles/.prolog"
      cp -R .zara "$out/share/zara/dotfiles/.zara"
      cp -R .prolog/kb "$out/share/zara/dotfiles/.prolog/kb"
    '';
in
{
  inherit emacs library;
}
