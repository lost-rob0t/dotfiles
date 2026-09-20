{ lib, stdenvNoCC, emacs-nox, python3, swi-prolog }:

stdenvNoCC.mkDerivation {
  pname = "dotfiles-zara-emacs-expert";
  version = "0.1.0";
  src = ./.;
  nativeBuildInputs = [ emacs-nox python3 swi-prolog ];
  doCheck = true;

  buildPhase = ''
    runHook preBuild
    export HOME="$TMPDIR/emacs-home"
    mkdir -p "$HOME"
    export LC_ALL=C.UTF-8
    export ZARA_EMACS_KB_SOURCE=${emacs-nox}
    emacs -Q --batch -l export.el -f zara-emacs-kb-export-batch > corpus.jsonl
    python3 emacs_kb.py --input corpus.jsonl --out generated
    cp generated/corpus.pl ./corpus.pl
    runHook postBuild
  '';

  checkPhase = ''
    runHook preCheck
    PYTHONPATH="$PWD" python3 ${../test/test_kb.py}
    emacs -Q --batch -l tests/export-tests.el -f ert-run-tests-batch-and-exit
    swipl -q -f none -s tests/corpus-tests.pl -g "(run_tests -> halt(0) ; halt(1))"
    python3 tests/roundtrip.py
    emacs -Q --batch -l export.el -f zara-emacs-kb-export-batch > repeated.jsonl
    python3 emacs_kb.py --input repeated.jsonl --out repeated
    cmp generated/corpus.pl repeated/corpus.pl
    cmp generated/manifest.json repeated/manifest.json
    runHook postCheck
  '';

  installPhase = ''
    runHook preInstall
    dest="$out/share/zara/experts/emacs"
    mkdir -p "$dest"
    cp generated/corpus.pl generated/manifest.json expert.pl corpus.jsonl "$dest/"
    cp ${emacs-nox}/share/emacs/*/etc/COPYING "$dest/COPYING.emacs"
    runHook postInstall
  '';

  meta = {
    description = "Dotfiles-owned GNU Emacs documentation corpus and read-only Prolog expert";
    license = lib.licenses.gpl3Plus;
    platforms = lib.platforms.linux;
  };
}
