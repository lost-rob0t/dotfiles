{
  home-manager,
  inputs,
  lib,
  pkgs,
}:
let
  home = home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    extraSpecialArgs = { inherit inputs; };
    modules = [
      ../nix/home-manager/mods/agent-verification.nix
      ../nix/home-manager/mods/opencode-prolog.nix
      {
        home = {
          username = "prolog-test";
          homeDirectory = "/home/prolog-test";
          stateVersion = "25.11";
        };
        agentVerification.enable = true;
        agentProlog.bootstrap.enable = true;
      }
      # stubs for the codex/opencode option layers the full flake provides
      ({ lib, ... }: {
        options.codex.globalAgentsFile = lib.mkOption { type = lib.types.anything; };
        options.opencode.globalAgentsFile = lib.mkOption { type = lib.types.anything; };
      })
    ];
  };
  byName = name: lib.findFirst (p: lib.getName p == name) null home.config.home.packages;
  bootstrap = byName "prolog-bootstrap";
  verifier = byName "prolog-verify";
in
assert bootstrap != null;
assert verifier != null;
pkgs.runCommand "opencode-prolog-bootstrap-check"
  {
    nativeBuildInputs = [
      pkgs.swi-prolog
      verifier
      bootstrap
    ];
  }
  ''
    work=$(mktemp -d)

    prolog-bootstrap --task demo "$work" > bootstrap.log

    test -f "$work/.prolog/facts.kb"
    test -f "$work/.prolog/verify.pl"
    test -f "$work/.prolog/checks/check-journal.pl"
    test -f "$work/.prolog/checks/floors.pl"
    test -f "$work/.prolog/handoff.pl"

    journal=$(find "$work/.prolog/runs" -maxdepth 2 -name evidence.pl)
    test -f "$journal"
    echo "observation(demo, 'bootstrap check')." >> "$journal"

    # activate the shipped floors template: one observation clause is required,
    # so the gate checks both load integrity and the floor
    echo "min_clauses(observation/2, 1)." >> "$work/.prolog/checks/floors.pl"

    swipl "$work/.prolog/checks/check-journal.pl" -- "$journal"
    echo "gate accepted the healthy journal"

    # a missing period at EOF makes SWI skip the final term, dropping the
    # clause count below the floor: the strict gate must fail closed
    cp -r "$work/.prolog" corrupt-prolog
    corrupt=$(find corrupt-prolog/runs -maxdepth 2 -name evidence.pl)
    sed -i 's/\.$//' "$corrupt"
    status=0
    output=$(swipl corrupt-prolog/checks/check-journal.pl -- "$corrupt" 2>&1) || status=$?
    test "$status" -eq 1 || {
      echo "gate exit $status (wanted 1) for the corrupt journal: $output" >&2
      exit 1
    }
    echo "gate rejected the corrupt journal"

    mv "$work" "$out"
    chmod -R u+w "$out"
  ''
