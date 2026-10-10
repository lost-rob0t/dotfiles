{ config, lib, pkgs, ... }:
let
  cfg = config.services.starintel;
  inherit (lib) mkOption types;
  root = cfg.workDir;
  projectName = "starintel-${builtins.substring 0 16 (builtins.hashString "sha256" root)}";
  secretDir = "${root}/config/secrets";
  packageOption = description: mkOption {
    type = types.nullOr types.package;
    default = null;
    inherit description;
  };
  imageNames = cfg.infrastructure.imageTags;
  defaultImageNames = {
    couchdb = "starintel/couchdb:3.5.2";
    clouseau = "starintel/clouseau:3.3.0";
    rabbitmq = "rabbitmq:4.3.4-management";
    valkey = "starintel/valkey:9.0.1";
  };
  image = name: cfg.infrastructure.images.${name};
  mount = name: destination: {
    type = "bind";
    source = "${root}/data/${name}";
    target = destination;
    bind.create_host_path = false;
  };
  secret = name: "/run/secrets/${name}";
  health = test: { inherit test; interval = "5s"; timeout = "5s"; retries = 24; start_period = "15s"; };
  compose = pkgs.writeText "starintel-compose.json" (builtins.toJSON {
    name = projectName;
    services = {
      clouseau = {
        image = imageNames.clouseau;
        pull_policy = "never";
        hostname = "clouseau";
        domainname = "starintel.internal";
        environment.CLOUSEAU_COOKIE_FILE = secret "erlang_cookie";
        secrets = [ "erlang_cookie" ];
        volumes = [ (mount "clouseau" "/var/lib/clouseau") ];
        networks.backend.aliases = [ "clouseau.starintel.internal" ];
        healthcheck = health [ "CMD-SHELL" "epmd -names | grep -q 'name clouseau at port'" ];
      };
      couchdb = {
        image = imageNames.couchdb;
        pull_policy = "never";
        hostname = "couchdb";
        domainname = "starintel.internal";
        environment = {
          COUCHDB_USER = "starintel";
          COUCHDB_PASSWORD_FILE = secret "couchdb_password";
          COUCHDB_SECRET_FILE = secret "couchdb_secret";
          COUCHDB_ERLANG_COOKIE_FILE = secret "erlang_cookie";
          NODENAME = "couchdb.starintel.internal";
        };
        secrets = [ "couchdb_password" "couchdb_secret" "erlang_cookie" ];
        volumes = [ (mount "couchdb" "/opt/couchdb/data") ];
        ports = [ "127.0.0.1:5984:5984" ];
        networks.backend.aliases = [ "couchdb.starintel.internal" ];
        depends_on.clouseau.condition = "service_healthy";
        healthcheck = health [ "CMD" "curl" "--fail" "--silent" "http://localhost:5984/_up" ];
      };
      rabbitmq = {
        image = imageNames.rabbitmq;
        pull_policy = "never";
        hostname = "rabbitmq";
        environment.RABBITMQ_DEFAULT_USER = "starintel";
        entrypoint = [ "/bin/bash" "-ec" ];
        command = [ ''export RABBITMQ_DEFAULT_PASS="$$(cat /run/secrets/rabbitmq_password)"; exec /usr/local/bin/docker-entrypoint.sh rabbitmq-server'' ];
        secrets = [ "rabbitmq_password" ];
        volumes = [ (mount "rabbitmq" "/var/lib/rabbitmq") ];
        ports = [ "127.0.0.1:5672:5672" "127.0.0.1:15672:15672" ];
        networks = [ "backend" ];
        healthcheck = health [ "CMD" "rabbitmq-diagnostics" "-q" "ping" ];
      };
      valkey = {
        image = imageNames.valkey;
        pull_policy = "never";
        environment.VALKEY_PASSWORD_FILE = secret "valkey_password";
        secrets = [ "valkey_password" ];
        volumes = [ (mount "valkey" "/data") ];
        ports = [ "127.0.0.1:6379:6379" ];
        networks = [ "backend" ];
        healthcheck = health [ "CMD-SHELL" ''VALKEYCLI_AUTH="$$(cat /run/secrets/valkey_password)" valkey-cli ping | grep -q PONG'' ];
      };
    };
    secrets = lib.genAttrs [ "couchdb_password" "couchdb_secret" "erlang_cookie" "rabbitmq_password" "valkey_password" ]
      (name: { file = "${secretDir}/${name}"; });
    networks.backend = { };
  });
  prepare = pkgs.writeShellApplication {
    name = "starintel-prepare";
    runtimeInputs = [ pkgs.coreutils ];
    text = ''
      umask 077
      root=${lib.escapeShellArg root}
      mkdir -p "$root/config/secrets" "$root/logs" "$root/cache" "$root/data/quasar" "$root/data/actors"
      # Infrastructure directories are provisioned explicitly by the operator.
      # Never chown or migrate an existing database during service startup.
      for name in couchdb clouseau rabbitmq valkey; do
        test -d "$root/data/$name" || { echo "Missing infrastructure directory: $root/data/$name" >&2; exit 1; }
      done
      for name in couchdb_password couchdb_secret erlang_cookie rabbitmq_password valkey_password auth_pepper initial_password; do
        file="$root/config/secrets/$name"
        test -s "$file" || { echo "Missing nonempty secret file: $name" >&2; exit 1; }
        mode="$(stat -c %a "$file")"
        if (( (8#$mode & 077) != 0 )); then
          echo "Secret must not be group/world accessible: $name" >&2; exit 1
        fi
      done
      if [[ ! -e "$root/config/init.lisp" ]]; then
        (set -o noclobber; cat ${../files/starintel/init.lisp} > "$root/config/init.lisp")
      fi
      if [[ ! -e "$root/config/quasar-init.lisp" ]]; then
        (set -o noclobber; cat ${../files/starintel/quasar-init.lisp} > "$root/config/quasar-init.lisp")
      fi
    '';
  };
  infrastructure = pkgs.writeShellApplication {
    name = "starintel-infrastructure";
    runtimeInputs = [ pkgs.docker-client pkgs.docker-compose ];
    text = ''
      docker info >/dev/null
      case "$1" in
        start)
          ${lib.concatMapStringsSep "\n" (name: "docker load --input ${image name}") (builtins.attrNames defaultImageNames)}
          docker-compose --project-name ${projectName} --file ${compose} up --detach --wait --wait-timeout 180
          ;;
        stop) docker-compose --project-name ${projectName} --file ${compose} down ;;
        *) echo "Use start or stop" >&2; exit 2 ;;
      esac
    '';
  };
  server = pkgs.writeShellApplication {
    name = "starintel-server-start";
    runtimeInputs = [ pkgs.coreutils ];
    text = ''
      exec ${cfg.packages.server}/bin/star-server start -i ${lib.escapeShellArg "${root}/config/init.lisp"}
    '';
  };
  ready = pkgs.writeShellApplication {
    name = "starintel-wait-ready";
    runtimeInputs = [ pkgs.curl pkgs.coreutils ];
    text = ''
      for _attempt in $(seq 1 60); do
        if curl --fail --silent --max-time 2 http://127.0.0.1:5000/health >/dev/null; then exit 0; fi
        sleep 2
      done
      echo "StarIntel server did not become healthy" >&2
      exit 1
    '';
  };
  appService = description: after: exec: {
    Unit = {
      Description = description;
      Requires = [ "starintel-prepare.service" ] ++ after;
      After = [ "starintel-prepare.service" ] ++ after;
      PartOf = [ "starintel.target" ];
      StartLimitIntervalSec = 120;
      StartLimitBurst = 3;
    };
    Service = {
      ExecStart = exec;
      WorkingDirectory = root;
      UMask = "0077";
      Restart = "on-failure";
      RestartSec = 5;
      Environment = [ "XDG_CACHE_HOME=${root}/cache" "XDG_DATA_HOME=${root}/data" ];
      EnvironmentFile = lib.optional (cfg.environmentFile != null) cfg.environmentFile;
    };
  };
in
{
  options.services.starintel = {
    enable = lib.mkEnableOption "the operator-started, local StarIntel full stack";
    workDir = mkOption {
      type = types.str;
      default = "${config.home.homeDirectory}/.starintel";
      description = "Absolute root for all durable stack state. Changing it does not migrate data.";
    };
    environmentFile = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Optional runtime environment file path; never put secret values in Nix options.";
    };
    packages = {
      server = packageOption "Pinned upstream starintel-server default package.";
      quasar = packageOption "Pinned upstream Quasar quasar-server package, not web-dist.";
      router = packageOption "Pinned upstream starRouter default package.";
    };
    infrastructure = {
      engineReady = mkOption {
        type = types.bool;
        default = false;
        description = "Explicit acknowledgement that an existing Docker engine is available to this user. Does not install or configure it.";
      };
      imageTags = mkOption {
        type = types.attrsOf types.str;
        default = defaultImageNames;
        description = "Exact tags contained in the supplied upstream image archives.";
      };
      images = lib.genAttrs (builtins.attrNames defaultImageNames) (name:
        packageOption "Pinned upstream ${name} image archive; see the documented compatible image tags.");
    };
    actors = mkOption {
      default = { };
      description = "Explicit optional actor services; no actor or private source is installed by default.";
      type = types.attrsOf (types.submodule {
        options = {
          package = mkOption { type = types.package; description = "Operator-selected actor package."; };
          executable = mkOption { type = types.str; description = "Executable name under the package bin directory."; };
          arguments = mkOption { type = types.listOf types.str; default = [ ]; };
          environmentFile = mkOption { type = types.str; description = "Required runtime actor environment file path."; };
        };
      });
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      { assertion = lib.hasPrefix "/" root && !(lib.hasInfix "/../" "${root}/") && !(lib.hasInfix "%" root) && !(lib.hasInfix "\n" root); message = "StarIntel workDir must be an absolute path without parent traversal, systemd specifiers or newlines."; }
      { assertion = builtins.attrNames cfg.infrastructure.imageTags == builtins.attrNames defaultImageNames; message = "StarIntel imageTags must contain exactly couchdb, clouseau, rabbitmq and valkey."; }
      { assertion = cfg.infrastructure.engineReady; message = "StarIntel requires an operator-provisioned Docker engine; set infrastructure.engineReady only after verifying it."; }
      { assertion = lib.all (p: p != null) (builtins.attrValues cfg.packages ++ builtins.attrValues cfg.infrastructure.images); message = "StarIntel requires three explicit upstream application packages and four infrastructure image archives."; }
      { assertion = lib.all (name: builtins.match "[a-zA-Z0-9_-]+" name != null) (builtins.attrNames cfg.actors); message = "StarIntel actor service names may contain only letters, digits, underscores and hyphens."; }
      { assertion = lib.all (actor: builtins.match "[a-zA-Z0-9_.-]+" actor.executable != null) (builtins.attrValues cfg.actors); message = "StarIntel actor executable must be a basename."; }
    ];
    systemd.user.targets.starintel.Unit = {
      Description = "Local StarIntel stack (manual start only)";
      Requires = [ "starintel-server.service" "starintel-quasar.service" "starintel-router.service" ]
        ++ map (name: "starintel-actor-${name}.service") (builtins.attrNames cfg.actors);
      After = [ "starintel-server.service" "starintel-quasar.service" "starintel-router.service" ];
    };
    systemd.user.services = {
      starintel-prepare = {
        Unit = { Description = "Check StarIntel state and preserve operator configuration"; PartOf = [ "starintel.target" ]; };
        Service = { Type = "oneshot"; RemainAfterExit = true; ExecStart = lib.getExe prepare; UMask = "0077"; };
      };
      starintel-infrastructure = {
        Unit = { Description = "StarIntel persistent infrastructure"; Requires = [ "starintel-prepare.service" ]; After = [ "starintel-prepare.service" ]; PartOf = [ "starintel.target" ]; };
        Service = {
          Type = "oneshot";
          RemainAfterExit = true;
          ExecStart = "${lib.getExe infrastructure} start";
          ExecStopPost = "${lib.getExe infrastructure} stop";
          TimeoutStartSec = 600;
          TimeoutStopSec = 120;
        };
      };
      starintel-server = lib.recursiveUpdate (appService "StarIntel server" [ "starintel-infrastructure.service" ] (lib.getExe server)) {
        Service.ExecStartPost = lib.getExe ready;
        Service.TimeoutStartSec = 180;
        Service.Environment = [
          "XDG_CACHE_HOME=${root}/cache" "XDG_DATA_HOME=${root}/data"
          "COUCHDB_HOST=127.0.0.1" "COUCHDB_USER=starintel" "COUCHDB_PASSWORD_FILE=${secretDir}/couchdb_password" "RABBITMQ_ADDRESS=127.0.0.1" "RABBITMQ_USER=starintel" "RABBITMQ_PASSWORD_FILE=${secretDir}/rabbitmq_password"
          "STAR_LEASE_STORE_BACKEND=valkey" "VALKEY_HOST=127.0.0.1" "VALKEY_PORT=6379" "VALKEY_PASSWORD_FILE=${secretDir}/valkey_password"
          "STAR_AUTH_INITIAL_PASSWORD_FILE=${secretDir}/initial_password" "STAR_AUTH_MODE=api-key" "STAR_AUTH_PEPPER_FILE=${secretDir}/auth_pepper" "STAR_AUTH_BOOTSTRAP_SECRET_FILE=${secretDir}/auth_bootstrap_secret"
        ];
      };
      starintel-quasar = lib.recursiveUpdate (appService "Quasar workspace" [ "starintel-server.service" ] "${cfg.packages.quasar}/bin/quasar-server") {
        Service.Environment = [ "XDG_CACHE_HOME=${root}/cache" "XDG_DATA_HOME=${root}/data" "QUASAR_INIT_FILE=${root}/config/quasar-init.lisp" "QUASAR_STORAGE_PATH=${root}/data/quasar/tek9/" "QUASAR_HOST=127.0.0.1" "QUASAR_HTTP_PORT=8080" "QUASAR_WS_PORT=8081" ];
      };
      starintel-router = lib.recursiveUpdate (appService "StarIntel legacy ZeroMQ router" [ ] "${cfg.packages.router}/bin/starRouter --pubAddress=tcp://127.0.0.1:6000 --apiAddress=tcp://127.0.0.1:6001") {
        Service.Environment = [ "FEDIWATCH_LOG=${root}/logs/starRouter.log" "ROUTER_LOG_LEVEL=lvlInfo" ];
      };
    } // lib.mapAttrs' (name: actor:
      let runner = pkgs.writeShellApplication {
        name = "starintel-actor-${name}";
        runtimeInputs = [ pkgs.coreutils ];
        text = ''
          mkdir -p ${lib.escapeShellArg "${root}/data/actors/${name}"} ${lib.escapeShellArg "${root}/cache/actors/${name}"}
          cd ${lib.escapeShellArg "${root}/data/actors/${name}"}
          exec ${actor.package}/bin/${actor.executable} ${lib.escapeShellArgs actor.arguments}
        '';
      }; in lib.nameValuePair "starintel-actor-${name}"
      (lib.recursiveUpdate (appService "StarIntel actor ${name}" [ "starintel-server.service" ]
        (lib.getExe runner)) {
          Service = {
            EnvironmentFile = actor.environmentFile;
            Environment = [ "XDG_DATA_HOME=${root}/data/actors/${name}" "XDG_CACHE_HOME=${root}/cache/actors/${name}" ];
          };
        })) cfg.actors;
  };
}
