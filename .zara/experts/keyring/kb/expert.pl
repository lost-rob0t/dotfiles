:- module(dotfiles_keyring_expert,
    [ expert_id/1,
      upstream_contract/1,
      keyring_policy/1,
      keyring_backend/1,
      keyring_backend_surface/2,
      keyring_provider/2,
      keyring_platform/1,
      backend_platform/2,
      probe_spec/2,
      lookup_argv/5,
      store_argv/7,
      store_value_channel/2,
      select_keyring/3,
      safe_keyring_atom/1,
      emit_probes/1,
      emit_pairing_plan/7
    ]).

% Platform keyring-service enumeration and selection policy for Zara client
% pairing. This expert owns only symbolic knowledge: which keyring services
% exist per platform, which provider daemons implement them, how availability
% is probed, and how secret lookup/store commands are shaped. All I/O,
% secrets, and session state live in the caller.
%
% The pairing client defers every platform/backend decision to this module
% and must fail closed when no backend is selected: there is no plaintext
% fallback policy.

expert_id('zara:expert/keyring').
upstream_contract('lost-rob0t/dotfiles#zara-pair').

keyring_policy(fail_closed).
keyring_policy(no_plaintext_fallback).
keyring_policy(probe_before_selection).

% Enumerated keyring services, not CLI tools.
keyring_backend(freedesktop_secrets).
keyring_backend(macos_keychain).
keyring_backend(windows_credential_manager).
keyring_backend(android_keystore).

% Windows Credential Manager (cmdkey writes but cannot read back without the
% CredRead API) and Android Keystore (in-process app API) are enumerated for
% completeness but are not selectable through a CLI pairing tool.
keyring_backend_surface(freedesktop_secrets, cli).
keyring_backend_surface(macos_keychain, cli).
keyring_backend_surface(windows_credential_manager, app_api).
keyring_backend_surface(android_keystore, app_api).

% Provider daemons behind the D-Bus secrets protocol are interchangeable at
% the wire level; the probe is bus-name ownership, never provider identity.
keyring_provider(freedesktop_secrets, gnome_keyring).
keyring_provider(freedesktop_secrets, kwalletd6).

keyring_platform(linux).
keyring_platform(darwin).
keyring_platform(windows).
keyring_platform(android).

backend_platform(freedesktop_secrets, linux).
backend_platform(macos_keychain, darwin).
backend_platform(windows_credential_manager, windows).
backend_platform(android_keystore, android).

% Availability facts the caller must measure before selection. Rendered as
% generic probe requests; the expert never performs the probe itself. Tool
% names are literal executable names, not abstract probe labels.
probe_spec(freedesktop_secrets, bus_owner('org.freedesktop.secrets')).
probe_spec(freedesktop_secrets, cli('secret-tool')).
probe_spec(macos_keychain, cli(security)).

% Secret value transport: freedesktop secret-tool reads the value from stdin;
% the macOS security CLI carries it as an argv element.
store_value_channel(freedesktop_secrets, stdin).
store_value_channel(macos_keychain, argv).

% One keyring item per secret field keeps every stored value single-line so
% argv-channel backends never need multi-line values. The macOS keychain has
% only service/account slots, so the field is composed into the account name.
% secret-tool takes attributes as alternating key/value argv elements.
macos_account(Account, Field, Compound) :-
    format(atom(Compound), "~a:~a", [Account, Field]).

lookup_argv(freedesktop_secrets, Service, Account, Field,
            ['secret-tool', lookup, service, Service, account, Account, field, Field]).
lookup_argv(macos_keychain, Service, Account, Field,
            ['security', find-generic-password, '-s', Service,
             '-a', Compound, '-w']) :-
    macos_account(Account, Field, Compound).

store_argv(freedesktop_secrets, Service, Account, Field, Label, _Value,
           ['secret-tool', store, '--label', Label,
            service, Service, account, Account, field, Field]).
store_argv(macos_keychain, Service, Account, Field, Label, Value,
           ['security', add-generic-password, '-U',
            '-s', Service, '-a', Compound, '-l', Label, '-w', Value]) :-
    macos_account(Account, Field, Compound).

% Ordered, ground probe results select at most one CLI backend per platform.
% Selection fails (closed) unless every probe for the backend is satisfied.
select_keyring(Platform, Results, Backend) :-
    ground(Platform),
    ground(Results),
    keyring_backend(Backend),
    backend_platform(Backend, Platform),
    keyring_backend_surface(Backend, cli),
    backend_probes_satisfied(Backend, Results).

backend_probes_satisfied(Backend, Results) :-
    forall(probe_spec(Backend, Probe), probe_satisfied(Probe, Results)).

probe_satisfied(bus_owner(Bus), Results) :-
    memberchk(bus_owner(Bus, true), Results).
probe_satisfied(cli(Tool), Results) :-
    memberchk(cli(Tool, present), Results).

% Line-oriented emission for the pairing client. Atoms crossing this boundary
% are restricted to a whitespace-free, quote-free charset (superset of Z85 so
% raw CURVE key material is transportable on argv-channel backends) and no
% quoting layer is needed.
safe_keyring_code(Code) :- between(0'a, 0'z, Code).
safe_keyring_code(Code) :- between(0'A, 0'Z, Code).
safe_keyring_code(Code) :- between(0'0, 0'9, Code).
safe_keyring_code(0'_).
safe_keyring_code(0'-).
safe_keyring_code(0'.).
safe_keyring_code(0'=).
safe_keyring_code(0':).
safe_keyring_code(0'+).
safe_keyring_code(0'^).
safe_keyring_code(0'!).
safe_keyring_code(0'/).
safe_keyring_code(0'*).
safe_keyring_code(0'?).
safe_keyring_code(0'&).
safe_keyring_code(0'<).
safe_keyring_code(0'>).
safe_keyring_code(0'().
safe_keyring_code(0')).
safe_keyring_code(0'[).
safe_keyring_code(0']).
safe_keyring_code(0'{).
safe_keyring_code(0'}).
safe_keyring_code(0'@).
safe_keyring_code(0'%).
safe_keyring_code(0'$).
safe_keyring_code(0'#).

safe_keyring_atom(Atom) :-
    atom(Atom),
    atom_codes(Atom, Codes),
    Codes \= [],
    maplist(safe_keyring_code, Codes),
    !.

emit_probes(Platform) :-
    forall(
        ( backend_platform(Backend, Platform), probe_spec(Backend, Probe) ),
        emit_probe(Probe)
    ).

emit_probe(bus_owner(Bus)) :- format("PROBE bus_owner ~a~n", [Bus]).
emit_probe(cli(Tool)) :- format("PROBE cli ~a~n", [Tool]).

emit_pairing_plan(Platform, Results, Service, Account, Field, Label, Value) :-
    (   \+ safe_keyring_atom(Service)
    ;   \+ safe_keyring_atom(Account)
    ;   \+ safe_keyring_atom(Field)
    ;   \+ safe_keyring_atom(Label)
    ;   \+ safe_keyring_atom(Value)
    ) -> !, format("error unsafe_atom~n")
    ;   (   select_keyring(Platform, Results, Backend)
        ->  emit_selected_plan(Backend, Service, Account, Field, Label, Value)
        ;   format("backend none~n")
        ).

emit_selected_plan(Backend, Service, Account, Field, Label, Value) :-
    format("backend ~a~n", [Backend]),
    (   store_value_channel(Backend, Channel)
    ->  format("value_channel ~a~n", [Channel])
    ;   format("error no_value_channel~n")
    ),
    (   lookup_argv(Backend, Service, Account, Field, Argv)
    ->  emit_args(lookup_arg, Argv)
    ;   format("error no_lookup_argv~n")
    ),
    (   store_argv(Backend, Service, Account, Field, Label, Value, StoreArgv)
    ->  emit_args(store_arg, StoreArgv)
    ;   format("error no_store_argv~n")
    ).

emit_args(Tag, Argv) :-
    maplist(safe_keyring_atom, Argv),
    !,
    forall(member(Arg, Argv), format("~a ~a~n", [Tag, Arg])).
emit_args(Tag, _) :-
    format("error unsafe_atom ~a~n", [Tag]).
