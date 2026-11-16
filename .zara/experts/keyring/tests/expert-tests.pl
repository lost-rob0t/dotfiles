:- use_module(library(plunit)).
:- use_module('../kb/expert').

:- begin_tests(dotfiles_keyring_expert).

test(identity_and_policy_fail_closed) :-
    expert_id('zara:expert/keyring'),
    keyring_policy(fail_closed),
    keyring_policy(no_plaintext_fallback).

test(every_backend_has_exactly_one_platform) :-
    forall(
        keyring_backend(Backend),
        ( backend_platform(Backend, _),
          \+ ( backend_platform(Backend, P1),
               backend_platform(Backend, P2),
               P1 \== P2 )
        )
    ).

test(every_platform_enumerates_at_least_one_backend) :-
    forall(
        keyring_platform(Platform),
        backend_platform(_, Platform)
    ).

test(cli_backends_have_commands_and_value_channel) :-
    forall(
        ( keyring_backend_surface(Backend, cli) ),
        ( lookup_argv(Backend, 'zara-daemon', 'desktop-flake',
                      'curve_public_key', _),
          store_argv(Backend, 'zara-daemon', 'desktop-flake',
                     'curve_public_key', 'zara-daemon-client', 'PLACEHOLDER', _),
          store_value_channel(Backend, _)
        )
    ).

test(api_only_backends_are_never_selected) :-
    \+ keyring_backend_surface(windows_credential_manager, cli),
    \+ keyring_backend_surface(android_keystore, cli).

test(linux_selects_freedesktop_secrets_when_probes_pass) :-
    Results = [bus_owner('org.freedesktop.secrets', true),
               cli('secret-tool', present)],
    select_keyring(linux, Results, freedesktop_secrets).

test(selection_fails_closed_without_secrets_bus, [fail]) :-
    Results = [bus_owner('org.freedesktop.secrets', false),
               cli('secret-tool', present)],
    select_keyring(linux, Results, _).

test(selection_fails_closed_without_cli_tool, [fail]) :-
    Results = [bus_owner('org.freedesktop.secrets', true),
               cli('secret-tool', absent)],
    select_keyring(linux, Results, _).

test(selection_fails_closed_with_no_probe_results, [fail]) :-
    select_keyring(linux, [], _).

test(darwin_selects_keychain_via_security_cli) :-
    Results = [cli(security, present)],
    select_keyring(darwin, Results, macos_keychain).

test(no_backend_is_selected_on_a_platform_without_one, [fail]) :-
    select_keyring(darwin, [bus_owner('org.freedesktop.secrets', true),
                            cli('secret-tool', present)], _).

test(probes_are_provider_agnostic_on_linux) :-
    findall(Probe, probe_spec(freedesktop_secrets, Probe), Probes),
    Probes == [bus_owner('org.freedesktop.secrets'), cli('secret-tool')].

test(both_linux_providers_serve_the_same_bus_protocol) :-
    keyring_provider(freedesktop_secrets, gnome_keyring),
    keyring_provider(freedesktop_secrets, kwalletd6).

test(freedesktop_lookup_argv_carries_alternating_attribute_pairs) :-
    lookup_argv(freedesktop_secrets, 'zara-daemon', 'desktop-flake',
                'curve_secret_key', Argv),
    Argv = ['secret-tool', lookup | Attrs],
    pairs_in_sequence(Attrs,
                      [service-'zara-daemon',
                       account-'desktop-flake',
                       field-'curve_secret_key']).

pairs_in_sequence([], []).
pairs_in_sequence([K, V | Rest], [K-V | Pairs]) :-
    pairs_in_sequence(Rest, Pairs).

test(macos_composes_the_field_into_the_account_slot) :-
    lookup_argv(macos_keychain, 'zara-daemon', 'desktop-flake',
                'curve_public_key', Argv),
    memberchk('desktop-flake:curve_public_key', Argv),
    Argv = ['security', find-generic-password | _].

test(macos_store_value_travels_on_argv) :-
    store_value_channel(macos_keychain, argv),
    store_argv(macos_keychain, 'zara-daemon', 'desktop-flake',
               'curve_public_key', 'zara-daemon-client', 'SECRET', Argv),
    last(Argv, 'SECRET').

test(freedesktop_store_value_travels_on_stdin) :-
    store_value_channel(freedesktop_secrets, stdin),
    store_argv(freedesktop_secrets, 'zara-daemon', 'desktop-flake',
               'curve_public_key', 'zara-daemon-client', 'IGNORED', Argv),
    \+ member('IGNORED', Argv).

test(unsafe_atoms_are_rejected) :-
    must_be_rejected("has space"),
    must_be_rejected("quote'"),
    must_be_rejected("new\nline"),
    must_be_rejected("").

must_be_rejected(Text) :-
    atom_codes(Atom, Text),
    \+ safe_keyring_atom(Atom),
    !.

test(safe_atoms_are_accepted) :-
    safe_keyring_atom('zara-daemon'),
    safe_keyring_atom('desktop-flake.v2_2026'),
    safe_keyring_atom('service=account=').

test(z85_key_material_is_transport_safe) :-
    z85_like('PnQ0QyyPfUPQVG1F5pF0ige'),
    z85_like('4DdrR(icCmoAx3WhlJ2IDk'),
    z85_like('{?fLrR}Y<yv+DAhOHZ-%0j&fkBidgBp:Ek-C2H1').

z85_like(Atom) :- safe_keyring_atom(Atom).

:- end_tests(dotfiles_keyring_expert).
