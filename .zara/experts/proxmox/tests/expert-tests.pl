:- use_module(library(plunit)).
:- use_module('../kb/expert').

:- begin_tests(dotfiles_proxmox_expert).

test(identity_and_zero_model_policy) :-
    expert_id('zara:expert/proxmox'),
    provider_policy(disabled),
    max_model_calls(0),
    model_calls(0).

test(read_operations_need_no_effect_approval) :-
    proxmox_decision(cluster_inventory,
        proxmox_decision(
            expert('zara:expert/proxmox'),
            operation(cluster_inventory),
            class(observation),
            tool('sysadmin.proxmox.inventory'),
            approval(none),
            postcondition(fresh_observation),
            credentials([environment, wallet, auth_source]),
            provider_policy(disabled),
            model_calls(0)
        )).

test(guest_mutation_requires_approval_and_postcondition) :-
    proxmox_decision(guest_reboot,
        proxmox_decision(
            expert('zara:expert/proxmox'),
            operation(guest_reboot),
            class(mutation),
            tool('sysadmin.proxmox.guest_action'),
            approval(required),
            postcondition(fresh_postcondition),
            credentials([environment, wallet, auth_source]),
            provider_policy(disabled),
            model_calls(0)
        )).

test(destroy_is_explicit_operator_only) :-
    operation_class(guest_destroy, destructive),
    approval_policy(destructive, explicit_operator_only).

test(secret_sources_are_closed) :-
    credential_source_allowed(environment),
    credential_source_allowed(wallet),
    credential_source_allowed(auth_source).

test(checked_in_secret_source_is_rejected, [fail]) :-
    credential_source_allowed(config_file_literal).

test(unknown_operation_fails_closed, [fail]) :-
    proxmox_decision(format_cluster, _).

:- end_tests(dotfiles_proxmox_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
