:- use_module(library(plunit)).
:- use_module('../kb/expert').

:- begin_tests(dotfiles_sysadmin_expert, [setup(reset_facts), cleanup(reset_facts)]).
test(failed_service_chain) :-
    observe(service_active, false),
    observe(service_result, exit_code),
    hypothesis(service_failed),
    next_diagnostic(service_failed, inspect_recent_journal),
    verification(service_failed, service_status_after_remediation).
test(listener_chain) :-
    observe(service_active, true),
    observe(port_listening, false),
    hypothesis(running_without_listener),
    next_diagnostic(running_without_listener, inspect_socket_or_service_config).
test(dns_chain) :-
    observe(resolver_configured, true),
    observe(default_route_present, true),
    observe(dns_upstream_reachable, false),
    hypothesis(dns_upstream_failure).
test(no_false_service_failure, [fail]) :-
    observe(service_active, true),
    observe(service_result, success),
    hypothesis(service_failed).
:- end_tests(dotfiles_sysadmin_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
