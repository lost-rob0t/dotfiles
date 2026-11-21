:- module(dotfiles_proxmox_expert,
    [ expert_id/1,
      operation_class/2,
      approval_policy/2,
      postcondition_policy/2,
      credential_source_allowed/1,
      tool_route/2,
      proxmox_decision/2,
      provider_policy/1,
      max_model_calls/1,
      model_calls/1
    ]).

expert_id('zara:expert/proxmox').

operation_class(cluster_inventory, observation).
operation_class(node_status, observation).
operation_class(guest_status, observation).
operation_class(storage_status, observation).
operation_class(guest_start, mutation).
operation_class(guest_stop, mutation).
operation_class(guest_reboot, mutation).
operation_class(snapshot_create, mutation).
operation_class(migrate, mutation).
operation_class(guest_destroy, destructive).

approval_policy(observation, none).
approval_policy(mutation, required).
approval_policy(destructive, explicit_operator_only).

postcondition_policy(observation, fresh_observation).
postcondition_policy(mutation, fresh_postcondition).
postcondition_policy(destructive, fresh_postcondition_and_audit).

credential_source_allowed(environment).
credential_source_allowed(wallet).
credential_source_allowed(auth_source).

tool_route(cluster_inventory, 'sysadmin.proxmox.inventory').
tool_route(node_status, 'sysadmin.proxmox.node_status').
tool_route(guest_status, 'sysadmin.proxmox.guest_status').
tool_route(storage_status, 'sysadmin.proxmox.storage_status').
tool_route(guest_start, 'sysadmin.proxmox.guest_action').
tool_route(guest_stop, 'sysadmin.proxmox.guest_action').
tool_route(guest_reboot, 'sysadmin.proxmox.guest_action').
tool_route(snapshot_create, 'sysadmin.proxmox.snapshot').
tool_route(migrate, 'sysadmin.proxmox.migrate').
tool_route(guest_destroy, 'sysadmin.proxmox.destroy').

proxmox_decision(Operation,
    proxmox_decision(
        expert('zara:expert/proxmox'),
        operation(Operation),
        class(Class),
        tool(Tool),
        approval(Approval),
        postcondition(Postcondition),
        credentials([environment, wallet, auth_source]),
        provider_policy(disabled),
        model_calls(0)
    )) :-
    operation_class(Operation, Class),
    tool_route(Operation, Tool),
    approval_policy(Class, Approval),
    postcondition_policy(Class, Postcondition).

% Literal credentials are deliberately absent from the vocabulary. This brain
% does not read secrets and never calls a model/provider.
provider_policy(disabled).
max_model_calls(0).
model_calls(0).
