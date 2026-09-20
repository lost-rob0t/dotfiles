:- module(dotfiles_bash_expert,
    [ expert_id/1,
      upstream_contract/1,
      accepts_extension/1,
      supports_semantic/1,
      startup_file/2,
      inspection_policy/1,
      source_execution_policy/1,
      parser_probe/2,
      repair_verification/1,
      provider_policy/1,
      max_model_calls/1,
      model_calls/1
    ]).

expert_id('zara:expert/bash').
upstream_contract('lost-rob0t/prolog-rlm#502').

accepts_extension(sh).
accepts_extension(bash).

supports_semantic(parse).
supports_semantic(diagnose).
supports_semantic(quoting).
supports_semantic(word_splitting).
supports_semantic(globbing).
supports_semantic(arrays).
supports_semantic(redirection).
supports_semantic(pipelines).
supports_semantic(functions).
supports_semantic(command_substitution).
supports_semantic(process_substitution).
supports_semantic(heredocs).
supports_semantic(startup_source_graph).
supports_semantic(style).
supports_semantic(repair_verify).

startup_file('.bash_profile', login_shell).
startup_file('.profile', login_shell_fallback).
startup_file('.bashrc', interactive_nonlogin).

% Read-only inspection parses inert source data. It never sources or executes it.
inspection_policy(parse_only).
source_execution_policy(never).
parser_probe(bash_n, read_only).

% A repair cannot be called successful until fresh parser evidence exists.
repair_verification(parse_and_bash_n).

provider_policy(disabled).
max_model_calls(0).
model_calls(0).
