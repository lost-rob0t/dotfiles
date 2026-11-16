:- module(dotfiles_nix_style,
    [ style_revision/1,
      style_rule/3,
      style_provenance/4
    ]).

style_revision('dotfiles-nix-style-v1').

style_rule(formatting, formatter, 'nixfmt-rfc-style').
style_rule(inspection, implicit_evaluation, false).
style_rule(inspection, implicit_build, false).
style_rule(verification, repair_success_requires_fresh_parser_evidence, true).

style_provenance(formatting,
                 '.prolog/kb/literate_sync.pl',
                 durable_kb,
                 'dotfiles-nix-style-v1').
style_provenance(inspection,
                 '.zara/experts/nix/kb/expert.pl',
                 canonical_expert,
                 'dotfiles-nix-style-v1').
style_provenance(verification,
                 '.zara/experts/nix/kb/expert.pl',
                 canonical_expert,
                 'dotfiles-nix-style-v1').
