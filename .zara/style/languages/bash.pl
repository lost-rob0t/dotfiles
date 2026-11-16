:- module(dotfiles_bash_style,
    [ style_revision/1,
      style_rule/3,
      style_provenance/4
    ]).

style_revision('dotfiles-bash-style-v1').

style_rule(inspection, source_is_inert_data, true).
style_rule(inspection, source_before_analysis, false).
style_rule(safety, quote_expansions_by_default, true).
style_rule(verification, repair_success_requires_fresh_parser_evidence, true).

style_provenance(inspection,
                 '.zara/experts/bash/kb/expert.pl',
                 canonical_expert,
                 'dotfiles-bash-style-v1').
style_provenance(safety,
                 '.zara/experts/bash/kb/expert.pl',
                 canonical_expert,
                 'dotfiles-bash-style-v1').
style_provenance(verification,
                 '.zara/experts/bash/kb/expert.pl',
                 canonical_expert,
                 'dotfiles-bash-style-v1').
