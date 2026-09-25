% GENERATED FILE. DO NOT EDIT.
% Source of truth: .zara/experts/*/kb/expert.pl
% Regenerate with: python3 scripts/generate-zara-android-experts.py
%
% Android's Git-template workspace is intentionally data-only. This
% projection exposes identity and local explain routing for canonical
% zero-model experts without creating another ZARA-EXPERT registry.

android_expert_source(bash, 'zara:expert/bash', '.zara/experts/bash').
expert_activation(bash, bash).
bash_explain(Entity, Result) :-
    Result = expert_projection(
        id('zara:expert/bash'),
        entity(Entity),
        source('.zara/experts/bash'),
        policy(pure_symbolic),
        model_calls(0)
    ).

android_expert_source(common_lisp, 'zara:expert/common-lisp', '.zara/experts/common-lisp').
expert_activation(common_lisp, common_lisp).
common_lisp_explain(Entity, Result) :-
    Result = expert_projection(
        id('zara:expert/common-lisp'),
        entity(Entity),
        source('.zara/experts/common-lisp'),
        policy(pure_symbolic),
        model_calls(0)
    ).

android_expert_source(dotfiles, 'zara:expert/dotfiles', '.zara/experts/dotfiles').
expert_activation(dotfiles, dotfiles).
dotfiles_explain(Entity, Result) :-
    Result = expert_projection(
        id('zara:expert/dotfiles'),
        entity(Entity),
        source('.zara/experts/dotfiles'),
        policy(pure_symbolic),
        model_calls(0)
    ).

android_expert_source(emacs_lisp, 'zara:expert/emacs-lisp', '.zara/experts/emacs-lisp').
expert_activation(emacs_lisp, emacs_lisp).
emacs_lisp_explain(Entity, Result) :-
    Result = expert_projection(
        id('zara:expert/emacs-lisp'),
        entity(Entity),
        source('.zara/experts/emacs-lisp'),
        policy(pure_symbolic),
        model_calls(0)
    ).

android_expert_source(java, 'zara:expert/java', '.zara/experts/java').
expert_activation(java, java).
java_explain(Entity, Result) :-
    Result = expert_projection(
        id('zara:expert/java'),
        entity(Entity),
        source('.zara/experts/java'),
        policy(pure_symbolic),
        model_calls(0)
    ).

android_expert_source(javascript, 'zara:expert/javascript', '.zara/experts/javascript').
expert_activation(javascript, javascript).
javascript_explain(Entity, Result) :-
    Result = expert_projection(
        id('zara:expert/javascript'),
        entity(Entity),
        source('.zara/experts/javascript'),
        policy(pure_symbolic),
        model_calls(0)
    ).

android_expert_source(kotlin, 'zara:expert/kotlin', '.zara/experts/kotlin').
expert_activation(kotlin, kotlin).
kotlin_explain(Entity, Result) :-
    Result = expert_projection(
        id('zara:expert/kotlin'),
        entity(Entity),
        source('.zara/experts/kotlin'),
        policy(pure_symbolic),
        model_calls(0)
    ).

android_expert_source(lisp, 'zara:expert/lisp', '.zara/experts/lisp').
expert_activation(lisp, lisp).
lisp_explain(Entity, Result) :-
    Result = expert_projection(
        id('zara:expert/lisp'),
        entity(Entity),
        source('.zara/experts/lisp'),
        policy(pure_symbolic),
        model_calls(0)
    ).

android_expert_source(nim, 'zara:expert/nim', '.zara/experts/nim').
expert_activation(nim, nim).
nim_explain(Entity, Result) :-
    Result = expert_projection(
        id('zara:expert/nim'),
        entity(Entity),
        source('.zara/experts/nim'),
        policy(pure_symbolic),
        model_calls(0)
    ).

android_expert_source(nix, 'zara:expert/nix', '.zara/experts/nix').
expert_activation(nix, nix).
nix_explain(Entity, Result) :-
    Result = expert_projection(
        id('zara:expert/nix'),
        entity(Entity),
        source('.zara/experts/nix'),
        policy(pure_symbolic),
        model_calls(0)
    ).

android_expert_source(prolog, 'zara:expert/prolog', '.zara/experts/prolog').
expert_activation(prolog, prolog).
prolog_explain(Entity, Result) :-
    Result = expert_projection(
        id('zara:expert/prolog'),
        entity(Entity),
        source('.zara/experts/prolog'),
        policy(pure_symbolic),
        model_calls(0)
    ).

android_expert_source(python, 'zara:expert/python', '.zara/experts/python').
expert_activation(python, python).
python_explain(Entity, Result) :-
    Result = expert_projection(
        id('zara:expert/python'),
        entity(Entity),
        source('.zara/experts/python'),
        policy(pure_symbolic),
        model_calls(0)
    ).

android_expert_source(style, 'zara:expert/style', '.zara/experts/style').
expert_activation(style, style).
style_explain(Entity, Result) :-
    Result = expert_projection(
        id('zara:expert/style'),
        entity(Entity),
        source('.zara/experts/style'),
        policy(pure_symbolic),
        model_calls(0)
    ).

android_expert_source(typescript, 'zara:expert/typescript', '.zara/experts/typescript').
expert_activation(typescript, typescript).
typescript_explain(Entity, Result) :-
    Result = expert_projection(
        id('zara:expert/typescript'),
        entity(Entity),
        source('.zara/experts/typescript'),
        policy(pure_symbolic),
        model_calls(0)
    ).
