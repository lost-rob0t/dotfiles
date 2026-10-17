:- module(dotfiles_symbolic_chat_topics, [topic/3, phrase_topic/2]).

topic(local_ai,
    "Local AI runs an installed model on this device. Import a compatible LiteRT-LM bundle in Settings, then select Local AI. Once installed, its chat turns do not need a network connection.",
    'wiki/android-local-ai.org').
topic(remote_ai,
    "Select Remote in Settings and configure an enabled API provider there. A Remote chat turn sends its request to that provider over the network.",
    'wiki/android-local-ai.org').
topic(symbolic_mode,
    "Pure symbolic mode uses Prolog with zero model and provider calls. It can answer known intents and inspect facts; device actions still require Zara's normal capability and approval path.",
    'modules/symbolic_dialogue.pl').
topic(prolog_studio,
    "Open Logic, choose a source in Prolog Studio, edit its rules, then tap Save & reload. Query the active runtime to check the result.",
    'wiki/android-prolog-studio.org').
topic(prolog_query,
    "Open Logic and enter a Prolog query ending in a period. Run it against the loaded local knowledge base and inspect the returned terms.",
    'wiki/android-prolog-studio.org').
topic(model_import,
    "Open Settings, Runtime & local AI, and import a compatible LiteRT-LM model file from device storage. Review its format, backend, and storage before activation.",
    'wiki/android-local-ai.org').
topic(model_unload,
    "Open Settings, Runtime & local AI, then use Unload from memory. The imported file remains installed and can be activated again with Use.",
    'wiki/android-local-ai.org').
topic(model_privacy,
    "Local AI inference stays on this device. Remote mode contacts the selected provider. Choose the mode that matches where you want a chat turn processed.",
    'wiki/android-local-ai.org').
topic(mode_switch,
    "Use Settings to choose Local AI, Remote, or a connected Zara server. Pure symbolic chat is an explicit execution policy and keeps model and provider calls at zero.",
    'wiki/android-local-ai.org').
topic(symbolic_unknown,
    "When no Prolog rule resolves a question, pure symbolic chat reports that it has no deterministic answer. It does not silently ask a model or remote provider.",
    'modules/symbolic_dialogue_turn.pl').
topic(symbolic_actions,
    "A symbolic frame describes an intended action. Zara reports success only after its registered capability executes and a verified result is available.",
    'modules/symbolic_dialogue.pl').
topic(prolog_safety,
    "The app checks editable Prolog sources before reloading. A rule does not grant Android permissions or bypass Zara's action approvals.",
    'wiki/android-prolog-studio.org').

phrase_topic(Words, local_ai) :- contains(Words, [local, ai]), contains_any(Words, [offline, internet, network]).
phrase_topic(Words, remote_ai) :- contains_any(Words, [remote, api, provider]), contains_any(Words, [use, configure, setup, connect]).
phrase_topic(Words, symbolic_mode) :- contains(Words, [symbolic, mode]), contains_any(Words, [do, work, mean]).
phrase_topic(Words, prolog_studio) :- contains_any(Words, [edit, change, write]), contains_any(Words, [prolog, rules, logic]).
phrase_topic(Words, prolog_query) :- contains(Words, [prolog, query]).
phrase_topic(Words, model_import) :- contains_any(Words, [import, install]), contains_any(Words, [model, litert]).
phrase_topic(Words, model_unload) :- contains_any(Words, [unload, remove]), contains(Words, [model]).
phrase_topic(Words, model_privacy) :- contains_any(Words, [privacy, private, data]), contains_any(Words, [model, local, remote]).
phrase_topic(Words, mode_switch) :- contains_any(Words, [switch, choose, change]), contains(Words, [mode]).
phrase_topic(Words, symbolic_unknown) :- contains_any(Words, [unknown, unsupported, fallback]), contains(Words, [symbolic]).
phrase_topic(Words, symbolic_actions) :- contains_any(Words, [action, actions, execute]), contains(Words, [symbolic]).
phrase_topic(Words, prolog_safety) :- contains_any(Words, [permission, permissions, safe, approval]), contains_any(Words, [prolog, rule, rules]).

contains(Words, Required) :- append(_, Tail, Words), append(Required, _, Tail), !.
contains_any(Words, Choices) :- member(Word, Words), member(Word, Choices), !.
