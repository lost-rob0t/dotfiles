%% Deterministic corpus builder for the quasar-ui expert.
%%
%% Reads the operator-journey Playwright specs straight out of a quasar
%% checkout (explicit path argument, read-only) and derives journey facts:
%%
%%   journey(SpecBase, Header, Titles)
%%
%% Header is the leading block comment of the spec (the journey
%% description); Titles are the test titles in file order. No model calls,
%% no network, no writes outside the asserted dynamic predicate.
:- module(quasar_ui_corpus,
    [ parse_spec_source/2,   % +Lines, -journey(Header, Titles)
      build_corpus/2,        % +QuasarRoot, -Journeys
      unmapped_specs/3,      % +QuasarRoot, +MappedSpecs, -Unmapped (audit)
      load_corpus/1,         % +QuasarRoot (asserts journey/3, resets first)
      journey/3, reset_corpus/0 ]).

:- use_module(library(readutil)).
:- dynamic journey/3.

reset_corpus :- retractall(journey(_, _, _)).

%% parse_spec_source(+Lines, -journey(Header, Titles)): pure parser over
%% the lines of one .spec.ts file. Header = the leading run of comment
%% (and blank) lines before the first code line; titles = the quoted names
%% of top-level test("...") declarations, in order.
parse_spec_source(Lines, journey(Header, Titles)) :-
    take_header(Lines, HeaderLines, _Rest),
    atomic_list_concat(HeaderLines, ' ', Header),
    findall(Title, ( member(Line, Lines), line_title(Line, Title) ), Titles).

take_header([], [], []).
take_header([Line | Rest], [Text | Header], Codes) :-
    line_kind(Line, comment, Text), !,
    take_header(Rest, Header, Codes).
take_header([Line | Rest], Header, Codes) :-
    line_kind(Line, blank, _), !,
    take_header(Rest, Header, Codes).
take_header([Line | Rest], [], [Line | Rest]).

%% line_kind(+Line, ?Kind, ?Text)
line_kind(Line, comment, Text) :-
    split_string(Line, "", " \t", [Trimmed]),
    sub_string(Trimmed, 0, 2, _, "//"),
    sub_string(Trimmed, 2, _, 0, After),
    ( sub_string(After, 0, 1, _, " ") ->
        sub_string(After, 1, _, 0, Text)
    ;   After = Text
    ).
line_kind(Line, blank, "") :-
    split_string(Line, "", " \t", []).

%% line_title(+Line, -Title): succeeds when the line declares a test whose
%% title is a simple double-quoted string. The word directly before the
%% opening quote must be exactly "test(".
line_title(Line, Title) :-
    split_string(Line, "\"", "", [Prefix, Title | _]),
    split_string(Prefix, " ", " \t", PrefixWords),
    last(PrefixWords, "test(").

%% build_corpus(+QuasarRoot, -Journeys): scan <Root>/frontend/e2e/*.spec.ts
%% in sorted order; specs without any test title are excluded.
build_corpus(QuasarRoot, Journeys) :-
    format(atom(Pattern), '~a/frontend/e2e/*.spec.ts', [QuasarRoot]),
    expand_file_name(Pattern, Files),
    Files \= [],
    maplist(spec_journey, Files, Journeys0),
    exclude(empty_journey, Journeys0, Journeys).

spec_journey(File, journey(SpecBase, Header, Titles)) :-
    file_base_name(File, SpecFile),
    atom_concat(SpecBase, '.spec.ts', SpecFile),
    read_file_to_string(File, Source, []),
    split_string(Source, "\n", "\r", Lines),
    parse_spec_source(Lines, journey(Header, Titles)).

empty_journey(journey(_, _, [])).

%% unmapped_specs(+QuasarRoot, +MappedSpecs, -Unmapped): coverage audit -
%% spec files present in the checkout but absent from the caller's mapped
%% set (the expert's surface_spec/2 registry). Empty when the registry is
%% complete; the expert README documents the call.
unmapped_specs(QuasarRoot, MappedSpecs, Unmapped) :-
    format(atom(Pattern), '~a/frontend/e2e/*.spec.ts', [QuasarRoot]),
    expand_file_name(Pattern, Files),
    maplist(file_base_name, Files, OnDisk0),
    sort(OnDisk0, OnDisk),
    sort(MappedSpecs, Mapped),
    include({Mapped}/[Spec]:( \+ member(Spec, Mapped) ), OnDisk, Unmapped).


%% load_corpus(+QuasarRoot): assert the corpus as journey/3 facts.
load_corpus(QuasarRoot) :-
    reset_corpus,
    build_corpus(QuasarRoot, Journeys),
    forall(member(journey(SpecBase, Header, Titles), Journeys),
           assertz(journey(SpecBase, Header, Titles))).
