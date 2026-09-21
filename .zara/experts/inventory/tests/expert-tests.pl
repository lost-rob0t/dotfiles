:- begin_tests(dotfiles_inventory_expert).
:- use_module('../kb/expert.pl').

test(receive_adds_stock) :-
    inventory_delta(receive, 3, 3).

test(consume_removes_stock) :-
    inventory_delta(consume, 2, -2).

test(current_quantity_replays_events) :-
    current_quantity(
        [event(buy, 5), event(consume, 1), event(waste, 0.5)],
        Quantity),
    assertion(Quantity =:= 3.5).

test(reorder_at_threshold) :-
    reorder_needed(2, 2).

test(recipe_reports_missing) :-
    recipe_missing(
        [ingredient(flour, 2, cup), ingredient(egg, 2, each)],
        [stock(flour, 3, cup), stock(egg, 1, each)],
        Missing),
    assertion(Missing == [ingredient(egg, 2, each)]).

test(recipe_available_when_stock_satisfies) :-
    recipe_available(
        [ingredient(flour, 2, cup), ingredient(egg, 2, each)],
        [stock(flour, 3, cup), stock(egg, 6, each)]).

test(symbolic_only) :-
    model_calls(0).

:- end_tests(dotfiles_inventory_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
