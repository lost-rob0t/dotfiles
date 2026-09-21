:- module(dotfiles_inventory_expert,
    [ expert_id/1,
      model_calls/1,
      canonical_store/1,
      inventory_delta/3,
      event_kind/1,
      current_quantity/2,
      reorder_needed/2,
      recipe_missing/3,
      recipe_available/2,
      ai_recipe_requires_provenance/1,
      food_table_columns/1,
      recipe_table_columns/1
    ]).

:- use_module(library(error)).
:- use_module(library(lists)).

expert_id('zara:expert/inventory').
model_calls(0).
canonical_store(org_roam).

event_kind(ordered).
event_kind(receive).
event_kind(buy).
event_kind(putaway).
event_kind(move).
event_kind(open).
event_kind(consume).
event_kind(waste).
event_kind(return).
event_kind(adjust_add).
event_kind(adjust_remove).

inventory_delta(receive, Qty, Qty) :- positive_number(Qty).
inventory_delta(buy, Qty, Qty) :- positive_number(Qty).
inventory_delta(return, Qty, Qty) :- positive_number(Qty).
inventory_delta(consume, Qty, Delta) :- positive_number(Qty), Delta is -Qty.
inventory_delta(waste, Qty, Delta) :- positive_number(Qty), Delta is -Qty.
inventory_delta(adjust_add, Qty, Qty) :- positive_number(Qty).
inventory_delta(adjust_remove, Qty, Delta) :- positive_number(Qty), Delta is -Qty.
inventory_delta(ordered, Qty, 0) :- positive_number(Qty).
inventory_delta(putaway, Qty, 0) :- positive_number(Qty).
inventory_delta(move, Qty, 0) :- positive_number(Qty).
inventory_delta(open, Qty, 0) :- positive_number(Qty).

positive_number(Value) :-
    must_be(number, Value),
    Value > 0.

current_quantity(Events, Quantity) :-
    must_be(list, Events),
    foldl(apply_event, Events, 0, Quantity).

apply_event(event(Kind, Qty), Before, After) :-
    inventory_delta(Kind, Qty, Delta),
    After is Before + Delta.

reorder_needed(Current, Threshold) :-
    must_be(number, Current),
    must_be(number, Threshold),
    Current =< Threshold.

recipe_missing(Requirements, Stocks, Missing) :-
    must_be(list, Requirements),
    must_be(list, Stocks),
    findall(
        ingredient(Key, Need, Unit),
        ( member(ingredient(Key, Need, Unit), Requirements),
          \+ stock_satisfies(Stocks, Key, Need, Unit)
        ),
        Missing).

recipe_available(Requirements, Stocks) :-
    recipe_missing(Requirements, Stocks, []).

stock_satisfies(Stocks, Key, Need, Unit) :-
    member(stock(Key, Have, Unit), Stocks),
    number(Have),
    number(Need),
    Have >= Need,
    !.

ai_recipe_requires_provenance(ai_generated).
ai_recipe_requires_provenance(unique_ai_tag).
ai_recipe_requires_provenance(source_reference).

food_table_columns([name, category, quantity, unit, location, reorder_at]).
recipe_table_columns([title, cuisine, servings, source, ai_generated]).
