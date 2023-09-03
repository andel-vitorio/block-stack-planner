% Main predicates
% Plan function for achieving goals from a given state.
plan(State, Goals, Plan) :-
    satisfied(State, Goals),  % If goals are satisfied in the current state.
    Plan = [].  % Then, the plan is empty.

plan(State, Goals, Plan) :-
    append(PrePlan, [Action], Plan),  % Select an action from the plan.
    select(State, Goals, Goal),  % Select a goal from the list of goals.
    achieves(Action, Goal),  % Check if the action achieves the goal.
    can(Action, Condition),  % Check if the action can be performed given certain conditions.
    preserves(Action, Goals),  % Check if the action preserves other goals.
    regress(Goals, Action, RegressedGoals),  % Update the goals after applying the action.
    plan(State, RegressedGoals, PrePlan).  % Recursively plan with the updated goals.

% Auxiliary predicates
% Check if a state satisfies a set of goals.
satisfied(State, Goals) :-
    delete_all(Goals, State, []).

% Select a goal from the list of goals.
select(State, Goals, Goal) :-
    member(Goal, Goals).

% Check if an action achieves a specific goal.
achieves(Action, Goal) :-
    adds(Action, Goals),
    member(Goal, Goals).

% Check if an action preserves other goals.
preserves(Action, Goals) :-
    deletes(Action, Relations),
    \+ (member(Goal, Relations), member(Goal, Goals)).

% Calculate the goals after applying an action.
regress(Goals, Action, RegressedGoals) :-
    adds(Action, NewRelations),
    delete_all(Goals, NewRelations, RestGoals),
    can(Action, Condition),
    addnew(Condition, RestGoals, RegressedGoals).

% Add new conditions to the goals.
addnew([], L, L).

addnew([Goal | _], Goals, _) :-
    impossible(Goal, Goals),
    !,
    fail.

addnew([X | L1], L2, L3) :-
    member(X, L2), !,
    addnew(L1, L2, L3).

addnew([X | L1], L2, [X | L3]) :-
    addnew(L1, L2, L3).

% Delete all occurrences of elements from a list.
delete_all([], _, []).

delete_all([X | L1], L2, Diff) :-
    member(X, L2), !,
    delete_all(L1, L2, Diff).

delete_all([X | L1], L2, [X | Diff]) :-
    delete_all(L1, L2, Diff).

% Check if an action is impossible due to specific conditions.
impossible(on(X, X), _).

impossible(on(X, Y), Goals) :-
    (member(clear(Y), Goals);
    member(on(X, Y1), Goals), Y1 \== Y;
    member(on(X1, Y), Goals), X1 \== X).

impossible(clear(X), Goals) :-
    member(on(_, X), Goals).

% Conditions for possible actions
% Define the conditions for the "move" action for different scenarios.
can(move(Block, From, To, Y, Z), Conditions) :-
    block(Block, 3),
    object(To),
    To \== Block,
    object(From),
    From \== To,
    Block \== From,
    Conditions = [clear(Block, A, B), clear(Block, F, B), clear(Block, G, B), F is A + 1, G is A + 2, clear(To, C, 0), clear(To, E, 0), clear(To, H, 0), E is C + 1, H is C + 2, E < 5, H < 6, on(Block, From, Q, W)].

can(move(Block, From, To, Y, Z), Conditions) :-
    block(Block, 3),
    object(To),
    To \== Block,
    object(From),
    From \== To,
    Block \== From,
    Conditions = [clear(Block, A, B), clear(Block, F, B), clear(Block, G, B), F is A + 1, G is A + 2, clear(To, C, D), clear(To, E, D), clear(To, H, D), clear(To, C, O), clear(To, E, O), O is D - 1, E is C + 1, H is C + 2, E < 5, H < 6, on(Block, From, Q, W)].

can(move(Block, From, To, Y, Z), Conditions) :-
    block(Block, 3),
    object(To),
    To \== Block,
    object(From),
    From \== To,
    Block \== From,
    Conditions = [clear(Block, A, B, Y, Z), clear(Block, F, B), clear(Block, G, B), F is A + 1, G is A + 2, clear(To, C, D), clear(To, E, D), clear(To, H, D), clear(To, C, O), clear(To, H, O), O is D - 1, E is C + 1, H is C + 2, E < 5, H < 6, on(Block, From, Q, W)].

can(move(Block, From, To, Y, Z), Conditions) :-
    block(Block, 3),
    object(To),
    To \== Block,
    object(From),
    From \== To,
    Block \== From,
    Conditions = [clear(Block, A, B), clear(Block, F, B), clear(Block, G, B), F is A + 1, G is A + 2, clear(To, C, D), clear(To, E, D), clear(To, H, D), clear(To, C, O), clear(To, H, O), clear(To, E, O), O is D - 1, E is C + 1, H is C + 2, E < 5, H < 6, on(Block, From, Q, W)].

can(move(Block, From, To, Y, Z), Conditions) :-
    block(Block, 2),
    object(To),
    To \== Block,
    object(From),
    From \== To,
    Block \== From,
    Conditions = [clear(Block, A, B), clear(Block, F, B), F is A + 1, clear(To, C, D), clear(To, E, D), E is C + 1, E < 6, on(Block, From, Q, W)].

can(move(Block, From, To, Y, Z), Conditions) :-
    block(Block, 1),
    object(To),
    To \== Block,
    object(From),
    From \== To,
    Block \== From,
    Conditions = [clear(Block, A, B), clear(To, C, D), on(Block, From, Q, W)].


% Define the effects of the "move" action for different block types.
adds(move(X, From, To, Y, Z), Effects) :-
    block(X, 1),
    Effects = [on(X, To, Q, W), clear(From, A, B)].

adds(move(X, From, To, Y, Z), Effects) :-
    block(X, 2),
    Effects = [on(X, To, Q, W), clear(From, A, B), clear(From, C, B), C is A + 1, C < 6].

adds(move(X, From, To, Y, Z), Effects) :-
    block(X, 3),
    Effects = [on(X, To, Q, W), clear(From, A, B), clear(From, C, B), C is A + 1, clear(From, D, B), D is A + 2, C < 5, D < 6].

% Define the conditions for objects and blocks.
object(X) :-
    place(X);
    block(X, _).

% Define blocks and their types.
block(a, 1).
block(b, 1).
block(c, 2).
block(d, 3).

% Define places.
place(0).
place(1).
place(2).
place(3).
place(4).
place(5).
