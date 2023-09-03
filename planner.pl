% Definition of objects (blocks and places)
object(X) :- place(X) ; block(X,_).

% Block size definition
block(a, 1).
block(b, 1).
block(c, 2).
block(d, 3).

% Place definition
place(0).
place(1).
place(2).
place(3).
place(4).
place(5).

% Predicate to check if a move is possible
can(move(Block, From, To, Y, Z), Conditions) :-
	% Conditions for moving a block
	block(Block, BlockSize),
	BlockSize >= 1,
	BlockSize =< 3,
	% Conditions for movement action
	append([clear(Block, A, B)], ExtraConditions, Conditions),
	clear(From, A, B),
	clear(To, C, D),
	on(Block, From, Q, W),
	different(Block, To),
	different(From, To),
	different(Block, From),
	% Additional conditions based on block size
	additional_conditions(Block, BlockSize, To, C, ExtraConditions).

% Predicate to add additional conditions based on block size
additional_conditions(_, 1, _, _, []).
additional_conditions(Block, Size, To, C, [clear(To, E, F), clear(To, G, H), E is C + 1, G is C + 2 | RestConditions]) :-
	Size > 1, E < 6, G < 6,
	(   
		Size > 2 ->  
		E < 5,
		G < 5 ; 
		true
	),
additional_conditions(Block, Size - 1, To, C, RestConditions).

% Predicate to check if an objective has been met
satisfied(State, Goals) :-
	delete_all(Goals, State, []).

satisfied(State, [Goal | Goals]) :-
	holds(Goal),
	satisfied(State, Goals).

% Predicate to select an objective from the list of goals
select(State, Goals, Goal) :-
	member(Goal, Goals).

% Predicate to check whether an action achieves a goal
achieves(Action, Goal) :-
	adds(Action, Goals),
	member(Goal, Goals).

% Predicate to check if an action preserves the goals
preserves(Action, Goals) :-
    deletes(Action, Relations),
	\+ (member(Goal, Relations), member(Goal, Goals)).

% Predicate to set goals back (reverse) after an action
regress(Goals, Action, RegressedGoals) :-
	adds(Action, NewRelations),
	delete_all(Goals, NewRelations, RestGoals),
	can(Action, Condition),
	addnew(Condition, RestGoals, RegressedGoals).

% Predicate to add new conditions after action
addnew([], L, L).
addnew([Goal | Rest], Goals, Result) :- (   
	impossible(Goal, Goals) ->  
		Result = Goals, % If the goal is impossible, keep the list of goals unchanged
		fail;
			
	memberchk(Goal, Goals) ->  
		addnew(Rest, Goals, Result); % If the target already exists, move on to the next one
		Result = [Goal | NewGoals], % Adds the goal to the list of goals
		addnew(Rest, NewGoals, Result)
	).

% Predicate to delete all the elements of a list from another list
delete_all([], _, []).
delete_all([X | Rest], ToDelete, Result) :- (   
	memberchk(X, ToDelete) -> 
		delete_all(Rest, ToDelete, Result); % Element found
	  Result = [X | NewList], % Element not found
		delete_all(Rest, ToDelete, NewList)
).

% Predicate to check if an action is impossible
impossible(on(X, X), _).
impossible(on(X, Y), Goals) :-
    memberchk(clear(Y), Goals),
    !. % If there is a "clear(Y)" objective, action is impossible
impossible(on(X, Y), Goals) :-
    memberchk(on(X, Y1), Goals),
    Y1 \== Y,
    !. % If there is an objective "on(X, Y1)" with Y1 different from Y, the action is impossible
impossible(on(X, Y), Goals) :-
    memberchk(on(X1, Y), Goals),
    X1 \== X. % If there is an objective "on(X1, Y)" with X1 different from X, the action is impossible

impossible(clear(X), Goals) :-
    memberchk(on(_, X), Goals). % If there is an "on(_, X)" goal, the action is impossible

% Predicate to check if two variables are different
holds(different(X, Y)) :-
	\+ X = Y; \+ X == Y.

% Main predicate for planning
plan(State, Goals, []) :-
    satisfied(State, Goals).
plan(State, Goals, Plan) :-
    append(PrePlan, [Action], Plan),
    select(State, Goals, Goal),
    achieves(Action, Goal),
    can(Action, Condition),
    preserves(Action, Goals),
    regress(Goals, Action, RegressedGoals),
    plan(State, RegressedGoals, PrePlan).
