% Definition of action move( Block, From, To) in blocks world
% can(Action, Condition): Action possible if Condition true
can(move(Object, From, To), [clear(Block), clear(To), on(Block, From)]) :-
	\+ place(Object),										% Object to be moved cannot be a place
	object(To),													% 'To' is a block or a place
	To \== Block,												% Block cannot be moved to itself
	object(From),												% 'From' is a block or a place
	From \== To,												% Move to new position
	Block \== From,											% Block not moved from itself
	safe_to_stack(Object, To).					% It's safe to stack

% safe_to_stack(Object, Object): True if it's a block on a pyramid or false, otherwise.
safe_to_stack(Object, To) :-
	state1(State),											% Current State
	is_clear(Object, State),        			% 
	is_clear(To, State),
	(
		( block(Object), \+ pyramids(To)) ;
		( pyramids(Object), block(To))
	).


is_clear(Object, State) :- !,
	memberchk(clear(Object), State).

% adds(Action, Relationships): Action establishes Relationships 
adds(move(X, From, To), [on(X,To), clear(From)]).

% deletes(Action, Relationships): Action destroys Relationships 
deletes(move(X, From, To), [on(X, From), clear(To)]).

object(X) :- 													% X is an object if
	place(X)														% X is a place
	;																		% or
	block(X).														% X is a block

% A blocks world
pyramids(a).
block(b).
place(1).
place(2).
place(3).
place(4).

% A state in the blocks world
%
%				
%				a   b
%				= = = =
% place 1 2 3 4

state1([clear(a), on(a, 1), clear(2), clear(b), on(b, 3), clear(4)]).
