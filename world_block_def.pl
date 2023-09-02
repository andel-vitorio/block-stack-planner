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

% safe_to_stack(Object, Object). Pryramids and blocks
safe_to_stack(Object, To) :-
	state1(State),											% Current State
	is_clear(Object, State),        			% 
	is_clear(To, State),
	(
		( sphere(Object), opened_box(To) ) ;
		( block(Object), \+ pyramids(To)) ;
		( block(Object), block(To)) ;
		( pyramids(Object), \+ pyramids(To)) ;
		( pyramids(Object), block(To))
	).


% Bloco a ser movido é menor que o bloco onde sera posicionado
safe_to_stack(Object, To) :-
	size(Object, SizeOfObject),
	size(To, SizeTo),
	(
		SizeOfObject =< SizeTo ;
		(
			SizeOfObject = 3, SizeTo = 1,
			state1(State),
			memberchk(on(To, Pi), State),
			Pi > 1, Pi < 6
		)
	).

safe_to_stack(Object, p([Bi, Bj])) :-
	size(Object, SizeObject),
	SizeObject = 2,
	state1(State),
	memberchk(on(Bi, Pi), State),
	memberchk(on(Bj, Pj), State),
	size(Bi, Si), size(Bj, Sj),
	SizeTo is abs(Pi - Pj) + 1 - (Si + Sj),
	SizeTo = 0.

safe_to_stack(Object, p([Bi, Bj])) :-
	size(Object, SizeObject),
	SizeObject = 3,
	state1(State),
	memberchk(on(Bi, Pi), State),
	memberchk(on(Bj, Pj), State),
	SizeTo is abs(Pi - Pj) + 1,
	(SizeTo = 2 ; SizeTo = 3).

safe_to_stack(Object, To) :-
	size(Object, SizeOfObject),
	size(To, SizeTo),
	SizeOfObject =< SizeTo.
	

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
horizontal_parallelepiped(a).
block(b).
block(c).
place(0).
place(1).
place(2).
place(3).
place(4).
place(5).

size(a, 3).
size(b, 1).
size(c, 1).

state1([
	clear(3),
	clear(4),
	clear(5),
	on(a, pos([1, 3])),
	on(b, 4),
	on(c, 6)
]).
