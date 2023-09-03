state1([clear(3),on(c,p([1,2])),on(b,6),on(a,4), on(d,p([a,b]))]).

block(a1).
block(a2).
block(b1).
block(c1).
pyramid(d1).

size(a1, 1).
size(a2, 1).
size(b1, 2).
size(c1, 3).
size(d1, 1).

place(1).
place(2).
place(3).
place(4).
place(5).
place(6).

% Move the block from a to b
can(move(Block, p([Ai,Aj]), p([Bi,Bj])),[clear(Block),clear(Bi),clear(Bj),on(Block,p([Ai,Aj]))]):-
	object(Block),           
	object(Ai),               
	object(Aj),
	Ai \== Block,           
	object(Bi), object(Bj),   
	Bi \== Ai,              
	Bj \== Aj,              
	Block \== Bi,
	safe_to_stack(Block, p([Bi, Bj])).

% Move the block from A to B
can(move(Block, p([Ai,Aj]), B),[clear(Block),clear(B),on(Block,p([Ai,Aj]))]) :-
	object(Block),           
	object(A),
	A \== Block,           
	object(Bi), object(Bj),   
	Bi \== A,               
	Bj \== A,
	safe_to_stack(Block, p([Bi,Bj])).


can(move(Block,From,To),[clear(Block),clear(To),on(Block,From)]) :-
	object(Block),          
	object(To),            
	To \== Block,           
	object(From),           
	From \== To,           
	Block \== From,
	safe_to_stack(Block, To).      

% It's safe to stack a smaller block on top of a larger one.
safe_to_stack(Block, To) :-
	\+ pyramid(To),
	size(Block, Sb),
	size(To, St),
	Sb =< St.

% It's safe to stack a block that's one unit larger than the other.
safe_to_stack(Block, p([Bi, Bj])) :-
	\+ pyramid(Bi), \+ pyramid(Bj),
	(
		place(Bi), place(Bj) ;
		(
			size(Block,Sb),
			size(Bi,Si),
			size(Bj,Sj),
			SizeTo is Si + Sj,
			Sb =< SizeTo + 1
		)
	).

% It's safe to stack blocks with the centroids aligned.
safe_to_stack(Block, To) :-
	\+ pyramid(To),
	size(Block, Sb),
	size(To,St),
	Diff is abs(Sb - St),
	Diff mod 2 =:= 0.


adds(move(X,From,To),[on(X,To),clear(From)]). 

deletes(move(X,From,To),[on(X,From),clear(To)]).

object(X):-
	place(X);
	block(X);
	pyramid(X).