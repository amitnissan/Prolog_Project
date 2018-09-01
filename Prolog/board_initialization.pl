test:- /*Defining and initializing the checkers' board and rows*/
	functor(Board,board,8),Board = board(A,B,C,D,E,F,G,H),
	functor(A,first,8),
	functor(B,second,8),
	functor(C,third,8),
	functor(D,fourth,8),
	functor(E,fifth,8),
	functor(F,sixth,8),
	functor(G,seventh,8),
	functor(H,eighth,8).

	init_odd_black(A),
	init_even_black(B),
	init_odd_black(C),
	init_even_none(D),
	init_odd_none(E),
	init_even_white(F),
	init_odd_white(G),
	init_even_white(H).


init_odd_black(Line):- /*Initializing black soldiers in odd numbered rows*/
	setarg(1,Line,0),setarg(2,Line,b),setarg(3,Line,0),
	setarg(4,Line,b),setarg(5,Line,0),setarg(6,Line,b),
	setarg(7,Line,0),setarg(8,Line,b).


init_even_black(Line):- /*Initializing black soldiers in even numbered rows*/
	setarg(1,Line,b),setarg(2,Line,0),setarg(3,Line,b),
	setarg(4,Line,0),setarg(5,Line,b),setarg(6,Line,0),
	setarg(7,Line,b),setarg(8,Line,0).


init_odd_none(Line):- /*Initializing cells in odd numbered rows which have no soldiers*/
	setarg(1,Line,0),setarg(2,Line,1),setarg(3,Line,0),
	setarg(4,Line,1),setarg(5,Line,0),setarg(6,Line,1),
	setarg(7,Line,0),setarg(8,Line,1).


init_even_none(Line):- /*Initializing cells in even numbered rows which have no soldiers*/
	setarg(1,Line,1),setarg(2,Line,0),setarg(3,Line,1),
	setarg(4,Line,0),setarg(5,Line,1),setarg(6,Line,0),
	setarg(7,Line,1),setarg(8,Line,0).


init_odd_white(Line):- /*Initializing white soldiers in odd numbered rows*/
	setarg(1,Line,0),setarg(2,Line,w),setarg(3,Line,0),
	setarg(4,Line,w),setarg(5,Line,0),setarg(6,Line,w),
	setarg(7,Line,0),setarg(8,Line,w).


init_even_white(Line):- /*Initializing white soldiers in even numbered rows*/
	setarg(1,Line,w),setarg(2,Line,0),setarg(3,Line,w),
	setarg(4,Line,0),setarg(5,Line,w),setarg(6,Line,0),
	setarg(7,Line,w),setarg(8,Line,0).


replace_in_board(Board,P1,P2):- /*Placing the value of P1 cell in P2 and clearing P1 cell's value*/
	position(Board,P1,Temp),
	set_value(Board,P2,Temp),
	clear(Board,P1).



set_value(Board,p(X,Y),Value):- /*Setting the cell in cordinates (X,Y) to be Value*/
	arg(X,Board,Line),
	setarg(Y,Line,Value).



clear(Board,p(X,Y)):- /*Clearing the cell in cordinates (X,Y) to its original color in the board (without soldiers on it)*/
	(
		mod(X,2) = 0	->
		(
			mod(Y,2) = 0	->
				set_value(Board,p(X,Y),1)

			;	set_value(Board,p(X,Y),0)
		)
	;	mod(X,2) \= 0	->
		(
			mod(Y,2) = 0	->
				set_value(Board,p(X,Y),1)

			;	set_value(Board,p(X,Y),0)
		)
	).



position(Board,p(X,Y),Res):- /*Saving the cell in cordinates (X,Y) in the variable Res*/
	arg(X,Board,Line),
	arg(Y,Line,Res).




print_board(_,9):-!.

print_board(Board,X):-
	arg(X,Board,Line),
	print_line(Line,1),nl,
	X1 is X + 1,
	print_board(Board,X1).


print_line(_,9):-!.

print_line(Line,N):-
	arg(N,Line,X),
	write(X),tab(1),write('|'),tab(1),
	N1 is N + 1,
	print_line(Line,N1).