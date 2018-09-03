start:- /*Defining and initializing the checkers' board and rows*/
	functor(Board,board,8),Board = board(A,B,C,D,E,F,G,H),
	functor(A,first,8),
	functor(B,second,8),
	functor(C,third,8),
	functor(D,fourth,8),
	functor(E,fifth,8),
	functor(F,sixth,8),
	functor(G,seventh,8),
	functor(H,eighth,8),


	init_odd_black(A),
	init_even_black(B),
	init_odd_black(C),
	init_even_none(D),
	init_odd_none(E),
	init_even_white(F),
	init_odd_white(G),
	init_even_white(H),
	print_board(Board,1),
	write('please select a player to start the game, enter 1 to start, 2 to be second'),nl,
    read(V),
        (
            V =1 ->
                turn(Board,player,w)
        ;   turn(Board,computer,w)
        ).



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


replace_in_board(Board,P1,P2,Player):- /*Placing the value of P1 cell in P2 and clearing P1 cell's value*/
	legal_move(s(Board,Player),P1,P2),
	set_value(Board,P2,Player),
	clear(Board,P1),
	check_queen(Board,P2,Player).

set_value(Board,p(X,Y),Value):- /*Setting the cell in cordinates (X,Y) to be Value*/
	arg(X,Board,Line),
	setarg(Y,Line,Value).



clear(Board,p(X,Y)):- /*Clearing the cell in cordinates (X,Y) to its original color in the board (without soldiers on it)*/
	(
		0 is mod(X,2) 	->
		(
			0 is mod(Y,2) 	->
				set_value(Board,p(X,Y),0)

			;	set_value(Board,p(X,Y),1)
		)
	;	0 is mod(Y,2) 	->
				set_value(Board,p(X,Y),1)

			;	set_value(Board,p(X,Y),0)

	).



legal_move(s(Board,w),p(X1,Y1),p(X2,Y2)):- /*Check validity of the white soldiers move*/
    position(Board,p(X1,Y1),w),
    between(1,8,X2),
    between(1,8,Y2),
    position(Board,p(X2,Y2),Element),\+atom(Element),
    (
        (T1 is X1-1,T2 is Y1+1, Y2 = T2,X2 = T1); /*Regular move - moving across the board to the right or left*/
        (T1 is X1-1,T2 is Y1-1, Y2 = T2,X2 = T1);
        (T1 is X1-2, T2 is Y1-2,T3 is X1-1,T4 is Y1-1,Y2 = T2, X2 = T1, position(Board,p(T3,T4),b),clear(Board,p(T3,T4))); /*Eating a black soldier*/
        (T1 is X1-2, T2 is Y1+2,T3 is X1-1,T4 is Y1+1,Y2 = T2, X2 = T1, position(Board,p(T3,T4),b),clear(Board,p(T3,T4)))
    ).


legal_move(s(Board,b),p(X1,Y1),p(X2,Y2)):- /*Check validity of the white soldiers move*/
    position(Board,p(X1,Y1),b),
    between(1,8,X2),
    between(1,8,Y2),position(Board,p(X2,Y2),Element),\+atom(Element),
    (
        (T1 is X1+1,T2 is Y1+1, Y2 = T2,X2 = T1); /*Regular move - moving across the board to the right or left*/
        (T1 is X1+1,T2 is Y1-1,Y2 = T2 , X2 = T1);
        (T1 is X1+2, T2 is Y1-2,T3 is X1+1, T4 is Y1-1, Y2 = T2, X2 = T1, position(Board,p(T3,T4),b),clear(Board,p(T3,T4))); /*Eating a black soldier*/
        (T1 is X1+2, T2 is Y1+2,T3 is X1+1, T4 is Y1+1, Y2 = T2, X2 = T1, position(Board,p(T3,T4),b),clear(Board,p(T3,T4)))
    ).

legal_move(s(Board,Player),p(X1,Y1),p(X2,Y2)):- /*Check validity of the white queen move*/
    (Player = qw; Player = qb),
    position(Board,p(X1,Y1),Player),
    between(1,8,X2),
    between(1,8,Y2),position(Board,p(X2,Y2),Element),\+atom(Element),
    second_player(Player,SecondPlayer),
    (
        (T1 is X1+1,T2 is Y1+1, Y2 = T2,X2 = T1); /*Regular move - moving across the board to the right or left*/
        (T1 is X1+1,T2 is Y1-1,Y2 = T2 , X2 = T1);
        (T1 is X1-1,T2 is Y1+1, Y2 = T2,X2 = T1);
        (T1 is X1-1,T2 is Y1-1,Y2 = T2 , X2 = T1);
        (T1 is X1+2, T2 is Y1-2,T3 is X1+1, T4 is Y1-1, Y2 = T2, X2 = T1, position(Board,p(T3,T4),SecondPlayer),clear(Board,p(T3,T4))); /*Eating a black soldier*/
        (T1 is X1+2, T2 is Y1+2,T3 is X1+1, T4 is Y1+1, Y2 = T2, X2 = T1, position(Board,p(T3,T4),SecondPlayer),clear(Board,p(T3,T4)));
        (T1 is X1-2, T2 is Y1-2,T3 is X1-1, T4 is Y1-1, Y2 = T2, X2 = T1, position(Board,p(T3,T4),SecondPlayer),clear(Board,p(T3,T4)));
        (T1 is X1-2, T2 is Y1+2,T3 is X1-1, T4 is Y1+1, Y2 = T2, X2 = T1, position(Board,p(T3,T4),SecondPlayer),clear(Board,p(T3,T4)))
    ).

check_queen(Board,p(X,Y),Player):-
    (
        (X = 8; X = 1) ->
        (
            Player = b  ->
     	        set_value(Board,p(X,Y),qb)
        ;	Player = w ->
     	        set_value(Board,p(X,Y),qw)
        )
        ;   write('')
    ).


position(Board,p(X,Y),Res):- /*Saving the cell in cordinates (X,Y) in the variable Res*/
	arg(X,Board,Line),
	arg(Y,Line,Res).


turn(Board,player,Color):-
    write('insert your next move'),
    read(P1),
    read(P2),
    replace_in_board(Board,P1,P2,Color),
	second_player(Color,Next_color),
	print_board(Board,1),
	turn(Board,computer,Next_color).


turn(Board,computer,Color):-
    write('Computer turn'),
    exAlphabeta(s(Board,Color),2,P1/P2),nl,
    position(Board,P1,Res),
    set_value(Board,P2,Res),
    clear(Board,P1),
    write('computer move was '),write(P1),write(' to '),write(P2),nl,
	second_player(Color,Next_color),
	print_board(Board,1),
	turn(Board,player,Next_color).


won(s(Board,Player)):-
    second_player(Player,SecondPlayer),
    findall(EndPos,legal_move(s(Board,SecondPlayer),_,EndPos),[]).


% Figure 24.5  An implementation of the alpha-beta algorithm.

min_to_move(s(_,player)).
max_to_move(s(_,computer)).

%moves( Pos, PosList): PosList is a list of all positions which are reachable by a legal move from Pos
moves( Pos, PosList):-
    Pos = s(Board,Color),
    \+ (won(s(Board,Color))),
    findall(StartPos/EndPos,legal_move(s(Board,Color),StartPos,EndPos),PosList),
    PosList \= [].

exAlphabeta(s(BoardFunctor,StartTurn), Depth,Next_Move)  :-
    alphabeta(s(BoardFunctor,StartTurn), -1000000, 1000000, Next_Move, _,Depth),nl.

alphabeta( Pos, Alpha, Beta, GoodPos, Val,MaxDepth)  :-
    MaxDepth >0,
    moves( Pos, PosList), !,
    NewMaxDepth is MaxDepth - 1,
    boundedbest( PosList, Alpha, Beta, GoodPos, Val,NewMaxDepth)
    ;
    staticval( Pos, Val).                              % Static value of Pos

staticval( _, 14).

boundedbest( [Pos | PosList], Alpha, Beta, GoodPos, GoodVal,MaxDepth)  :-
  alphabeta( Pos, Alpha, Beta, _, Val,MaxDepth),
  goodenough( PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal,MaxDepth).

goodenough( [], _, _, Pos, Val, Pos, Val,_)  :-  !.    % No other candidate

goodenough( _, Alpha, Beta, Pos, Val, Pos, Val,_)  :-
    min_to_move( Pos), Val > Beta, !                   % Maximizer attained upper bound
    ;
    max_to_move( Pos), Val < Alpha, !.                 % Minimizer attained lower bound

goodenough( PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal,MaxDepth)  :-
    newbounds( Alpha, Beta, Pos, Val, NewAlpha, NewBeta),    % Refine bounds
    boundedbest( PosList, NewAlpha, NewBeta, Pos1, Val1,MaxDepth),
    betterof( Pos, Val, Pos1, Val1, GoodPos, GoodVal).

newbounds( Alpha, Beta, Pos, Val, Val, Beta)  :-
    min_to_move( Pos), Val > Alpha, !.                 % Maximizer increased lower bound

newbounds( Alpha, Beta, Pos, Val, Alpha, Val)  :-
    max_to_move( Pos), Val < Beta, !.                 % Minimizer decreased upper bound

newbounds( Alpha, Beta, _, _, Alpha, Beta).          % Otherwise bounds unchanged

betterof( Pos, Val, _, Val1, Pos, Val)  :-        % Pos better than Pos1
    min_to_move( Pos), Val > Val1, !
    ;
    max_to_move( Pos), Val < Val1, !.

betterof( _, _, Pos1, Val1, Pos1, Val1).             % Otherwise Pos1 better
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

second_player(Player,SecondPlayer):-
    Player = w ->
        SecondPlayer = b;
        SecondPlayer = w.


num_in_board(_,9,0,_).

num_in_board(Board,X,Num,Kind):-
	arg(X,Board,Line),
	num_in_line(Line,1,N1,Kind),
	X1 is X + 1,
	num_in_board(Board,X1,N2,Kind),
	Num is N1 + N2.





num_in_line(_,9,0,_):-!.

num_in_line(Line,X,Num,Kind):-
	X1 is X + 1,
	num_in_line(Line,X1,Num1,Kind),
	arg(X,Line,Temp),
	(
	    Temp = Kind     ->
	        Z = 1
	;   Z = 0
	),
	Num is Num1 + Z.


