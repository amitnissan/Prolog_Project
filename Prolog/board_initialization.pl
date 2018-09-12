:- dynamic global/2.
:- dynamic s/2.

second_player(player,computer).
second_player(computer,player).
second_color(b,w).
second_color(w,b).
same(b,qb).
same(w,qw).

min_to_move(s(_,_,player,_,_)).
max_to_move(s(_,_,computer,_,_)).

build_board(_,Index,Size):-
  Size1 is Size + 1,Index = Size1.

build_board(Board,Index,Size):-
  arg(Index,Board,Line),
  functor(Line,l,Size),
  Size1 is Size/2,
  (
      1 is mod(Index,2),Index < Size1   ->
        init_odd_black(Line,1,Size)
  ;   0 is mod(Index,2),Index < Size1   ->
        init_even_black(Line,1,Size)
  ;   Index = Size1                   ->
        init_even_none(Line,1,Size)
  ;   Size2 is Size1 + 1,Index = Size2               ->
        init_odd_none(Line,1,Size)
  ;   1 is mod(Index,2),Index > Size1  ->
        init_odd_white(Line,1,Size)
  ;   init_even_white(Line,1,Size)
  ),
  Index1 is Index + 1,
  build_board(Board,Index1,Size).


start:- /*Defining and initializing the checkers board and rows*/
   ansi_format([bold,fg(red)], 'Welcome ~w', [,]),
   ansi_format([bold,fg(cyan)], ' Checkers game, good luck ~w', [ !]),nl,nl,
   write('please select the size of the board (numbers of rows)'),tab(2),
   read(Size),nl,
   functor(Board,board,Size),
   build_board(Board,1,Size),
   write('please select a player to start the game, enter 1 to start, 2 to be second'),tab(1),
   read(V),nl,
   write('Please select game level, enter 1 for easy level, 2 for medium or 3 for hard'),tab(1),
   read(L),nl,
   print_board(Board,Size),
        (
            V =1 ->
                turn(Board,player,w,1,_,L,Size)
        ;   turn(Board,computer,w,L,Size)
        ).



init_odd_black(_,Index,Size):-
  Size1 is Size + 1,Index = Size1,!.
init_odd_black(Line,Index,Size):- /*Initializing black soldiers in odd numbered rows*/
   (
      1 is mod(Index,2)   ->
        setarg(Index,Line,0)
    ; setarg(Index,Line,b)
    ),Index1 is Index + 1,
    init_odd_black(Line,Index1,Size).

init_even_black(_,Index,Size):-
  Size1 is Size + 1,Index = Size1.

init_even_black(Line,Index,Size):- /*Initializing black soldiers in even numbered rows*/
   (
      1 is mod(Index,2)   ->
        setarg(Index,Line,b)
    ; setarg(Index,Line,1)
    ),Index1 is Index + 1,
    init_even_black(Line,Index1,Size).


init_odd_none(_,Index,Size):-
  Size1 is Size + 1,Index = Size1.

init_odd_none(Line,Index,Size):- /*Initializing cells in odd numbered rows which have no soldiers*/
   (
      1 is mod(Index,2)   ->
        setarg(Index,Line,0)
    ; setarg(Index,Line,1)
    ),Index1 is Index + 1,
    init_odd_none(Line,Index1,Size).


init_even_none(_,Index,Size):-
  Size1 is Size + 1,Index = Size1.

init_even_none(Line,Index,Size):- /*Initializing cells in even numbered rows which have no soldiers*/
   (
      1 is mod(Index,2)   ->
        setarg(Index,Line,1)
    ; setarg(Index,Line,0)
    ),Index1 is Index + 1,
    init_even_none(Line,Index1,Size).


init_odd_white(_,Index,Size):-
  Size1 is Size + 1,Index = Size1.

init_odd_white(Line,Index,Size):- /*Initializing white soldiers in odd numbered rows*/
   (
      1 is mod(Index,2)   ->
        setarg(Index,Line,0)
    ; setarg(Index,Line,w)
    ),Index1 is Index + 1,
    init_odd_white(Line,Index1,Size).


init_even_white(_,Index,Size):-
  Size1 is Size + 1,Index = Size1.

init_even_white(Line,Index,Size):- /*Initializing white soldiers in even numbered rows*/
   (
      1 is mod(Index,2)   ->
        setarg(Index,Line,w)
    ; setarg(Index,Line,0)
    ),Index1 is Index + 1,
    init_even_white(Line,Index1,Size).


replace_in_board(Board,P1,P2,Player,Back,Size):- /*Placing the value of P1 cell in P2 and clearing P1 cell's value*/
   legal_move(s(Board,Player),P1,P2,ISEATING,PCLEAR,Back,Size),
   (
    ISEATING = 1 ->
        clear(Board,PCLEAR);
        true
   ),
   position(Board,P1,Value),
   set_value(Board,P2,Value),
   clear(Board,P1),
   check_queen(Board,P2,Player,Size).

set_value(Board,p(X,Y),Value):- /*Setting the cell in cordinates (X,Y) to be Value*/
   arg(X,Board,Line),
   setarg(Y,Line,Value).



clear(Board,p(X,Y)):- /*Clearing the cell in cordinates (X,Y) to its original color in the board (without soldiers on it)*/
   (
      0 is mod(X,2)  ->
      (
         0 is mod(Y,2)  ->
            set_value(Board,p(X,Y),0)

         ;  set_value(Board,p(X,Y),1)
      )
   ;  0 is mod(Y,2)  ->
            set_value(Board,p(X,Y),1)

         ;  set_value(Board,p(X,Y),0)

   ).



legal_move(s(Board,w),p(X1,Y1),p(X2,Y2),ISEATING, PCLEAR,_,Size):- /*Check validity of the white soldiers move*/
    position(Board,p(X1,Y1),w),
    between(1,Size,X2),
    between(1,Size,Y2),
    position(Board,p(X2,Y2),Element),\+atom(Element),
    (
        (T1 is X1-1,T2 is Y1+1, Y2 = T2,X2 = T1,ISEATING = 0); /*Regular move - moving across the board to the right or left*/
        (T1 is X1-1,T2 is Y1-1, Y2 = T2,X2 = T1,ISEATING = 0);
        (T1 is X1-2, T2 is Y1-2,T3 is X1-1,T4 is Y1-1,Y2 = T2, X2 = T1, position(Board,p(T3,T4),Second),(Second=b;same(b,Second)),ISEATING = 1, PCLEAR = p(T3,T4)); /*Eating a black soldier*/
        (T1 is X1-2, T2 is Y1+2,T3 is X1-1,T4 is Y1+1,Y2 = T2, X2 = T1, position(Board,p(T3,T4),Second),(Second=b;same(b,Second)),ISEATING = 1, PCLEAR = p(T3,T4))
    ).


legal_move(s(Board,b),p(X1,Y1),p(X2,Y2),ISEATING, PCLEAR,_,Size):- /*Check validity of the white soldiers move*/
    position(Board,p(X1,Y1),b),
    between(1,Size,X2),
    between(1,Size,Y2),position(Board,p(X2,Y2),Element),\+atom(Element),
    (
        (T1 is X1+1,T2 is Y1+1, Y2 = T2,X2 = T1,ISEATING = 0); /*Regular move - moving across the board to the right or left*/
        (T1 is X1+1,T2 is Y1-1,Y2 = T2 , X2 = T1,ISEATING = 0);
        (T1 is X1+2, T2 is Y1-2,T3 is X1+1, T4 is Y1-1, Y2 = T2, X2 = T1, position(Board,p(T3,T4),Second),(Second=w;same(w,Second)),ISEATING = 1, PCLEAR = p(T3,T4)); /*Eating a black soldier*/
        (T1 is X1+2, T2 is Y1+2,T3 is X1+1, T4 is Y1+1, Y2 = T2, X2 = T1, position(Board,p(T3,T4),Second),(Second=w;same(w,Second)),ISEATING = 1, PCLEAR = p(T3,T4))
    ).

legal_move(s(Board,Player),p(X1,Y1),p(X2,Y2),ISEATING, PCLEAR, Sec,Size):- /*Check validity of the white queen move*/
    position(Board,p(X1,Y1),Res),
    (
    	((Player = w, Res = qw);(Player = b, Res = qb);(Sec = 1)) ->
   			between(1,Size,X2),
    		between(1,Size,Y2),position(Board,p(X2,Y2),Element),\+atom(Element),
    		second_color(Player,SecondPlayer),
    		(
    		    (T1 is X1+1,T2 is Y1+1, Y2 = T2,X2 = T1, ISEATING = 0); /*Regular move - moving across the board to the right or left*/
    		    (T1 is X1+1,T2 is Y1-1,Y2 = T2 , X2 = T1, ISEATING = 0);
  	 		    (T1 is X1-1,T2 is Y1+1, Y2 = T2,X2 = T1, ISEATING = 0);
  	 		    (T1 is X1-1,T2 is Y1-1,Y2 = T2 , X2 = T1, ISEATING = 0);
 	    	    (T1 is X1+2, T2 is Y1-2,T3 is X1+1, T4 is Y1-1, Y2 = T2, X2 = T1, position(Board,p(T3,T4),Second),(Second=SecondPlayer;same(SecondPlayer,Second)),ISEATING = 1, PCLEAR = p(T3,T4)); /*Eating a black soldier*/
 	    	    (T1 is X1+2, T2 is Y1+2,T3 is X1+1, T4 is Y1+1, Y2 = T2, X2 = T1, position(Board,p(T3,T4),Second),(Second=SecondPlayer;same(SecondPlayer,Second)),ISEATING = 1, PCLEAR = p(T3,T4));
 	    	    (T1 is X1-2, T2 is Y1-2,T3 is X1-1, T4 is Y1-1, Y2 = T2, X2 = T1, position(Board,p(T3,T4),Second),(Second=SecondPlayer;same(SecondPlayer,Second)),ISEATING = 1, PCLEAR = p(T3,T4));
      			(T1 is X1-2, T2 is Y1+2,T3 is X1-1, T4 is Y1+1, Y2 = T2, X2 = T1, position(Board,p(T3,T4),Second),(Second=SecondPlayer;same(SecondPlayer,Second)),ISEATING = 1, PCLEAR = p(T3,T4)))
    ).



check_queen(Board,p(X,Y),Color,Size):-
    (
        (X = Size, Color = b) ->
            set_value(Board,p(X,Y),qb)
    ;   (X = 1, Color = w) ->
            set_value(Board,p(X,Y),qw)

    ;   true
    ).


position(Board,p(X,Y),Res):- /*Saving the cell in cordinates (X,Y) in the variable Res*/
   arg(X,Board,Line),
   arg(Y,Line,Res).






turn(Board,player,Color,Is_first,P,Level,Size):-
    second_color(Color,Next_color),
    (
      Is_first = 0    ->
        write('insert next move from : ' ),write(P),nl
      ; write('insert your next move'),nl
    ),
    repeat,read(Input),
    handle(Input,Is_first),   % process the input
    (
        Is_first = 0    ->
        (
            Input = p(X,Y),replace_in_board(Board,P,p(X,Y),Color,1,Size)  ->
                print_board(Board,Size),
                (
                    won(s(Board,Color,_),Size)   ->
                      write('player won - game over!')
                ;   (
                        legal_move(s(Board,Color),p(X,Y),_,1, _, 1,Size)   ->
                           turn(Board,player,Color,0,p(X,Y),Level,Size)
                    ;   turn(Board,computer,Next_color,Level,Size)

                    )
                )
        ;    write('Incorrect input, please try again  '),fail
        )
    ;   (
          (
            (same(Color,Next),moveable_soldiers(Board,Color,Size,0),moveable_soldiers(Board,Next,Size,0))  ->
              write('computer won - game over!')

          ;   Input = p(X1,Y1)/p(X2,Y2),replace_in_board(Board,p(X1,Y1),p(X2,Y2),Color,0,Size)  ->
              print_board(Board,Size),
              (
                  won(s(Board,Color,_),Size)   ->
                        write('player won - game over!')
              ;   Z is abs(X1 - X2),
                  (
                      Z is 2,legal_move(s(Board,Color),p(X2,Y2),_,1, _, 1,Size)   ->
                            turn(Board,player,Color,0,p(X2,Y2),Level,Size)
                  ;  turn(Board,computer,Next_color,Level,Size)
                  )
               )
        ;      write('Incorrect input, please try again  '),fail
        )
      )
    ).


turn(Board,computer,Color,Level,Size):-
      (
        (same(Color,Next),moveable_soldiers(Board,Color,Size,0),moveable_soldiers(Board,Next,Size,0)) ->
            write('player won - game over!')
      ;   write('Computer turn'),
          exAlphabeta(s(Board,Color,computer),Level,Size,s(Newboard,NextColor,_,P1,P2)),
          write('computer move was : '),write(P1),tab(2),write('->'),tab(2), write(P2),nl,print_board(Newboard,Size),
          (
                won(s(Newboard,Color,_),Size)   ->
                       write('computer won - game over!')
          ;     turn(Newboard,player,NextColor,1,_,Level,Size)

          )
      ).

handle(Input,Is_first):-
	(
		Input = stop	->
			write('<game is over>'),abort
	;	Input = _/_		->
			true
	;	Input = restart		->
			write('restart a new Game'),nl,nl,start
	; Input = p(_,_),Is_first = 0
  ; write('Incorrect input, please try again  '),fail
	).

won(s(Board,Player,_),Size):-
    nl,write(Player),nl,
    second_color(Player,SecondPlayer),
    (
        SecondPlayer = qb ->
          Second = w
    ;   SecondPlayer = qw ->
          Second = b
    ;   Second = SecondPlayer
    ),
    num_in_board(Board,1,0,Second,Size).





move(s(Board,Color,Player),Back,Start,Size,Next_turn):-
    second_color(Color,SecondColor),
    second_player(Player,SecondPlayer),
    (
        Back = 1    ->
            legal_move(s(Board,Color),Start,P2,1,_,1,Size),
            replace_in_board(Board,Start,P2,Color,_,Size),
            (
                legal_move(s(Board,Color),P2,_,1,_,1,Size)   ->
                    move(s(Board,Color,Player),1,P2,Size,Next_turn)
            ;   Next_turn = s(Board,SecondColor,SecondPlayer,Start,P2)
            )
    ;   legal_move(s(Board,Color),P1,P2,ISEATING,_,0,Size),
        replace_in_board(Board,P1,P2,Color,_,Size),
        (
            ISEATING = 1    ->
            (
                legal_move(s(Board,Color),P2,_,1,_,1,Size)   ->
                    move(s(Board,Color,Player),1,P2,Size,Next_turn)
            ;   Next_turn = s(Board,SecondColor,SecondPlayer,P1,P2)
            )
        ;   Next_turn = s(Board,SecondColor,SecondPlayer,P1,P2)
        )
    ).


% Figure 24.5  An implementation of the alpha-beta algorithm.



%moves( Pos, Next_turn): PosList is a list of all positions which are reachable by a legal move from Pos
moves( Pos ,PosList,Size):-
    \+(won(Pos,Size)),
    findall(Next_turn,move(Pos,0,_,Size,Next_turn),PosList),
    PosList \= [].

exAlphabeta(Pos, Depth,Size,Next_Move)  :-
    alphabeta(Pos, -1000000, 1000000, Next_Move, _,Depth,Size),nl.



alphabeta( Pos, Alpha, Beta, GoodPos, Val,MaxDepth,Size)  :-
    MaxDepth >0,
    moves( Pos, PosList,Size), !,
    NewMaxDepth is MaxDepth - 1,
    boundedbest( PosList, Alpha, Beta, GoodPos, Val,NewMaxDepth,Size)
    ;
    heuristic(Pos,Size,Val).                              % Static value of Pos



boundedbest( [Pos | PosList], Alpha, Beta, GoodPos, GoodVal,MaxDepth,Size)  :-
  alphabeta( Pos, Alpha, Beta, _, Val,MaxDepth,Size),
  goodenough( PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal,MaxDepth,Size).

goodenough( [], _, _, Pos, Val, Pos, Val,_,_)  :-  !.    % No other candidate

goodenough( _, Alpha, Beta, Pos, Val, Pos, Val,_,_)  :-
    min_to_move( Pos), Val > Beta, !                   % Maximizer attained upper bound
    ;
    max_to_move( Pos), Val < Alpha, !.                 % Minimizer attained lower bound

goodenough( PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal,MaxDepth,Size)  :-
    newbounds( Alpha, Beta, Pos, Val, NewAlpha, NewBeta),    % Refine bounds
    boundedbest( PosList, NewAlpha, NewBeta, Pos1, Val1,MaxDepth,Size),
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


print_board(Board,Size):-
    write('      '),
    printBoard(1,Size),nl,

    print_board(Board,1,Size).



print_board(_,Index,Size):-
  Size1 is Size + 1, Index = Size1,!.

print_board(Board,X,Size):-
   arg(X,Board,Line),
   ansi_format([bold,fg(cyan)], '  ~w', [X]),
   (
    X < 10  ->
      write(' : ')
  ; write(': ')
  ),print_line(Line,1,Size),nl,
   X1 is X + 1,
   print_board(Board,X1,Size).


print_line(_,Index,Size):-
  Size1 is Size + 1, Index = Size1.

print_line(Line,N,Size):-
   arg(N,Line,X),
   (
        X = b   ->
            ansi_format([bold,fg(black)],' ~c',[186]),write(' |')
   ;    X = w   ->
            ansi_format([bold,fg(magenta)],' ~c',[186]),write(' |')
   ;    (X = 1 ; X =0)   ->
            ansi_format([bold,fg(white)],' ~c',[216]),write(' |')
   ;    X = qb   ->
              ansi_format([bold,fg(black)],' ~c',[75]),write(' |')
;       X = qw   ->
              ansi_format([bold,fg(magenta)],' ~c',[75]),write(' |')

   ),
   N1 is N + 1,
   print_line(Line,N1,Size).

printBoard(Index,Size):-
  Size1 is Size + 1, Index = Size1,!.

printBoard(X,Size) :-
    (
        X < 10  ->
          ansi_format([bold,fg(cyan)], ' ~w', [X]),write(' |')
    ;   ansi_format([bold,fg(cyan)], '~w', [X]),write(' |')
    ),
    X1 is X + 1,
    printBoard(X1,Size).






moveable_soldiers(Board,Soldier,Size,Moveable):-
       (
            setof(s(P1,Soldier),P2^legal_move(s(Board,Soldier),P1,P2,_,_,0,Size),Res) ->
                length(Res,Moveable)
            ;
            Moveable = 0
       ).
heuristic(s(Board,Color,_,_,_),Size,Func):-
    moveable_soldiers(Board,qw,Size,Xw),
    moveable_soldiers(Board,w,Size,Yw),
    moveable_soldiers(Board,qb,Size,Xb),
    moveable_soldiers(Board,b,Size,Yb),
    num_in_board(Board,1,TempZw1,w,Size),
    num_in_board(Board,1,TempZb1,b,Size),
    num_in_board(Board,1,TempZw2,qw,Size),
    num_in_board(Board,1,TempZb2,qb,Size),
    FuncW is 1.25*(TempZw1 + TempZw2) + 1.5*Xw + Yw,
    FuncB is 1.25*(TempZb1 + TempZb2) + 1.5*Xb + Yb,
    (
        Color = w   ->
        (
            (TempZw1 + TempZw2 = 0 ; (moveable_soldiers(Board,qb,Size,0),moveable_soldiers(Board,b,Size,0))) ->
              Func = -1000
        ;   (TempZb1 + TempZb2 = 0 ; (moveable_soldiers(Board,qw,Size,0),moveable_soldiers(Board,w,Size,0)))   ->
              Func = 1000
        ;   Func is FuncB - FuncW
        )

    ;   (
            (TempZw1 + TempZw2 = 0 ; (moveable_soldiers(Board,qb,Size,0),moveable_soldiers(Board,b,Size,0)))  ->
              Func = 1000
        ;   (TempZb1 + TempZb2 = 0  ; (moveable_soldiers(Board,qw,Size,0),moveable_soldiers(Board,w,Size,0))) ->
              Func = -1000
        ;   Func is FuncW - FuncB
        )
    ).


num_in_board(_,Index,0,_,Size):-
  Size1 is Size + 1, Index = Size1,!.

num_in_board(Board,X,Num,Kind,Size):-
   arg(X,Board,Line),
   num_in_line(Line,1,N1,Kind,Size),
   X1 is X + 1,
   num_in_board(Board,X1,N2,Kind,Size),
   Num is N1 + N2.

num_in_line(_,Index,0,_,Size):-
  Size1 is Size + 1, Index = Size1,!.

num_in_line(Line,X,Num,Kind,Size):-
   X1 is X + 1,
   num_in_line(Line,X1,Num1,Kind,Size),
   arg(X,Line,Temp),
   (
       Temp = Kind     ->
           Z = 1
   ;   Z = 0
   ),
   Num is Num1 + Z.