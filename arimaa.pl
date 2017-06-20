%:- module(bot,
      [  get_moves/3
      ]).
	
% A few comments but all is explained in README of github

board([[0,0,rabbit,silver],[0,1,rabbit,silver],[4,4,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[4,5,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]).

% get_moves signature
% get_moves(Moves, gamestate, board).

% Exemple of variable
% gamestate: [side, [captured pieces] ] (e.g. [silver, [ [0,1,rabbit,silver],[0,2,horse,silver] ]) 

%set_board(board) :- Board = [[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]].

% Call exemple:
% get_moves(Moves, [silver, []], [[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]).

% default call

%position
% Piece2 is placed on the top of Piece1

up([X1,Y1],[X2,Y2]) :- Y is Y1-1, X2 = X1, Y2 = Y, Y>=0.

% Piece2 is placed below Piece1

down([X1,Y1],[X2,Y2]) :-  Y is Y1+1, X2 = X1, Y2 = Y, Y<8.

% Piece2 is placed to the right of Piece1

right([X1,Y1],[X2,Y2]) :- X is X1+1, X2 = X, Y2 = Y1, X<8.

% Piece2 is placed at the left of Piece1

left([X1,Y1],[X2,Y2]) :-  X is X1-1, X2 = X, Y2 = Y1, X>=0.

%in_game

in_game(X1,Y1) :- 	X1 >= 0, 
					X1 < 8, 
					Y1 >= 0,
					Y1 < 8.

%align

align(X1,Y1,X2,Y2,X3,Y3) :- up([X1,Y1],[X2,Y2]),up([X2,Y2],[X3,Y3]),!.
align(X1,Y1,X2,Y2,X3,Y3) :- down([X1,Y1],[X2,Y2]),down([X2,Y2],[X3,Y3]),!.
align(X1,Y1,X2,Y2,X3,Y3) :- right([X1,Y1],[X2,Y2]),right([X2,Y2],[X3,Y3]),!.
align(X1,Y1,X2,Y2,X3,Y3) :- left([X1,Y1],[X2,Y2]),left([X2,Y2],[X3,Y3]),!.

%neighbor of X1,Y1 

neighbor(X1,Y1,X2,Y2) :- up([X1,Y1],[X2,Y2]).
neighbor(X1,Y1,X2,Y2) :- down([X1,Y1],[X2,Y2]).
neighbor(X1,Y1,X2,Y2) :- left([X1,Y1],[X2,Y2]).
neighbor(X1,Y1,X2,Y2) :- right([X1,Y1],[X2,Y2]).

%friendly neighbor

friendly_neighbor(X,Y,Team,Board) :- 
							neighbor(X,Y,Z,W) ,
							element([Z,W,_,Team],Board),
							Team = silver.

%enemy neighbor

enemy_neighbor(X,Y,Enemy,Team,Board) :- 
							neighbor(X,Y,Z,W) ,
							element([Z,W,Enemy,Team],Board),
							Team = gold.

%empty place

empty(Board,X,Y) :- 
	\+element([X,Y,_,_],Board).


%trap position

trap([2,2]).
trap([2,5]).
trap([5,2]).
trap([5,5]).

%Predicat to name a Piece

elephant(Piece) :- Piece = elephant.
camel(Piece) :- Piece = camel.
horse(Piece) :- Piece = horse.
dog(Piece) :- Piece = dog.
cat(Piece) :- Piece = cat.
rabbit(Piece) :- Piece = rabbit.

%testPiece

isPiece(Piece) :- elephant(Piece),!.
isPiece(Piece) :- camel(Piece),!.
isPiece(Piece) :- horse(Piece),!.
isPiece(Piece) :- dog(Piece),!.
isPiece(Piece) :- cat(Piece),!.
isPiece(Piece) :- rabbit(Piece),!.

%give back the strength of a Piece.

strength(Piece,5) :- elephant(Piece),!.
strength(Piece,4) :- camel(Piece),!.
strength(Piece,3) :- horse(Piece),!.
strength(Piece,2) :- dog(Piece),!.
strength(Piece,1) :- cat(Piece),!.
strength(Piece,0) :- rabbit(Piece),!.

%Piece1 stronger than Piece2

stronger(Piece1,Piece2) :- strength(Piece1,S1) , strength(Piece2,S2) , S1 > S2.

% moove ok from X1,Y1 to X2,Y2

ok_moove([[X, Y],[W,Z]], Moves,Board):- 
					element([X,Y,Piece,silver],Board),
					neighbor(X,Y,W,Z),
					empty(Board,W,Z),
					rabbit(Piece),
					\+trap([W,Z]),
					\+frozen(X,Y,Piece,Board),
					\+up([X,Y],[W,Z]),
					\+member([[X,Y],[W,Z]],Moves)
					.

ok_moove([[X, Y],[W,Z]], Moves,Board):- 
					element([X,Y,Piece,silver],Board),
					Team = sivler,
					neighbor(X,Y,W,Z),
					empty(Board,W,Z),
					\+trap([W,Z]),
					\+frozen(X,Y,Piece,Board),
					\+member([[X,Y],[W,Z]],Moves)
					.

%ajout list
add(X,[],L1) :- L1 = [X],!.
add(X,L,L1) :- L1 =[X|L].

%bad position

%dangerous trap

bad_position(X,Y,Piece,Board) :- 
						trap([X,Y]),
						\+friendly_neighbor(X,Y,silver,Board),!.

%dangerous position

bad_position(X,Y,Piece,Board) :- 
					\+friendly_neighbor(X,Y,silver,Board), 
					enemy_neighbor(X,Y,Enemy,gold,Board), 
					stronger(Enemy,Piece),!. 

%frozen Piece

frozen(X1,Y1,Piece,Board) :- 
			\+friendly_neighbor(X1,Y1,silver,Board), 
			enemy_neighbor(X1,Y1,Enemy,gold,Board), 
			stronger(Enemy,Piece), 
			!.

%replace

replace(_, _, [], []).
replace(O, R, [O|T], [R|T2]) :- replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- H \= O, replace(O, R, T, T2).

%push & pull

ok_push([X1,Y1],[X2,Y2],[X3,Y3],Board) :-			
			element([X1,Y1,Piece,silver],Board), 
			element([X2,Y2,Enemy,gold],Board),
			stronger(Piece,Enemy),
			align(X1,Y1,X2,Y2,X3,Y3),
			in_game(X3,Y3),
			empty(Board,X3,Y3),
			\+frozen(X1,Y1,Piece,Board),
			\+enemy_neighbor(X2,Y2,Enemy_neir,gold,Board), 
			!.

push([X,Y],[W,Z],Board,NBoard):-
					ok_push([X,Y],[W,Z],[T,V],Board),
					replace([W,Z,Enemy,gold],[T,V,Enemy,gold],Board,TmpBoard),
					replace([X,Y,Piece,silver],[W,Z,Piece,silver],TmpBoard,NBoard),
					!.

ok_pull([X1,Y1],[X2,Y2],[X3,Y3],Board) :-
						element([X2,Y2,Piece,silver],Board), 
						element([X1,Y1,Enemy,gold],Board),
						align(X1,Y1,X2,Y2,X3,Y3),
						in_game(X3,Y3),
						empty(Board,X3,Y3),
						stronger(Piece,Enemy),	
						\+frozen(X2,Y2,Piece,Board),
						\+enemy_neighbor(X1,Y1,Enemy_neir,gold,Board), 
						!.

pull([X,Y],[W,Z],Board,NBoard):-
						ok_pull([W,Z],[X,Y],[T,V],Board),
						replace([X,Y,Piece,silver],[T,V,Piece,silver],Board,TmpBoard),
						replace([W,Z,Enemy,gold],[X,Y,Enemy,gold],TmpBoard,Board),
						!.
					


%usual fonction

element(X,[X|_]).
element(X,[_|R]) :- element(X,R).

%get all possible moove

getAllMoves([X,Y],ListMoove,OkMooves,Board):- setof([[X,Y],[W,Z]],ok_moove([[X,Y],[W,Z]],ListMoove,Board),OkMooves).

%Board Update

%board_update([[X,Y],[W,Z]],Board,NewBoard) :- push()

%add_moves(_,4,Board):- !.
add_moves(Moves,NB,Board) :- 
							getAllMoves([X,Y],Moves,[T|Q],Board),
							NB1 is NB + 1,
							append([T],Moves,LM),
							add_moves(LM,NB1,Board)
							.
add_moves(Moves,0,Board) :- 
					Moves = [],
					getAllMoves([X,Y],Moves,[T|Q],Board),
					NB1 = 1,
					append([T],Moves,LM),
					add_moves(LM,NB1,Board).

% default call

get_moves(Moves, Gamestate, Board):- add_moves(Moves,Board,0),!.

%test function

test([],_,_,[]).
test(Moves,OldMoves,I,Board):- 
			I < 5,
			getAllMoves([X,Y],Moves,[T|Q],Board),
			Nouveau_I is I + 1,
			append([T],OldMoves,NewMoves),
			test(Move,NewMoves,Nouveau_I,Board).
test([],_,4,_):- !.				

			