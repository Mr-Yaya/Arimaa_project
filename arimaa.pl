:- module(bot,
      [  get_moves/3
      ]).
	

%position
% Piece2 is placed on the top of Piece1

up([X1,Y1],[X2,Y2]) :- X is X1-1, Y2 = Y1, X2 = X, X2>=0.

% Piece2 is placed below Piece1

down([X1,Y1],[X2,Y2]) :-  X is X1+1, Y2 = Y1, X2 = X, X2<8.

% Piece2 is placed to the right of Piece1

right([X1,Y1],[X2,Y2]) :- Y is Y1+1, X2 = X1, Y2 = Y, Y2<8.

% Piece2 is placed at the left of Piece1

left([X1,Y1],[X2,Y2]) :-  Y is Y1-1, X2 = X1, Y2 = Y, Y2>=0.

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

friendly_neighbor(X,Y,Z,W,Team,Board) :- 
							neighbor(X,Y,Z,W) ,
							element([X,Y,Piece,Team],Board),
							element([Z,W,Piece2,Team],Board),
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

rabbitBackFire([X1,Y1],[X2,Y2]):- up([X1,Y1],[X2,Y2]),
							                            rabbit(Piece).

%Piece1 stronger than Piece2

stronger(Piece1,Piece2) :- strength(Piece1,S1) , strength(Piece2,S2) , S1 > S2.

% moove ok from X1,Y1 to X2,Y2

ok_moove([[X, Y],[W,Z]],Moves,Board):- 
					element([X,Y,Piece,silver],Board),
					neighbor(X,Y,W,Z),
					empty(Board,W,Z),
					\+bad_position(W,Z,Piece,Board),
					\+frozen(X,Y,Piece,Board),
					\+rabbitBackFire([X,Y],[W,Z]),
					\+member([[X,Y],[W,Z]],Moves),
					\+member([[W,Z],[X,Y]],Moves)
					.


%ajout list
add(X,[],L1) :- L1 = [X],!.
add(X,L,L1) :- L1 =[X|L].

%bad position

%dangerous trap

bad_position(X,Y,Piece,Board) :- 
						trap([X,Y]),
						\+friendly_neighbor(X,Y,_,_,silver,Board),!.

%dangerous position

bad_position(X,Y,Piece,Board) :- 
					\+friendly_neighbor(X,Y,_,_,silver,Board), 
					enemy_neighbor(X,Y,Enemy,gold,Board), 
					stronger(Enemy,Piece),!. 

%frozen Piece

frozen(X1,Y1,Piece,Board) :- 
			\+friendly_neighbor(X1,Y1,_,_,silver,Board), 
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
			trap([X3,Y3]),
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
						trap([X2,Y2]),
						friendly_neighbor(X2,Y2,_,_,silver,Board), 
						!.

pull([X,Y],[W,Z],Board,NBoard):-
						ok_pull([W,Z],[X,Y],[T,V],Board),
						replace([X,Y,Piece,silver],[T,V,Piece,silver],Board,TmpBoard),
						replace([W,Z,Enemy,gold],[X,Y,Enemy,gold],TmpBoard,NBoard),
						!.
					

%usual fonction

element(X,[X|_]).
element(X,[_|R]) :- element(X,R).

%get all possible moove

getAllMoves([X,Y],ListMoove,OkMooves,Board):- setof([[X,Y],[W,Z]],ok_moove([[X,Y],[W,Z]],ListMoove,Board),OkMooves).

%Board Update


%board_update([[X,Y],[W,Z]],Board,NBoard) :- push([[X,Y],[W,Z]],Board,NBoard).

%board_update([[X,Y],[W,Z]],Board,NBoard) :- pull([[X,Y],[W,Z]],Board,NBoard).

board_update([[X,Y],[W,Z]],Board,NBoard) :- 
						element([X,Y,Piece,Team],Board),
						replace([X,Y,Piece,Team],[W,Z,Piece,Team],Board,NBoard).

% default call

get_moves(Moves, Gamestate, Board):- final([],0,Board,Moves).

%STRATEGY

%getDist 

get_dist_x(D,[X,Y],[W,Z]) :- DC is X-W, D is abs(DC).

get_dist_y(D,[X,Y],[W,Z]) :- DR is Y-Z, D is abs(DR).

get_free_goal_pos([X,Y],Board) :- \+element([7,Y,_,_],Board).

get_dist_from_win(D,[X,Y],Board) :- 	get_free_goal_pos([X,Y],Board),
										get_dist_x(D,[X,Y],[7,Y]).



nth(M, 0, [M | _]).
nth(M, N, [_ | Moves]) :- N1 is N - 1, nth(M, N1, Moves).

one_random_move([], Moves) :- length(Moves,L), L = 0. % , print("ERROR : No move to pick at one_random_move").
one_random_move(M, Moves) :- length(Moves,L), random(0, L, N), nth(M, N, Moves).


%

score_move([[X,Y],[U,V]],Score,Board) :-
										element([X,Y,rabbit,silver],Board),
										down([X,Y],[U,V]),  U == 7,
										Score is 100.

score_move([[X,Y],[U,V]],Score,Board) :-
										element([X,Y,rabbit,silver],Board),
										get_dist_from_win(D,[X,Y],Board),
										D >= 2, D =< 4,
										down([X,Y],[U,V]), 
										Score is 95.

score_move([[X,Y],[U,V]],Score,Board) :-
										\+element([X,Y,rabbit,silver],Board),
										neighbor(U,V,A,B),
										element([A,B,rabbit,gold],Board),
										A >= 1, A =< 2,
										Score is 94.

score_move([[X,Y],[U,V]],Score,Board) :-
										\+element([X,Y,rabbit,silver],Board),
										element([A,B,rabbit,gold],Board),
										A >= 1, A =< 2,
										get_dist_x(D1,[X,Y],[A,B]),
										D1 >= 0, D1 =< 2,
										get_dist_y(D2,[X,Y],[A,B]),
										D2 >= 0, D2 =< 2,
										Score is 92.

score_move([[X,Y],[U,V]],Score,Board) :- 
										element([X,Y,rabbit,silver],Board),
										friendly_neighbor(U,V,T,W,Team,Board),
										T \= X,
										W \= Y,
										Score is 90.

score_move([[X,Y],[U,V]],Score,Board) :- 
										element([X,Y,Piece,silver],Board),
										element([U,V,Enemy,gold],Board),
										ok_push([X,Y],[U,V],[T,V],Board),
										Score is 80.

score_move([[X,Y],[U,V]],Score,Board) :- 
										element([X,Y,Piece,silver],Board),
										element([U,V,Enemy,gold],Board),
										ok_pull([U,V],[X,Y],[T,V],Board),
										Score is 80.

score_move([[X,Y],[U,V]],Score,Board) :- 
										element([X,Y,elephant,silver],Board),
										neighbor(U,V,A,B),
										element([A,B,_,gold],Board),
										Score is 70.

score_move([[X,Y],[U,V]],Score,Board) :- 
										element([X,Y,elephant,silver],Board),
										Y == V,
										element([A,Y,_,gold],Board),
										get_dist_x(D,[X,Y],[A,Y]),
										D >= 2, D =< 3,
										Score is 66.


score_move([[X,Y],[U,V]],Score,Board) :- 
										element([X,Y,elephant,silver],Board),
										X == U,
										element([X,B,_,gold],Board),
										get_dist_y(D,[X,Y],[X,B]),
										D >= 2, D =< 3,
										Score is 64.

score_move([[X,Y],[U,V]],Score,Board) :- 
										\+element([X,Y,rabbit,silver],Board),
										friendly_neighbor(U,V,T,W,Team,Board),
										T \= X,
										W \= Y,
										Score is 60.

score_move([[X,Y],[U,V]],Score,Board) :- 
										element([X,Y,camel,silver],Board),
										neighbor(U,V,A,B),
										element([A,B,Enemy,gold],Board),
										stronger(camel,Enemy),
										Score is 50.

score_move([[X,Y],[U,V]],Score,Board) :- 
										element([X,Y,camel,silver],Board),
										Y == V,
										element([A,Y,Enemy,gold],Board),
										stronger(camel,Enemy),
										get_dist_x(D,[X,Y],[A,Y]),
										D >= 2, D =< 3,
										Score is 46.

score_move([[X,Y],[U,V]],Score,Board) :- 
										element([X,Y,camel,silver],Board),
										X == U,
										element([X,B,Enemy,gold],Board),
										stronger(camel,Enemy),
										get_dist_y(D,[X,Y],[X,B]),
										D >= 2, D =< 3,
										Score is 44.

score_move([[X,Y],[U,V]],Score,Board) :- 
										element([X,Y,horse,silver],Board),
										neighbor(U,V,A,B),
										element([A,B,Enemy,gold],Board),
										stronger(horse,Enemy),
										Score is 40.

score_move([[X,Y],[U,V]],Score,Board) :- 
										element([X,Y,horse,silver],Board),
										Y == V,
										element([A,Y,Enemy,gold],Board),
										stronger(horse,Enemy),
										get_dist_x(D,[X,Y],[A,Y]),
										D == 2, 
										Score is 36.

score_move([[X,Y],[U,V]],Score,Board) :- 
										element([X,Y,horse,silver],Board),
										X == U,
										element([X,B,Enemy,gold],Board),
										stronger(horse,Enemy),
										get_dist_y(D,[X,Y],[X,B]),
										D == 2, 
										Score is 34.

score_move([[X,Y],[U,V]],Score,Board) :- 
										\+element([X,Y,rabbit,silver],Board),
										Y == V,
										element([A,Y,_,silver],Board),
										get_dist_x(D,[X,Y],[A,Y]),
										D == 2, 
										Score is 30.

score_move([[X,Y],[U,V]],Score,Board) :- 
										\+element([X,Y,rabbit,silver],Board),
										X == U,
										element([X,B,_,silver],Board),
										get_dist_y(D,[X,Y],[X,B]),
										D == 2, 
										Score is 20.


score_move([[X,Y],[U,V]],Score,Board) :-
										element([X,Y,rabbit,silver],Board),
										down([X,Y],[U,V]),
										Score is 10.

score_move([[X,Y],[U,V]], 0, _).

add_score_to_move([[X,Y],[W,Z]], [[[X,Y],[W,Z]],Score], Board) :- score_move([[X,Y],[W,Z]],Score,Board).

get_all_score([], ScoredMoves, _).
get_all_score([T|Q], ScoredMoves, Board):- add_score_to_move(T,ScoreMove,Board),
											get_all_score(Q,Scored,Board),
											append(Scored,[ScoreMove],ScoredMoves).

get_best_score([],M,M).
get_best_score([[[[X,Y],[W,Z]],S]|Q],[[[A,B],[C,D]],S2],M) :- 
											S>S2,
											get_best_score(Q, [[[X,Y],[W,Z]],S],M).

get_best_score([[[[X,Y],[W,Z]],S]|Q],[[[A,B],[C,D]],S2],M) :- 
											S2>=S,
											get_best_score(Q, [[[A,B],[C,D]],S2],M).
get_best_score([[[[X,Y],[W,Z]],S]|Q],[[[A,B],[C,D]],S2],M) :- 
											S2==S,
											S2==0,
											append([[[X,Y],[W,Z]],S],[[[A,B],[C,D]],S2],RandomCrapMoves),
											one_random_move(Crap, RandomCrapMoves),
											get_best_score(Q,Crap,M).
get_best_score([[[[X,Y],[U,V]],S]|Q],M):- get_best_score(Q,[[[X,Y],[U,V]],S],M).



choose_move(Moves,[[X,Y],[W,Z]],Board) :- 	get_all_score(Moves, ScoredMoves, Board),
											get_best_score(ScoredMoves,[[[X,Y],[W,Z]],M]),
											!.

add_moves(Nb,NNb,Moves,Move,Board,NBoard) :-
							getAllMoves([X,Y],Moves,ListOkMove,Board),
							choose_move(ListOkMove,Move,Board),
							NNb is Nb + 1,
							board_update(Move,Board,NBoard).

final(Moves_Final,4,_,Moves_Final).
final(Moves,NB,Board,Moves_Final) :- 
							add_moves(NB,NB1,Moves,T,Board,NBoard),
							append(Moves,[T],LM),
							board_update(T,Board,NBoard),
							final(LM,NB1,NBoard,Moves_Final).
