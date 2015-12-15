includeUtilities:- include('utilities.pl').
:-use_module(library(lists)).
:-use_module(library(clpfd)).
:-use_module(library(random)).



%Test purposes%

board7([[0,0,0,0,3,0,3],
        [4,0,0,2,0,0,0],
        [0,0,4,3,0,0,0],
        [4,0,0,0,3,0,2],
        [0,2,0,2,0,0,0],
        [0,0,0,0,2,0,0],
        [0,0,0,4,0,0,2]]).

board6([[0,3,0,0,0,2],
        [0,0,3,0,4,0],
        [0,1,0,0,0,0],
        [5,0,0,2,0,2],
        [0,0,0,0,0,0],
        [0,4,0,2,0,0]]).




generator(Size):-
	write('Generating random Hamle puzzle of size '), write(Size), write('.'), nl, nl,
	generate_random_board(Size, Initial),
	showBoard(Initial, Size, 0, Size), nl,
	write('This is your randomly generated initial puzzle.'), nl, nl,
	write('Enter anything to solve the puzzle.'), nl,
	read(_), nl,
	solve_board(Initial).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% GENERATOR %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_random_board(Size, Initial):- create_board(Sol, Size),
									adjacents(Sol),
									randomize_cardinality(Size, L),
									append(Sol, SolFlat),
									global_cardinality(SolFlat, L),
									labeling([], SolFlat),
									get_possib(Sol, Size, PossibList),
									create_board(Initial, Size),
									restrict(Initial, PossibList),
									append(Initial, InitFlat),
									global_cardinality(InitFlat, L),
									labeling([], InitFlat),
									nl, fd_statistics, nl,
									write('Generator finished! Here are some cool stats...'), nl, nl.

randomize_cardinality(Size, List):- NumPieces is floor(Size*Size*0.28),
									make_random_list(Size, List, 0, NumPieces).

make_random_list(Dim, L, 0, Num):- Number is (Dim*Dim)-Num,
								make_random_list(Dim, RestList, 1, Num),
								append([0-Number], RestList, L).

make_random_list(Dim, L, Elem, Num):- (Elem =:= Dim - 1 -> append([], [Elem-Num], L)
							;
							Aprox is ceiling(Num/(Dim-1)),
							(Aprox > 0 -> Lower is Aprox - 1
							;
								Lower is 0),
							(Aprox >= 0 -> Upper is Aprox + 1
							;
								Upper is 0),
							random(Lower, Upper, Final),
							NewNum is Num - Final,
							NewNum >= 0,
							NewElem is Elem + 1,
							make_random_list(Dim, RestList, NewElem, NewNum),
							append([Elem-Final], RestList, L)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% SOLVER %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve_board(Board):-
        length(Board, Dimension),
        get_cardinality(Board, Dimension, Cardin),
        create_board(Sol, Dimension),
        get_possib(Board, Dimension, PossibList),
        restrict(Sol, PossibList),
        adjacents(Sol),
        append(Sol, SolFlat),
        global_cardinality(SolFlat, Cardin),
		labeling([], SolFlat),
		nl, fd_statistics, nl,
		write('Solver finished! Here are some cool stats...'), nl, nl,
		write('Enter anything to see the solution'), nl,
		read(_), nl, nl,
		showBoard(Sol, Dimension, 0, Dimension).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Get list to be used in global_cardinality() to know how many pieces of each value are in the initial board. %%%
%%%%---------------------- List = [0-A, 1-B, 2-C,...,N-_] in a (N+1)x(N+1) board. -------------------------------%%%
%%%% Each variable will be assigned with the number of occurrences of each value in the initial board. %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_cardinality(Board, Dimension, List):- make_list(Dimension, List, 0, _),
											append(Board, BoardFlat),
											global_cardinality(BoardFlat, List).
										

make_list(Dim, L, Dim, A):- append([], [Dim-A], L).


make_list(Dim, L, Elem, A):- NewElem is Elem + 1,
							make_list(Dim, RestList, NewElem, _),
							append([Elem-A], RestList, L).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Create Initial Solution NxN Board %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_board(Board, N):- length(Board, N), maplist(set_length(N), Board).

set_length(L, Ls):- length(Ls, L).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Get possibilities list for each cell of the board %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_possib(Board, Dim, List):- get_possib_cell(Board, Dim, 1, 1, List).


get_possib_cell(Board, Dim, Dim, Dim, List):- make_possib_list(Board, Dim, Dim, Dim, TempList),
											append([], [TempList], List).


get_possib_cell(Board, Dim, Row, Dim, List):- make_possib_list(Board, Dim, Row, Dim, TempList),
											NewRow is Row + 1,
											get_possib_cell(Board, Dim, NewRow, 1, RestList),
											append([TempList], RestList, List).


get_possib_cell(Board, Dim, Row, Col, List):- make_possib_list(Board, Dim, Row, Col, TempList),
											NewCol is Col + 1,
											get_possib_cell(Board, Dim, Row, NewCol, RestList),
											append([TempList], RestList, List).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Make a list with the possible values of the (Row, Col) cell in the solution board %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_possib_list(Board, Dim, Row, Col, List):- checkRightPossib(Board, Dim, Row, Col, ResultRight, 1),
                                               checkLeftPossib(Board, Dim, Row, Col, ResultLeft, 1),
                                               checkUpPossib(Board, Dim, Row, Col, ResultUp, 1),
                                               checkDownPossib(Board, Dim, Row, Col, ResultDown, 1),
                                               Result = [[0], ResultRight, ResultLeft, ResultUp, ResultDown],
                                               append(Result, List).


checkRightPossib(Board, Dim, Row, Col, ResultRight, Counter):- NewCol is Col + Counter,
                                                               NewCol =< Dim, 
                                                               NewCounter is Counter + 1,
                                                               getCell(Board, Row, NewCol, Value, Dim),
                                                               Diff is NewCol - Col,
                                                               (Diff = Value -> checkRightPossib(Board, Dim, Row, Col, TempResult, NewCounter),
                                                                                append(TempResult, [Value], ResultRight) ; 
                                                               checkRightPossib(Board, Dim, Row, Col, ResultRight, NewCounter)).
checkRightPossib(_, _, _, _, _, _).


checkLeftPossib(Board, Dim, Row, Col, ResultLeft, Counter):- NewCol is Col - Counter,
                                                             NewCol >= 1,
                                                             NewCounter is Counter + 1,
                                                             getCell(Board, Row, NewCol, Value, Dim),
                                                             Diff is Col - NewCol,
                                                             (Diff = Value -> checkLeftPossib(Board, Dim, Row, Col, TempResult, NewCounter),
                                                                              append(TempResult, [Value], ResultLeft) ; 
                                                             checkLeftPossib(Board, Dim, Row, Col, ResultLeft, NewCounter)).
checkLeftPossib(_, _, _, _, _, _).

  
checkUpPossib(Board, Dim, Row, Col, ResultUp, Counter):-  NewRow is Row - Counter,
                                                            NewRow >= 1,
                                                            NewCounter is Counter + 1,
                                                            getCell(Board, NewRow, Col, Value, Dim),
                                                            Diff is Row - NewRow,
                                                            (Diff = Value -> checkUpPossib(Board, Dim, Row, Col, TempResult, NewCounter),
                                                                             append(TempResult, [Value], ResultUp) ; 
                                                            checkUpPossib(Board, Dim, Row, Col, ResultUp, NewCounter)). 
checkUpPossib(_, _, _, _, _, _).  
 

checkDownPossib(Board, Dim, Row, Col, ResultDown, Counter):-  NewRow is Row + Counter,
                                                              NewRow =< Dim,
                                                              NewCounter is Counter + 1,
                                                              getCell(Board, NewRow, Col, Value, Dim),
                                                              Diff is NewRow - Row,
                                                              (Diff = Value -> checkDownPossib(Board, Dim, Row, Col, TempResult, NewCounter),
                                                                               append(TempResult, [Value], ResultDown) ; 
                                                              checkDownPossib(Board, Dim, Row, Col, ResultDown, NewCounter)).
checkDownPossib(_, _, _, _, _, _). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Restrict each solution board cell so there cant be any adjacent pieces %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% First Collumn
adjacents([[H0Row0, H1Row0 | TRow0], [H0Row1 | TRow1] | TRows]):- ((H0Row0 #= 0 #/\ H1Row0 #= 0) #\/
																(H0Row0 #= 0 #/\ H1Row0 #\= 0) #\/
																(H1Row0 #= 0 #/\ H0Row0 #\= 0))
																#/\
																((H0Row0 #= 0 #/\ H0Row1 #= 0) #\/
																(H0Row0 #= 0 #/\ H0Row1 #\= 0) #\/
																(H0Row1 #= 0 #/\ H0Row0 #\= 0)),
																adjacents([[H1Row0 | TRow0], TRow1 | TRows], [H0Row1 | TRow1]).


% Last Cells
adjacents([[H0Row, H1Row | []] | []]):- (H0Row #= 0 #/\ H1Row #= 0) #\/
										(H0Row #= 0 #/\ H1Row #\= 0) #\/
										(H1Row #= 0 #/\ H0Row #\= 0).


% Last Row
adjacents([[H0Row, H1Row | TRow] | []]):- (H0Row #= 0 #/\ H1Row #= 0) #\/
										(H0Row #= 0 #/\ H1Row #\= 0) #\/
										(H1Row #= 0 #/\ H0Row #\= 0),
										adjacents([[H1Row | TRow] | []]).


adjacents([[H0Row0, H1Row0 | TRow0], [H0Row1 | TRow1] | TRows], SavedList):- ((H0Row0 #= 0 #/\ H1Row0 #= 0) #\/
																			(H0Row0 #= 0 #/\ H1Row0 #\= 0) #\/
																			(H1Row0 #= 0 #/\ H0Row0 #\= 0))
																			#/\
																			((H0Row0 #= 0 #/\ H0Row1 #= 0) #\/
																			(H0Row0 #= 0 #/\ H0Row1 #\= 0) #\/
																			(H0Row1 #= 0 #/\ H0Row0 #\= 0)),
																			adjacents([[H1Row0 | TRow0], TRow1 | TRows], SavedList).


% When going to Last Row
adjacents([[H0Row0 | []], [H0Row1 | []] | []], SavedList):- (H0Row0 #= 0 #/\ H0Row1 #= 0) #\/
															(H0Row0 #= 0 #/\ H0Row1 #\= 0) #\/
															(H0Row1 #= 0 #/\ H0Row0 #\= 0),
															adjacents([SavedList]).


% Last Collumn
adjacents([[H0Row0 | []], [H0Row1 | []] | TRows], SavedList):-  (H0Row0 #= 0 #/\ H0Row1 #= 0) #\/
																(H0Row0 #= 0 #/\ H0Row1 #\= 0) #\/
																(H0Row1 #= 0 #/\ H0Row0 #\= 0),
																append([SavedList], TRows, Board),
																adjacents(Board).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Restrict each cell with the respective previously calculated possibilities %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Last Cell
restrict([[HRLast | []] | []], [HCLast | []]):- restrict_cell(HRLast, HCLast).


restrict([[H0Row0 | TRow0] | TRows], [HCell | TCells]):- restrict_cell(H0Row0, HCell),
														restrict([TRow0 | TRows], TCells).


% Last Collumn
restrict([[H0Row0 | []] | TRows], [HCell | TCells]):- restrict_cell(H0Row0, HCell),
													restrict(TRows, TCells).


% Last Row
restrict([[H0Row | TRow] | []], [HCell | TCells]):- restrict_cell(H0Row, HCell),
													restrict(TRow, TCells).

restrict_cell(HRow, HCell):- list_to_fdset(HCell, Set),
							HRow in_set Set.