
includeUtilities:- include('utilities.pl').
:-use_module(library(lists)).
:-use_module(library(clpfd)).
:- use_module(library(random)).


% Running Game %

playGame:- clearScreen(100),
           menu,
           write('>'),
           read(Choice), nl, Choice > 0, Choice =< 3, startGame(Choice).


% Functions that allow Start Or Exit the game %


startGame(1):- clearScreen(100), % Starting Game
               write('Choose dimension board '),
               read(Dimension), 
               Dimension > 0,
               generateBoard(Dimension). 

startGame(2):- clearScreen(100), rules, % Showing rules
               read(Exit), Exit > -1, 
               playGame. 
        
startGame(3):- write('Exiting Game!'). % Exiting game


%Test purposes%

board( [[0,1,2,3,4,0,0],
        [1,0,0,0,0,0,0],
        [0,1,3,4,5,0,0],
        [1,0,0,4,0,0,0],
        [1,4,0,0,5,4,4],
        [1,0,0,0,5,4,4],
        [1,0,0,0,5,4,4]]).



% Solver

solve_board(Board, Sol):-
        length(Board, Dimension),
        create_board(Sol, Dimension).
        %get_black_pieces(Board, BlackList),
        %get_possibilities(Board, Dimension, PossibList),
        %adjacents(Sol),
        %restrict(Sol, PossibList),
        %append(Sol, SolFlat)
		%labeling([], SolFlat).


create_board(Board, N):- length(Board, N), maplist(set_length(N), Board).

set_length(L, Ls) :- length(Ls, L).



get_possib(Board, Dim, List):- get_possib_cell(Board, Dim, 1, 1, List).


get_possib_cell(Board, Dim, Dim, Dim, List):- make_possib_list(Board, Dim, Dim, Dim, TempList),
											append([], [[TempList]], List).


get_possib_cell(Board, Dim, Row, Dim, List):- make_possib_list(Board, Dim, Row, Dim, TempList),
											NewRow is Row + 1,
											get_possib_cell(Board, Dim, NewRow, 1, RestList),
											append([[TempList]], RestList, List).


get_possib_cell(Board, Dim, Row, Col, List):- make_possib_list(Board, Dim, Row, Col, TempList),
											NewCol is Col + 1,
											get_possib_cell(Board, Dim, Row, NewCol, RestList),
											append([[TempList]], RestList, List).



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


% Last Cell
adjacents([[_]]).


adjacents([[H0Row0, H1Row0 | TRow0], [H0Row1 | TRow1] | TRows]):-
	(H0Row0 + H1Row0 #= 0 #\/ (H0Row0 #= 0 #/\ H1Row0 #\= 0) #\/ (H1Row0 #= 0 #/\ H0Row0 #\= 0)) #/\
	(H0Row0 + H0Row1 #= 0 #\/ (H0Row0 #= 0 #/\ H0Row1 #\= 0) #\/ (H0Row1 #= 0 #/\ H0Row0 #\= 0)),
	adjacents([[H1Row0 | TRow0], [TRow1 | TRows]]).


% Last Collumn
adjacents([[H0Row0], H0Row1 | TRows]) :-
	H0Row0 + H0Row1 #= 0 #\/ (H0Row0 #= 0 #/\ H0Row1 #\= 0) #\/ (H0Row1 #= 0 #/\ H0Row0 #\= 0),
	adjacents([H0Row1 | TRows]).


% Last Row
/*adjacents([[H0Row, H1Row | TRow]]) :-
	H0Row0 + H1Row0 #= 0 #\/ (H0Row0 #= 0 #/\ H1Row0 #\= 0) #\/ (H1Row0 #= 0 #/\ H0Row0 #\= 0),
	adjacents([[H1Row | Trow]]).*/



% restrict([[H0Row0 | TRow0] , [Row1 | TRows]], [HCell | TCells]) :- 

/* Check Range Black pieces */

checkRange(Board, Row, Col, Dimension):- checkRight(Board, Row, Col, Dimension),
                                         checkLeft(Board, Row, Col, Dimension),
                                         checkUp(Board, Row, Col, Dimension),
                                         checkDown(Board, Row, Col, Dimension).


checkRight(Board, Row, Col, Dimension):- ((Col < Dimension, NewCol is Col + 1,
                                          getCell(Board, Row, NewCol, Cell, Dimension), 
                                          Cell > 0)) -> false; true.

checkLeft(Board, Row, Col, Dimension):- ((Col > 1, NewCol is Col - 1,
                                         getCell(Board, Row, NewCol, Cell, Dimension), 
                                         Cell > 0)) -> false; true.

checkUp(Board, Row, Col, Dimension):- ((Row > 1, NewRow is Row - 1,
                                       getCell(Board, NewRow, Col, Cell, Dimension), 
                                       Cell > 0)) -> false; true.

checkDown(Board, Row, Col, Dimension):- ((Row < Dimension, NewRow is Row + 1,
                                       getCell(Board, NewRow, Col, Cell, Dimension), 
                                       Cell > 0)) -> false; true.

/* Check the distance to the edges */

calcEdgeDistances(Dimension, Row, Col, Result):- DistanceRight is Dimension - Col, 
                                                 DistanceLeft is Col - 1,
                                                 DistanceUp is Row - 1,
                                                 DistanceDown is Dimension - Row,
                                                 X = [[DistanceRight],[DistanceLeft],[DistanceUp],[DistanceDown]],
                                                 append(X,Result).




% Solver

/* Get Black Pieces */
get_black_pieces([Hrow | Trows], List) :-
        length(Hrow, Dimension),
        get_black_pieces([Hrow | Trows], Dimension, 1, List).

get_black_pieces([], _, _, []).

get_black_pieces([Hrow | Trows], Dimension, Row, List) :-
        get_black_pieces_row(Hrow, Dimension, Row, RowList),
        NewRow is Row + 1,
        get_black_pieces(Trows, Dimension, NewRow, BoardList),
        append(RowList, BoardList, List).

get_black_pieces_row(RowList, Dimension, Row, List):-
        get_black_pieces_row(RowList, Dimension, Row, 1, List).

get_black_pieces_row(RowList, Dimension, Row, Dimension, List):-
        nth1(Dimension, RowList, Number),
        (Number > 0 -> append([], [[Row, Dimension, Number]], TempList)
        ;
        append([],[],TempList)),
        append([], TempList, List).

get_black_pieces_row(RowList, Dimension, Row, Col, List):-
        nth1(Col, RowList, Number),
        (Number > 0 -> append([], [[Row, Col, Number]], TempList)
        ;
        append([],[],TempList)),
        NewCol is Col + 1,
        get_black_pieces_row(RowList, Dimension, Row, NewCol, RestList),
        append(TempList, RestList, List).
