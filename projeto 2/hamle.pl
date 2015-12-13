
includeUtilities:- include('utilities.pl').
:- use_module(library(lists)).


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
board([[0,2,3,0,5,0,0],[1,0,3,0,5,0,0],[1,2,3,4,5,0,0],[0,2,0,4,0,0,0],[1,0,0,0,5,4,4],[1,0,0,0,5,4,4],[1,0,0,0,5,4,4]]).


% Generator

/*
 * Create an empty board
 */
generateBoard(_,0,[]).
generateBoard(Dimension, Counter, Board) :-
        length(Row, Dimension),
        NewCounter is Counter - 1,
        generateBoard(Dimension, NewCounter, T1),
        append([Row], T1, Board).

% Solver

solve([HRow | TRows], Sol):-
        getBlackPieces(Board, BlackList),
        length(BlackList, BlackNumber),
        length(HRow, Dimension),
        length(Cols, BlackNumber),
        length(Rows, BlackNumber),
        domain(Cols, 1, Dimension),
        domain(Rows, 1, Dimension),
        tryToSolve(Cols, Rows, BlackList),
        append(Sol, SolFlat)
		labeling([], SolFlat).




tryToSolve([Col1 | Cols], [Row1 | Rows], BlackList):-
        tryToSolvePiece(Col1, Row1, BlackList),
        tryToSolve(Cols, Rows, BlackList).




getBlackPieces([Hrow | Trows], List) :-
        length(Hrow, Dimension),
        getBlackPieces([Hrow | Trows], Dimension, 1, List).

getBlackPieces([], _, _, []).

getBlackPieces([Hrow | Trows], Dimension, Row, List) :-
        getBlackPiecesRow(Hrow, Dimension, Row, RowList),
        NewRow is Row + 1,
        getBlackPieces(Trows, Dimension, NewRow, BoardList),
        append(RowList, BoardList, List).

getBlackPiecesRow(RowList, Dimension, Row, List):-
        getBlackPiecesRow(RowList, Dimension, Row, 1, List).

getBlackPiecesRow(RowList, Dimension, Row, Dimension, List):-
        nth1(Dimension, RowList, Number),
        (Number > 0 -> append([], [[Row, Dimension, Number]], TempList)
        ;
        append([],[],TempList)),
        append([], TempList, List).

getBlackPiecesRow(RowList, Dimension, Row, Col, List):-
        nth1(Col, RowList, Number),
        (Number > 0 -> append([], [[Row, Col, Number]], TempList)
        ;
        append([],[],TempList)),
        NewCol is Col + 1,
        getBlackPiecesRow(RowList, Dimension, Row, NewCol, RestList),
        append(TempList, RestList, List).