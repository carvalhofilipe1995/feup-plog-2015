/*
 *      Utilities File
 */ 

board([[0,2,3,0,5,0,0],[1,0,3,0,5,0,0],[1,2,3,4,5,0,0],[0,2,0,4,0,0,0],[1,0,0,0,5,4,4],[1,0,0,0,5,4,4],[1,0,0,0,5,4,4]]).


%-- Printing board game --%

writeLineNumber(Number):- write('| L'), write(Number), write(' |'). 

writeCell(0):-  write(' '), write(' '), write(' |').
writeCell(Number):- write(' '), write(Number), write(' |').

drawDivision:- write('-----').

showCollNumbers(0,_).
showCollNumbers(Number,Increment):- write(' C'), write(Increment), write(' |'),
                          NewNumber is Number - 1,
                          NewIncrement is Increment + 1,
                          showCollNumbers(NewNumber,NewIncrement).

showColls(Number, Increment):- write('     |'),
                    showCollNumbers(Number,Increment),nl.
                             

showDivision(0):- write('------'), nl.
showDivision(Number):- drawDivision,
                       NewNumber is Number - 1,
                       showDivision(NewNumber).


showLine([]):- nl.
showLine([LineStart| LineEnd]):-
        write(' '),
        writeCell(LineStart),
        showLine(LineEnd).


showBoard([], 0, _, _). 
showBoard([StartBoard|EndBoard], Dimension, Increment, DimensionDivision) :-
                showDivision(DimensionDivision),
                writeLineNumber(Increment),
                showLine(StartBoard),
                NewIncrement is Increment + 1,
                NewDimension is Dimension - 1,
                showBoard(EndBoard, NewDimension, NewIncrement, DimensionDivision).


printBoard(Dimension):- board(X), showColls(Dimension,1), showBoard(X,Dimension,1,Dimension).

