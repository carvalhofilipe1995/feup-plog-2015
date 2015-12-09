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



% Getting Cell

getCell([ StartLine | _ ], 1, Collumn, Cell, Dimension):-
        getCellCol(StartLine, Collumn, Cell, Dimension).

getCell([ _ | RestBoard ], Row, Collumn, Cell, Dimension):-
        Row > 0, Row =< Dimension,
        Collumn > 0, Collumn =< Dimension,
        NewRow is Row - 1,
        getCell(RestBoard, NewRow, Collumn, Cell, Dimension).

getCellCol([TheCell | _ ], 1, TheCell, _).

getCellCol([ _ | RestLine ], Coll, Cell, Dimension):-
                Coll > 0, Coll =< Dimension,
                NewColl is Coll - 1,
                getCellCol(RestLine, NewColl, Cell, Dimension).


% Empty Cell

emptyCell([ StartLine |  RestBoard], 1, Collumn, Dimension, [ NewBoardLine | RestBoard]):-
        emptyCellColl(StartLine, Collumn, Dimension, NewBoardLine).
                                                            
emptyCell([ StartBoard |  RestBoard], Row, Collumn, Dimension, [ StartBoard| RestNewBoard]):-
        Row > 0, Row =< Dimension,
        Collumn > 0, Collumn =< Dimension,
        NewRow is Row - 1,
        emptyCell(RestBoard, NewRow, Collumn, Dimension, RestNewBoard).
                
emptyCellColl([ _ | RestLine ], 1, _, [0 | RestLine]).        
emptyCellColl([StartLine | RestLine], Collumn, Dimension, [ StartLine | RestNewBoard]):-
               Collumn > 0, Collumn =< Dimension,
               NewColl is Collumn - 1,
               emptyCellColl(RestLine, NewColl, Dimension, RestNewBoard).     
        
        
        
        
        
        
        
                               












