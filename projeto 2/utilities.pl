/*
 *      Utilities File
 */ 


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


showBoard([], 0, _, DimDivision):- showDivision(DimDivision). 
showBoard(Board, Dim, 0, DimDivision):- showColls(Dim,1),
                                        showBoard(Board, Dim, 1, DimDivision).
showBoard([StartBoard|EndBoard], Dim, Increment, DimDivision) :-
                showDivision(DimDivision),
                writeLineNumber(Increment),
                showLine(StartBoard),
                NewIncrement is Increment + 1,
                NewDim is Dim - 1,
                showBoard(EndBoard, NewDim, NewIncrement, DimDivision).



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
                Coll > 1, Coll =< Dimension,
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
        

% Set Cell    
setCell([ StartLine |  RestBoard], 1, Collumn, Dimension, [ NewBoardLine | RestBoard], Change):-
        setCellColl(StartLine, Collumn, Dimension, NewBoardLine, Change).
                                                            
setCell([ StartBoard |  RestBoard], Row, Collumn, Dimension, [ StartBoard| RestNewBoard], Change):-
        Row > 0, Row =< Dimension,
        Collumn > 0, Collumn =< Dimension,
        NewRow is Row - 1,
        setCell(RestBoard, NewRow, Collumn, Dimension, RestNewBoard, Change).
                
setCellColl([ _ | RestLine ], 1, _, [Change | RestLine], Change).        
setCellColl([StartLine | RestLine], Collumn, Dimension, [ StartLine | RestNewBoard], Change):-
               Collumn > 0, Collumn =< Dimension,
               NewColl is Collumn - 1,
               setCellColl(RestLine, NewColl, Dimension, RestNewBoard, Change).



                               












