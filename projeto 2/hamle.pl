
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



