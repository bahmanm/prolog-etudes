:- module(chapter3_main, [isList/1]).

isList([]) :- true.
isList([_|T]) :- isList(T).


:- begin_tests(chapter3_main).

test(isList_simple) :-
    isList([])
    , isList([[]]).

test(isList_elaborate) :-
    isList([a,b,[c,d]])
    , \+ isList(5).

:- end_tests(chapter3_main).
