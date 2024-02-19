:- module(basic_list_techniques_recursion, []).

isMember(Element, [Element|_]).
isMember(Element, [_|Tail]) :- isMember(Element, Tail).

:- begin_tests(basic_list_techniques_recursion).

test(isMember) :-
    \+ isMember(_, [])
    , isMember([], [[]])
    , isMember(d, [a, b, c, d])
    , \+ isMember(x, [a, b, c]).

:- end_tests(basic_list_techniques_recursion).
