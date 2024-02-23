:- module(basic_list_techniques_recursion, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

member(Element, [Element|_]).

member(Element, [_|Tail]) :-
    member(Element, Tail).


:- begin_tests(basic_list_techniques_recursion_member).

test(member, nondet) :-
    member(a, [a])
    , member(d, [a, b, c, d])
    , \+ member(x, [a, b, c])
    , \+ member(_, []).

:- end_tests(basic_list_techniques_recursion_member).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list([]).

list([_|Tail]) :-
    list(Tail).

:- begin_tests(basic_list_techniques_recursion_list).

test(list) :-
    list([]),
    list([1]),
    list([[]]),
    \+ list(5).

:- end_tests(basic_list_techniques_recursion_list).
