:- module(basic_list_techniques_mapping, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

square(X, R) :-
    R is X * X.

cube(X, R) :-
    R is X * X * X.

map_element(X, XMapped) :-
    square(X, XSquare),
    cube(X, XCube),
    XMapped = [X, square, XSquare, cube, XCube].

map([], []).

map([Head|Tail], Mapped) :-
    map(Tail, TailMapped),
    map_element(Head, HeadMapped),
    Mapped = [HeadMapped|TailMapped].


:- begin_tests(basic_list_techniques_mapping_map).

test(square) :-
    square(1, 1),
    square(3, 9).

test(cube) :-
    cube(1, 1),
    cube(3, 27).

test(map_element) :-
    map_element(1, [1, square, 1, cube, 1]),
    map_element(3, [3, square, 9, cube, 27]).

test(map) :-
    map([], []),
    map([1], [[1, square, 1, cube, 1]]),
    map([1,2,3], [[1, square, 1, cube, 1], [2, square, 4, cube, 8], [3, square, 9, cube, 27]]).

:- end_tests(basic_list_techniques_mapping_map).
