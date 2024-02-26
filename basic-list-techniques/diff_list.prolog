:- module(basic_list_techniques_diff_list, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

append_diff(OpenList1-Hole1, OpenList2-Hole2, OpenList1-Hole2) :-
    Hole1 = OpenList2.

:- begin_tests(basic_list_techniques_diff_list__append_diff).

test(append_diff__1) :-
    append_diff([a,b|Hole1]-Hole1, [c,d|Hole2]-Hole2, [a,b,c,d]-[]).

test(append_diff__2) :-
    append_diff(Hole1-Hole1, [c,d|Hole2]-Hole2, [c,d]-[]).

test(append_diff__3) :-
    append_diff(Hole1-Hole1, Hole2-Hole2, []-[]).

:- end_tests(basic_list_techniques_diff_list__append_diff).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

length_diff(OpenList-Hole, Length) :-
    var(Hole),
    !,
    OpenList-Hole = ProperList-[],
    length(ProperList, Length).

length_diff(OpenList-Hole, Length) :-
    nonvar(Hole),
    !,
    ProperList = OpenList,
    length(ProperList, Length).

:- begin_tests(basic_list_techniques_diff_list__length_diff).

test(length_diff__unbound_hole_1) :-
    length_diff([a, b, c|Hole1]-Hole1, 3),
    length_diff([a|Hole2]-Hole2, 1).

test(length_diff__bound_hole_1) :-
    Hole1 = [d, e], length_diff([a, b, c|Hole1]-Hole1, 5).

:- end_tests(basic_list_techniques_diff_list__length_diff).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
