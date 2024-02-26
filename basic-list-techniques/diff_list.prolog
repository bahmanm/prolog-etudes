:- module(basic_list_techniques_diff_list, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Write a predicate append_diff(L1, L2, Result) that appends two difference lists and returns the
% combined list.
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
% Write a predicate length_diff(L, Length) that calculates the length of a difference list and
% returns it in the variable Length.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

length_diff(OpenList-Hole, Length) :-
    var(Hole),
    !,
    OpenList-Hole = ProperList-[],
    length(ProperList, Length).

length_diff(OpenList-Hole, Length) :-
    nonvar(Hole),
    !,
    OpenList = ProperList,
    length(ProperList, Length).

:- begin_tests(basic_list_techniques_diff_list__length_diff).

test(length_diff__unbound_hole_1) :-
    length_diff([a, b, c|Hole1]-Hole1, 3),
    length_diff([a|Hole2]-Hole2, 1).

test(length_diff__bound_hole_1) :-
    Hole1 = [d, e], length_diff([a, b, c|Hole1]-Hole1, 5).

:- end_tests(basic_list_techniques_diff_list__length_diff).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Write a predicate member_diff(X, L) that checks if an element X is present in a difference list L.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

member_diff(X, OpenList-Hole) :-
    var(Hole),
    var(X),
    Hole = [],
    !,
    member(X, OpenList).

member_diff(X, OpenList-Hole) :-
    var(Hole),
    nonvar(X),
    Hole = [],
    member(X, OpenList),
    !.

member_diff(X, OpenList-Hole) :-
    nonvar(Hole), !,
    member(X, OpenList).

:- begin_tests(basic_list_techniques_diff_list__member_diff).

test(member_diff__unbound_hole) :-
    member_diff(a, [a|Hole1]-Hole1),
    member_diff(a, [a, b, c|Hole2]-Hole2).

test(member_diff__unbound_hole_unbound_X) :-
    findall(X, member_diff(X, [a|Hole1]-Hole1), Result1), Result1 = [a],
    findall(X, member_diff(X, [a, b, c|Hole2]-Hole2), Result2), Result2 = [a, b, c].

test(member_diff__bound_hole) :-
    Hole1 = [], member_diff(a, [a|Hole1]-Hole1),
    Hole2 = [b, c], member_diff(c, [a|Hole2]-Hole2).

test(member_diff__bound_hole_unbound_X) :-
    Hole1 = [], findall(X, member_diff(X, [a|Hole1]-Hole1), Result1), Result1 = [a],
    Hole2 = [b, c], findall(X, member_diff(X, [a|Hole2]-Hole2), Result2), Result2 = [a, b, c].

:- end_tests(basic_list_techniques_diff_list__member_diff).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Write a predicate reverse_diff(L, Reversed) that reverses a difference list and returns the
% reversed version.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reverse_diff(OpenList-Hole, Reversed) :-
    var(Hole),
    !,
    Hole = [],
    reverse(OpenList, Reversed).

reverse_diff(OpenList-Hole, Reversed) :-
    nonvar(Hole),
    !,
    reverse(OpenList, Reversed).

:- begin_tests(basic_list_techniques_diff_list__reverse_diff).

test(reverse_diff__unbound_hole) :-
    reverse_diff([a|Hole1]-Hole1, [a]),
    reverse_diff([a, b, c|Hole2]-Hole2, [c, b, a]).

test(reverse_diff__bound_hole) :-
    Hole1 = [], reverse_diff([a|Hole1]-Hole1, [a]),
    Hole2 = [d, e], reverse_diff([a, b, c|Hole2]-Hole2, [e, d, c, b, a]).

:- end_tests(basic_list_techniques_diff_list__reverse_diff).
