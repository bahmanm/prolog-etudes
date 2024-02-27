:- module(basic_list_techniques_diff_list, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Write a predicate append_diff(L1, L2, Result) that appends two difference lists and returns the
% combined list.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

append_diff(OpenList1-OpenList2, OpenList2-Hole2, Result-ResultHole) :-
    nonvar(OpenList2),
    !,
    Result = OpenList1,
    ResultHole = Hole2.

append_diff(OpenList1-Hole1, OpenList2-Hole2, Result-ResultHole) :-
    var(Hole1),
    !,
    Hole1 = OpenList2,
    Result-ResultHole = OpenList1-Hole2.

append_diff(OpenList1-Hole1, OpenList2-Hole2, Result-ResultHole) :-
    nonvar(Hole1),
    !,
    append(OpenList1, OpenList2, Result),
    ResultHole = Hole2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(basic_list_techniques_diff_list__append_diff).

test(append_diff__unbound_holes_1) :-
    append_diff([a,b|Hole1]-Hole1, [c,d|Hole2]-Hole2, [a,b,c,d]-[]).

test(append_diff__unbound_holes_2) :-
    append_diff(Hole1-Hole1, [c,d|Hole2]-Hole2, [c,d]-[]).

test(append_diff__unbound_holes_3) :-
    append_diff(Hole1-Hole1, Hole2-Hole2, []-[]).

test(append_diff__bound_holes_1) :-
    Hole1=[x],
    Hole2=[],
    append_diff([a|Hole1]-Hole1, [c|Hole2]-Hole2, [a,x,c]-Hole2).

test(append_diff__bound_holes_2) :-
    Hole1=[x],
    Hole2=[y],
    append_diff(Hole1-Hole1, [c,d|Hole2]-Hole2, [x,c,d,y]-Hole2).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(basic_list_techniques_diff_list__reverse_diff).

test(reverse_diff__unbound_hole) :-
    reverse_diff([a|Hole1]-Hole1, [a]),
    reverse_diff([a, b, c|Hole2]-Hole2, [c, b, a]).

test(reverse_diff__bound_hole) :-
    Hole1 = [], reverse_diff([a|Hole1]-Hole1, [a]),
    Hole2 = [d, e], reverse_diff([a, b, c|Hole2]-Hole2, [e, d, c, b, a]).

:- end_tests(basic_list_techniques_diff_list__reverse_diff).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Write a predicate filter_even_diff(L, Even) that removes all odd numbers from a difference list
% and returns the list containing only even numbers in Even.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

filter_even_diff(OpenList-Hole, Evens) :-
    var(Hole),
    Hole = [],
    !,
    filter_even_diff_helper(OpenList, AccHole-AccHole, Evens-[]).

filter_even_diff(OpenList-Hole, Evens) :-
    nonvar(Hole),
    !,
    filter_even_diff_helper(OpenList, AccHole-AccHole, Evens-[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

filter_even_diff_helper([], Acc-AccHole, Acc-AccHole).

filter_even_diff_helper([Head|Tail], Acc-AccHole, Evens) :-
    0 is Head mod 2,
    !,
    append_diff(Acc-AccHole, [Head|NewHole]-NewHole, NewAcc-NewHole),
    filter_even_diff_helper(Tail, NewAcc-NewHole, Evens).

filter_even_diff_helper([_|Tail], Acc-AccHole, Evens) :-
    filter_even_diff_helper(Tail, Acc-AccHole, Evens).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(basic_list_techniques_diff_list__filter_even_diff).

test(filter_even_diff__unbound_hole) :-
    filter_even_diff([1|Hole1]-Hole1, []),
    filter_even_diff([1, 2, 3, 4|Hole2]-Hole2, [2, 4]).

test(filter_even_diff__bound_hole) :-
    Hole1 = [3], filter_even_diff([1|Hole1]-Hole1, []),
    Hole2 = [6, 8, 9], filter_even_diff([1, 2, 3, 4|Hole2]-Hole2, [2, 4, 6, 8]).

:- end_tests(basic_list_techniques_diff_list__filter_even_diff).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Write a predicate min_diff(L, Min) that finds the minimum element in a difference list and
% returns it in Min.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

min_diff(OpenList-Hole, Min) :-
    var(Hole),
    !,
    Hole = [],
    [H|T] = OpenList,
    min_diff_helper(T, H, Min).

min_diff(OpenList-Hole, Min) :-
    nonvar(Hole),
    !,
    [H|T] = OpenList,
    min_diff_helper(T, H, Min).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

min_diff_helper([], Min, Min).

min_diff_helper([H|T], MinSoFar, Min) :-
    H < MinSoFar,
    !,
    min_diff_helper(T, H, Min).

min_diff_helper([_|T], MinSoFar, Min) :-
    min_diff_helper(T, MinSoFar, Min).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(basic_list_techniques_diff_list__min_diff).

test(min_diff__unbound_hole) :-
    min_diff([3|Hole1]-Hole1, 3),
    min_diff([3, 1, 2|Hole2]-Hole2, 1).

test(min_diff__bound_hole) :-
    Hole1 = [], min_diff([3|Hole1]-Hole1, 3),
    Hole2 = [5, 0], min_diff([3, 1, 2|Hole2]-Hole2, 0).

:- end_tests(basic_list_techniques_diff_list__min_diff).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Write a predicate insert_diff(X, Index, L, New) that inserts an element X at a specific Index
% (starting from 1) in a difference list and returns the resulting list in New.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_diff(X, Index, OpenList-Hole, ResultList-ResultHole) :-
    var(Hole),
    Hole = [],
    insert_diff_helper(X, 1, Index, OpenList, AccHole-AccHole, ResultList-ResultHole),
    !.

insert_diff(X, Index, OpenList-Hole, ResultList-ResultHole) :-
    nonvar(Hole),
    insert_diff_helper(X, 1, Index, OpenList, AccHole-AccHole, ResultList-ResultHole),
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_diff_helper(_, _, _, [], ResultList-ResultHole, ResultList-ResultHole).

insert_diff_helper(_, CurrentIndex, Index, List, Acc-AccHole, ResultList-ResultHole) :-
    CurrentIndex > Index,
    append_diff(Acc-AccHole, List-NewAccHole, NewAcc-NewAccHole),
    insert_diff_helper(_, CurrentIndex, Index, [], NewAcc-NewAccHole, ResultList-ResultHole).

insert_diff_helper(X, CurrentIndex, Index, List, Acc-AccHole, ResultList-ResultHole) :-
    CurrentIndex = Index,
    NewCurrentIndex is CurrentIndex + 1,
    append_diff(Acc-AccHole, [X|NewAccHole]-NewAccHole, NewAcc-NewAccHole),
    insert_diff_helper(X, NewCurrentIndex, Index, List, NewAcc-NewAccHole, ResultList-ResultHole).

insert_diff_helper(X, CurrentIndex, Index, [H|T], Acc-AccHole, ResultList-ResultHole) :-
    CurrentIndex < Index,
    NewCurrentIndex is CurrentIndex + 1,
    append_diff(Acc-AccHole, [H|NewAccHole]-NewAccHole, NewAcc-NewAccHole),
    insert_diff_helper(X, NewCurrentIndex, Index, T, NewAcc-NewAccHole, ResultList-ResultHole).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(basic_list_techniques_diff_list__insert_diff).

test(insert_diff__unbound_hole) :-
    insert_diff(x, 1, [a|Hole1]-Hole1, [x, a]-[]),
    insert_diff(x, 2, [a|Hole2]-Hole2, [a, x]-[]),
    insert_diff(x, 3, [a, b, c|Hole3]-Hole3, [a, b, x, c]-[]),
    insert_diff(x, 1, Hole4-Hole4, [x]-[]).

test(insert_diff__bound_hole) :-
    Hole1 = [], insert_diff(x, 1, [a|Hole1]-Hole1, [x, a]-[]),
    Hole2 = [], insert_diff(x, 2, [a|Hole2]-Hole2, [a, x]-[]),
    Hole3 = [d, e], insert_diff(x, 3, [a, b, c|Hole3]-Hole3, [a, b, x, c, d, e]-[]),
    Hole4 = [], insert_diff(x, 1, Hole4-Hole4, [x]-[]).

:- end_tests(basic_list_techniques_diff_list__insert_diff).
