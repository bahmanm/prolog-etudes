:- module(basic_list_techniques_mapping_nested, []).

direct_command(ugluk, azog).
direct_command(ugluk, grishnakh).
direct_command(ugluk, snaga).

direct_command(azog, balcmeg).
direct_command(grishnakh, lagduf).
direct_command(grishnakh, golfimbul).
direct_command(snaga, ufthak).

direct_command(balcmeg, muzgash).
direct_command(ufthak, shagrat).
direct_command(ufthak, bolg).

direct_command(bolg, radbug).
direct_command(bolg, golbag).

direct_command(mauhur, orcobal).
direct_command(mauhur, othrod).

direct_command(orcobal, lug).
direct_command(orcobal, gorgol).
direct_command(orcobal, zogbag).

direct_command(gorgol, boldog).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all_commands_list(Orcs, AllCommands) :-
    all_commands_list1(Orcs, [], AllCommands).

all_commands_list1([], Acc, Acc).

all_commands_list1([Orc|Orcs], Acc, AllCommands) :-
    all_commands(Orc, OrcAllCommands),
    all_commands_list1(Orcs, [OrcAllCommands|Acc], AllCommands).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all_commands(Orc, AllCommands) :-
    all_commands1([Orc], [], AllCommands1),
    remove_last(AllCommands1, AllCommands).

all_commands1([], Acc, Acc).
all_commands1([Orc|Orcs], Acc, Commands) :-
    findall(C, direct_command(Orc, C), DCs),
    append(Orcs, DCs, NewOrcs),
    all_commands1(NewOrcs, [Orc|Acc], Commands).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remove_last([], []).
remove_last(L, Result) :-
    remove_last(L, [], Result).

remove_last([_|[]], Acc, Acc).
remove_last([H|T], Acc, Result) :-
    remove_last(T, [H|Acc], Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(basic_list_techniques_mapping_nested).

test(all_commands_list, nondet) :-
    all_commands_list([azog, bolg], [[radbug, golbag], [balcmeg, muzgash]]),
    all_commands_list([boldog], [[]]),
    all_commands_list([], []).

test(all_commands, nondet) :-
    all_commands(snaga, [ufthak, shagrat, bolg, radbug, golbag]),
    all_commands(boldog, []).

test(remove_last, nondet) :-
    remove_last([a, b, c], [b, a]),
    remove_last([a], []).

:- end_tests(basic_list_techniques_mapping_nested).
