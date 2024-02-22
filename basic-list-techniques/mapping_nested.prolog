:- module(basic_list_techniques_mapping_nested, []).

commands(ugluk, azog).
commands(ugluk, grishnakh).
commands(ugluk, snaga).

commands(azog, balcmeg).
commands(grishnakh, lagduf).
commands(grishnakh, golfimbul).
commands(snaga, ufthak).

commands(balcmeg, muzgash).
commands(ufthak, shagrat).
commands(ufthak, bolg).

commands(bolg, radbug).
commands(bolg, golbag).

commands(mauhur, orcobal).
commands(mauhur, othrod).

commands(orcobal, lug).
commands(orcobal, gorgol).
commands(orcobal, zogbag).

commands(gorgol, boldog).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

chain_of_command(Orc, Result) :-
    c_o_c(Orc, Commanders),
    append(Commanders, [Orc], Result).

c_o_c(Orc, []) :-
    \+ commands(_, Orc).

c_o_c(Orc, Commanders) :-
    commands(DirectCommander, Orc),
    c_o_c(DirectCommander, IndirectCommanders),
    append(IndirectCommanders, [DirectCommander], Commanders).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(basic_list_techniques_mapping_nested).

test(chain_of_command__works_ok, nondet) :-
    chain_of_command(orcobal, [mauhur, orcobal]),
    chain_of_command(golbag, [ugluk, snaga, ufthak, bolg, golbag]),
    chain_of_command(boldog, [mauhur, orcobal, gorgol, boldog]).

test(chain_of_command__no_commander, nondet) :-
    chain_of_command(ugluk, [ugluk]),
    chain_of_command(mauhur, [mauhur]).

:- end_tests(basic_list_techniques_mapping_nested).
