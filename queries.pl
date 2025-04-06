label(N,M,L) :- format("Monster ~d (~d x ~d): ", [N,M,L]).

max_otter_proofs([], SoFar, SoFar).
max_otter_proofs([o(_,Steps)|Rest], SoFar, Max) :- NewSoFar is max(SoFar, Steps), max_otter_proofs(Rest, NewSoFar, Max). 

max_otter_diffs([], SoFar, SoFar).
max_otter_diffs([o(_Num, d(D1,D2))|Rest], SoFar, Result) :- NewSoFar is max(SoFar, D1+D2), max_otter_diffs(Rest, NewSoFar, Result). 

measure(Monsters, Dir, Suffix, Options) :-
    ( member(Monsters, [test,quick,slow,extra,nutters,extras]) -> string_concat(Monsters, ".csv", File) ; File = "special.csv" ), 
    string_concat(Dir, File, FileName), open(FileName, write, Stream),  format(Stream, "Number,Time,Otters,Ocelots,Otter Steps,Otter Max,Diff Max,Nested,Ocelot Steps,Ones,Hops~n", []), close(Stream),!, 
    measure_machines(Monsters, Dir, Suffix, Options, yes, FileName).

measure_machines(Monsters, Dir, Suffix, Options, Summary, SummaryFile) :- monsters(Monsters, List), member(N, List), measure_machine(N, Monsters, Dir, Suffix, Options, Summary, SummaryFile), fail.
measure_machines(_Monsters, _Dir, _Suffix, _Options, _Summary, _).

monsters(test,  [1,2,3,4]). 
monsters(quick, [1,2,3,4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,74,75,77,78,81,82,83,85,92,93,94,95,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115]).
monsters(slow,  [70,71,72,76,79,80,84,86,87,88,89,90,91,96,97,98]).
monsters(extra,  [91,96,97,98]).
monsters(nutters, [73,5,36]).
monsters(extras, [101,102,103,104,105,106,107,108,109,110,111,112,113,114,115]).
monsters(L, L) :- \+ member(L, [test,quick,slow,extra,nutters,extras]). 

% For "pure emulation" we don't use escape, blank, loop, search, or induction. So we delete these from emulate.pl
options(measure,   [noblank,newacc,otter,ocelot,otrans,compress,trie(5),final,machine]) :- !.
options(eval,      [noblank,newacc,otter,ocelot,induction,otrans,compress,trie(5),final,machine]) :- !.
options(ostart(N), [noblank,newacc,otter,ocelot,otrans,ocelotstart(N),compress,trie(5),final,machine]) :- !. 
options(otter,     [noblank,newacc,otter,ocelot,otrans,otrace,compress,trie(5),final,machine]) :- !.  
options(trace,     [noblank,newacc,otter,ocelot,otrans,otrace,compress,trie(5),trace,final,machine]) :- !. 
options(ctrace,    [noblank,newacc,otter,ocelot,otrans,otrace,compress,trie(5),ctrace,final,machine]) :- !.  
options(pctrace,   [noblank,newacc,otter,ocelot,otrans,otrace,compress,ctrace,final,machine]) :- !. 
options(plain,     [noblank,newacc,otter,ocelot,otrans,otrace,compress,final,machine]) :- !. 
options(Other, Other). 

measure_machine(Num, Monsters, Dir, Suffix, Options, Summary, SummaryFile) :-
    format("Measuring ~d ", [Num]),  ttyflush, 
    atom_number(List, Num), string_concat(List, Suffix, Name), string_concat(Dir, Name, FileName), atom_codes(File, FileName),
    options(Options, RealOptions), format("with options ~k ...", [RealOptions]), 
    monster(Num, M), bound(Num, Bound),
    get_time(T1),
    tell(File), 
    emulate_bb(M, Bound, flex, RealOptions, Ones, Hops, _Status, Outputs),
    get_time(T2), Time is T2-T1, format("took ~2f seconds~n", [Time]), ttyflush, 
    told,
    format(" done~n",[]), ttyflush,
    ( Summary \= yes -> true ; ( open(SummaryFile, append, Stream),
				 member(otters(Otters), Outputs), member(ocelots(Ocelots), Outputs), member(ottersteps(OtterSteps), Outputs), member(ocelotsteps(OcelotSteps), Outputs), member(otterproofs(OtterProofs), Outputs),  member(ottersnested(OttersNested), Outputs),member(otterdiffs(OtterDiffs), Outputs),
				 max_otter_proofs(OtterProofs, 0, OtterMax),
 				 max_otter_diffs(OtterDiffs, 0, OtterDiffMax), 
				 format(Stream, "~d,~2f,~d,~d,", [Num,Time,Otters,Ocelots]),
				 print_bignum(Stream, OtterSteps), format(Stream, ",", []), 
 				 format(Stream, "~d,", [OtterMax]),
 				 print_bignum(Stream, OtterDiffMax), format(Stream, ",", []), 
  				 format(Stream, "~d,", [OttersNested]),
				 print_bignum(Stream, OcelotSteps), format(Stream, ",", []), print_bignum(Stream, Ones), format(Stream, ",", []), print_bignum(Stream, Hops), format(Stream, "~n", []), 
				 close(Stream)
			       ) ), 
    true. 

full :- measure(quick, "Otters/", "-o2.txt", otter), measure(slow, "Otters/", "-o2.txt", otter), measure(nutters, "Otters/", "-o2.txt", otter), true. 

% bound(N,  tens(20)) :- between(1,36,N).
bound(N,  tens(20)) :- between(1,35,N).
bound(36, tens(15)). 
bound(N,  tens(25)) :- between(37,46,N).
bound(47, tens(65)).
bound(N,  tens(220)) :- between(48,49,N).
bound(50, tens(710)).
bound(N,  tens(22)) :- between(51,57,N).
bound(N,  tens(28)) :- between(58,59,N).
bound(N,  tens(45)) :- between(60,61,N).
bound(N,  tens(125)) :- between(62,66,N).
bound(67, tens(930)).
bound(68, tens(1740)).
bound(69, tens(1770)).
bound(70, tens(2890)).
bound(71, tens(21135)).
bound(72, tens(36550)).
bound(73, tens(10)).
bound(74, tens(55)).
bound(75, tens(285)).
bound(76, tens(880)).
bound(77, tens(1260)).
bound(78, tens(2610)).
bound(79, tens(4720)).
bound(80, tens(4750)).
bound(81, tens(13050)).
bound(82, tens(10)).
bound(83, tens(1430)).
bound(84, tens(1620)).
bound(85, tens(2000)).
bound(86, tens(7730)).
bound(87, tens(9130)).
bound(88, tens(9870)).
bound(89, tens(12075)).
bound(N,  tens(14080)) :- between(90,91,N).
bound(N,  tens(10)) :- between(92,94,N).
bound(95, tens(60)).
bound(96, tens(1650)).
bound(N,  tens(19870)) :- between(97,98,N).
bound(99, tens(10)).
bound(100, tens(95)).
bound(N, tens(20)) :- between(101,115,N).

    










