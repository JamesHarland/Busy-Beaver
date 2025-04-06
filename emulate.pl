emulate_bb(M, Bound, Mode, Inputs, Ones, Hops, Status, Outputs) :-
    emulate_general(M, blank, Bound, Mode, eval, [loop,blank], Inputs, Hops, Status, Outputs), % add escape as an option? 
    member(config(Left, State, Item, Dir, Right), Outputs),
    count_ones(config(Left, State, Item, Dir, Right), Ones),

    ( member(final, Inputs) ->   ( final_output(config(Left,State,Item,Dir,Right), Ones, Hops, Outputs)  ) ; true ), 
    ( member(machine, Inputs) -> ( (member(machine(_,Trans,_Otters), Outputs), display_trans(Trans), nl) ) ; true ),
    ( member(otrans,  Inputs) -> ( (member(machine(_,_Trans,Otters), Outputs), display_otters(Otters, 1), nl) ) ; true ). 

final_output(config(Left,State,Item,Dir,Right), Ones, Hops, Outputs) :- 
    format("% Final configuration is ", []), 
    display_config(config(Left, State, Item, Dir, Right), []), !, % nl,
    format(" Ones: ", []), print_bignum(Ones), !, ttyflush,
    RealHops is Hops, 
    format(" Hops: ", []), print_bignum(RealHops), !, ttyflush, 
    member(steps(Steps), Outputs),           format(" Steps: ", []),  print_bignum(Steps), !, nl, ttyflush, 
    member(otters(Otters), Outputs),         format("% Otters: ", []), print_bignum(Otters), !, ttyflush,
    member(machine(_,_,OtterList), Outputs), length(OtterList, UniqueOtters), format(" Unique Otters: ", []), print_bignum(UniqueOtters), !, ttyflush,
    member(ottersnested(OttersNested), Outputs),         format(" Otters Nested: ", []), print_bignum(OttersNested), !, ttyflush, 
    member(ottersteps(OtterSteps), Outputs), format(" Otter Steps: ", []), print_bignum(OtterSteps), !, ttyflush, 
    member(otterjumps(OtterJumps), Outputs), format(" Otter Jumps: ", []), print_bignum(OtterJumps), !, ttyflush,
    member(otterproofs(OtterProofs), Outputs), format(" Otter Proofs: ", []), write(OtterProofs), !, ttyflush,
    member(otterdiffs(OtterDiffs), Outputs), format(" Otter Diffs: ", []), write(OtterDiffs), !, ttyflush, 
    member(ocelots(Ocelots), Outputs),         format(" Ocelots: ", []), print_bignum(Ocelots), !, ttyflush, 
    member(ocelotsteps(OcelotSteps), Outputs), format(" Ocelot Steps: ", []), print_bignum(OcelotSteps), 
    member(ocelotproofs(OcelotProofs), Outputs), format(" Ocelot Proofs: ", []), print_bignum(OcelotProofs), 
    extract_shape(config(Left, State, Item, Dir, Right), Shape), format(" Shape: ", []), display_config(Shape, []), 
    shape_size(config(Left, State, Item, Dir, Right), Size),     format(" Shape Size: ~d ", [Size]),  
    format("~n", []),
    ( Otters \== UniqueOtters -> format("% Otter counting anomaly~n", []); true ), 
    !, ttyflush.

extract_shape(config(Left, State, Item, Dir, Right), config(SLeft, State, SItem, Dir, SRight)) :-
    extract_shape1(Left, SLeft), extract_shape1([Item], [SItem]), extract_shape1(Right, SRight).

extract_shape1([], []).
extract_shape1([Item|Rest], [Item|Result]) :- is_input(Item), extract_shape1(Rest, Result).
extract_shape1([grp(I,N)|Rest], [grp(I,N)|Result]) :- ground(N), N1 is N, N1 =< 3, extract_shape1(Rest, Result). 
extract_shape1([grp(I,N)|Rest], [grp(I,'*')|Result]) :- ground(N), N1 is N, N1 > 3, extract_shape1(Rest, Result). 
extract_shape1([grp(I,N)|Rest], [grp(I,'_')|Result]) :- \+ ground(N), extract_shape1(Rest, Result).

shape_size(config(Left, _, Item, _, Right), Size) :- append(Left, [Item], Temp), append(Temp, Right, Tape), shape_size1(Tape, 0, Size).

shape_size1([], Size, Size).
shape_size1([Item|Rest], SizeSoFar, Size) :- is_input(Item), NewSizeSoFar is SizeSoFar + 1, shape_size1(Rest, NewSizeSoFar, Size).
shape_size1([Item|Rest], SizeSoFar, Size) :- Item = grp(I,N), ground(N), N1 is N, N1 =< 3, length(I, L), NewSizeSoFar is SizeSoFar + N1*L, shape_size1(Rest, NewSizeSoFar, Size).
shape_size1([Item|Rest], SizeSoFar, Size) :- Item = grp(I,N), ground(N), N1 is N, N1 > 3, length(I, L), NewSizeSoFar is SizeSoFar + 4*L,  % So all sizes 4 or bigger are the same ... 
					     shape_size1(Rest, NewSizeSoFar, Size).
shape_size1([Item|Rest], SizeSoFar, Size) :- Item = grp(I,N), nonground(N,_), length(I, L), NewSizeSoFar is SizeSoFar + 4*L,  % Treat variables as 4 or bigger.
					     shape_size1(Rest, NewSizeSoFar, Size).

initial_config(naive, eval, blank, config([], a, I, l, []), 0) :- blank_symbol(I), !. 
initial_config(naive, verify, blank, config([], a, I, l, []), 0) :- blank_symbol(I), !. % Only use variable hops for ossify, not verify. 
initial_config(naive, ossify, blank, config([], a, I, l, []), hops(0, [])) :- blank_symbol(I), !. 
initial_config(naive, eval, config(Left, State, Item, Dir, Right), config(Left, State, Item, Dir, Right), 0). 
initial_config(naive, verify, config(Left, State, Item, Dir, Right), config(Left, State, Item, Dir, Right), 0).
initial_config(naive, ossify, config(Left, State, Item, Dir, Right), config(Left, State, Item, Dir, Right), hops(0, VarList)) :- initial_vars(config(Left, State, Item, Dir, Right), VarList). 
initial_config(flex, eval, blank, config([], a, grp(List,1), l, []), 0) :- blanks(1, List), !. % Act like macro(1) initially
initial_config(flex, verify, blank, config([], a, grp(List,1), l, []), 0) :- blanks(1, List), !. % Act like macro(1) initially
initial_config(flex, ossify, blank, config([], a, grp(List,1), l, []), hops(0,[])) :- blanks(1, List), !. % Act like macro(1) initially
initial_config(flex, eval, config(Left, State, Item, Dir, Right), config(Left, State, Item, Dir, Right), 0). 
initial_config(flex, verify, config(Left, State, Item, Dir, Right), config(Left, State, Item, Dir, Right), 0).
initial_config(flex, ossify, config(Left, State, Item, Dir, Right), config(Left, State, Item, Dir, Right), hops(0, VarList)) :- initial_vars(config(Left, State, Item, Dir, Right), VarList). 

initial_vars(config(Left, _State, Item, _Dir, Right), VarList) :- append(Left, [Item|Right], Tape), initial_vars1(Tape, VarList). 
initial_vars1([], []).
initial_vars1([grp(_I, Num)|Rest], VarList) :- integer(Num), initial_vars1(Rest, VarList). 
initial_vars1([grp(_I, Num)|Rest], [h(Name,0)|VarList]) :- variable(Num), get_name(Num, Name), initial_vars1(Rest, VarList). 

% Status is one of halts, going, going(S), abort, abort(S), loops(cycle), blank, loops(road_runner), target(_), will soon include others.
emulate_general(M, Start, B, Mode, Type, Termination, Inputs, Hops, Status, NewOutputs) :-
     initial_config(Mode, Type, Start, Config, InitHops), 
     machine(Mode, M, Machine), number_of_states(Machine, States), number_of_symbols(Machine, Symbols), % number_of_dirs(Machine, Directions), 
     Directions = 2,
     append(Termination, Inputs, Inputs0), 
     process_inputs(States, Symbols, Directions, Inputs0, Config, Inputs1), 
     process_bound(B, Bound), 
     output_status(M, Config, Mode, 0, Inputs1, Inputs2, init), 
     append([used(init)], Inputs2, NewInputs), !, 
     execute(Machine, Config, Bound, Mode, InitHops, Termination, NewInputs, Result), 
     Result = result(Machine1, Config1, Bound, Mode, Hops, _, Outputs, Status), 
     append(Outputs, [Machine1], Outputs1),  
     append([Config1], Outputs1, Outputs2),
     NewOutputs = Outputs2, 
     !. 

process_inputs(States, _Symbols, Directions, Inputs, Config, FinalInputs) :-
    Directions == 2, 
    (member(loop, Inputs); member(history, Inputs); member(otter, Inputs); member(ocelot, Inputs)), 
    set_type(Inputs, Type), 
    initial_history(Type, States, Directions, History), 	
    append([now(Config), history(History)], Inputs, Inputs1), % history initialisation
    append([pos(0), steps(0), otters(0), ottersnested(0), ottersteps(0), otterjumps(0), otterproofs([]), otterdiffs([]), ocelots(0), ocelotsteps(0), ocelotproofs(0)], Inputs1, Inputs2),
    ( \+ member(variable_names(_Names), Inputs) -> append([variable_names([])], Inputs2, Inputs3) ; Inputs3 = Inputs2 ),
    ( \+ member(ocelotstart(_OStart), Inputs) -> append([ocelotstart(0)], Inputs3, Inputs4) ; Inputs4 = Inputs3 ),
    ( \+ member(expansionstart(_EStart), Inputs) -> append([expansionstart(0)], Inputs4, Inputs5) ; Inputs5 = Inputs4 ),
    ( \+ member(inductionstart(_IStart), Inputs) -> append([inductionstart(0)], Inputs5, Inputs6) ; Inputs6 = Inputs5 ),
    ( member(time, Inputs) -> (get_time(T1), append([starttime(T1)], Inputs6, Inputs7)) ; Inputs7 = Inputs6 ),
    ( member(lemma, Inputs) -> append([lemmas([])], Inputs7, Inputs8) ; Inputs8 = Inputs7 ),
    FinalInputs = Inputs8. 

process_inputs(_States, _Symbols, Directions, Inputs, _Config, FinalInputs) :-
    Directions == 2, 
    \+ member(loop, Inputs), \+ member(history, Inputs), \+ member(otter, Inputs), \+ member(ocelot, Inputs), 
    append([pos(0), steps(0), otters(0), ottersnested(0), ottersteps(0), otterjumps(0), otterproofs([]), otterdiffs([]), ocelots(0), ocelotsteps(0), ocelotproofs(0)], Inputs, Inputs1),
    ( \+ member(variable_names(_Names), Inputs) -> append([variable_names([])], Inputs1, Inputs2) ; Inputs2 = Inputs1 ),
    ( \+ member(ocelotstart(_OStart), Inputs) -> append([ocelotstart(0)], Inputs2, Inputs3) ; Inputs3 = Inputs2 ),
    ( \+ member(expansionstart(_EStart), Inputs) -> append([expansionstart(0)], Inputs3, Inputs4) ; Inputs4 = Inputs3 ),
    ( \+ member(inductionstart(_IStart), Inputs) -> append([inductionstart(0)], Inputs4, Inputs5) ; Inputs5 = Inputs4 ),
    ( member(time, Inputs) -> (get_time(T1), append([starttime(T1)], Inputs5, Inputs6)) ; Inputs6 = Inputs5 ),
    ( member(lemma, Inputs) -> append([lemmas([])], Inputs6, Inputs7) ; Inputs7 = Inputs6 ),
    FinalInputs = Inputs7.

process_bound(N, N) :- integer(N). 
process_bound(mill(N), Num) :- integer(N), Num is N*1000000. % Num is N million.
process_bound(bill(N), Num) :- integer(N), Num is N*1000000000. % Num is N billion.
process_bound(trill(N), Num) :- integer(N), Num is N*1000000000000. % Num is N trillion.
process_bound(tens(N), Num) :- integer(N), Num is 10 ** N. % Num is 10**N.

execute(M, Config, Bound, Mode, Hops, Termination, Inputs, Result) :-
    halt_execution(0,0,0, M, Config, Bound, Mode, Hops, Termination, Inputs, Result), !. 
execute(M, Config, Bound, Mode, Hops, Termination, Inputs, Result) :-
    next_config(0, 0, 0, M, Config, Mode, Hops, Inputs, NewM, NewConfig, NewHops, NewInputs, Used), !,
    output_status(M, NewConfig, Mode, NewHops, NewInputs, NewerInputs, Used), !, ttyflush, % Don't die wondering ... :-)
    execute(NewM, NewConfig, Bound, Mode, NewHops, Termination, NewerInputs, Result). 

halt_execution(_,_,_, M, Config, Bound, Mode, Hops, Termination, Inputs, Result) :-
    Config = config(_Left, State, _Item, _Dir, _Right),
    halt_state(State), !, 
    Result = result(M, Config, Bound, Mode, Hops, Termination, Inputs, halts).
    
halt_execution(_,_,_, M, Config, Bound, Mode, Hops, Termination, Inputs, Result) :-
    Config = config(_Left, State, _Item, _Dir, _Right),
    looping_state(State),  !, 
    Result = result(M, Config, Bound, Mode, Hops, Termination, Inputs, loops(cycle)).

halt_execution(_,_,_, M, Config, Bound, Mode, Hops, Termination, Inputs, Result) :-
    no_transition(M, Config, Mode), !,  
    Result = result(M, Config, Bound, Mode, Hops, Termination, Inputs, abort(Hops)).

halt_execution(_,_,_, M, Config, Bound, Mode, Hops, Termination, Inputs, Result) :-
    ground(Hops), NHops is Hops, NHops >= Bound, !, 
    Result = result(M, Config, Bound, Mode, NHops, Termination, Inputs, going(NHops)).

halt_execution(_,_,_, M, Config, Bound, Mode, Hops, Termination, Inputs, Result) :-
    member(maxsteps(Max), Inputs), member(steps(Steps), Inputs), Steps >= Max, !, 
    Result = result(M, Config, Bound, Mode, Hops, Termination, Inputs, going(Hops)).

halt_execution(_,_,_, M, Config, Bound, Mode, Hops, Termination, Inputs, Result) :-
    member(targets(Tlist), Termination),
    member(Target, Tlist), 
    member(variable_names(Names), Inputs),
    reached(Names, Config, Mode, Target), !, 
    Result = result(M, Config, Bound, Mode, Hops, Termination, Inputs, target(Target)). 

no_transition(M, Config, Mode) :- Mode = naive, Config = config(_Left, State, Item, _Dir, _Right), \+ simple_transition(M, State, Item, _NewState, _OutItem, _OutDir, _Steps). 
no_transition(M, Config, Mode) :- member(Mode, [macro(_K),flex,search]), Config = config(_Left, State, Item, Dir, _Right), first_item(Item, Dir, I), 
				  \+ simple_transition(M, State, I, _NewState, _OutItem, _OutDir, _Steps).

zero_dextrous(States, M) :- \+ atomic(M), states_list(States, SList), zero_dextrous1(SList, M).
% No named machine is zero_dextrous.

zero_dextrous1([], _).
zero_dextrous1([State|Rest], M) :- member(t(State,0,_,r,_), M), !, zero_dextrous1(Rest, M). 

first_item(I, _, I) :- is_input(I). 
first_item([I|_], l, I) :- is_input(I).
first_item(Item, r, I) :- reverse(Item, RevItem), RevItem = [I|_], is_input(I). 
first_item(grp(_Item,N), _, 0) :- N == 0. 
first_item(grp([],_Num), _, 0).
first_item(grp(Item,Num), l, I) :- ground(Num), Num1 is Num, Num1 > 0, Item \= [], Item = [I|_].
first_item(grp(Item,Num), r, I) :- ground(Num), Num1 is Num, Num1 > 0, Item \= [], reverse(Item, RevItem), RevItem = [I|_].
first_item(grp(Item,Num), l, I) :- \+ ground(Num), Item \= [], Item = [I|_].
first_item(grp(Item,Num), r, I) :- \+ ground(Num), Item \= [], reverse(Item, RevItem), RevItem = [I|_].

halt_transition(States, Symbols, Dirs, M, _Config, NewMachine) :-
    M = machine(List1, List2, List3), 
    length(List1,L), 
    L is (States * Symbols) - 1, !, 
    states_list(States, StatesList),
    symbols_list1(Symbols, SymbolsList),
    once( missing(StatesList, SymbolsList, List1, NewState, NewSymbol) ), 
    halt_trans(Dirs, abs, t(NewState, NewSymbol, NewOut, NewDir, NewNewState)),
    append(List1, [t(NewState, NewSymbol, NewOut, NewDir, NewNewState)], NewList), % Makes it hard to have zero_dextrous test here - don't want further execution ... 
    NewMachine = machine(NewList, List2, List3), !.

missing(StatesList, SymbolsList, Transitions, State, Symbol) :-
    member(State, StatesList), member(Symbol, SymbolsList), \+ member(t(State, Symbol, _, _, _), Transitions).

looping(Config, Inputs) :-
    member(history(History), Inputs),
    set_type(Inputs, Type), 
    history_index(Type, Config, Index), 
    Config = config(_, State, _, Dir, _), 
    lookup_history(Type, State, Dir, Index, s(_Hops, Config, _, _), History). %% history lookup 

% Only works if both Config and Target are either both ground or both variable. Could make this more general but doesn't seem to be much point. 
reached(_Names, Config, Mode, Target) :- \+ member(Mode, [naive]), \+ (ground(Config), ground(Target)), equiv_config(Config, Target).
reached(_Names, Config, Mode, Target) :- \+ member(Mode, [naive]), ground(Config), ground(Target), Config == Target. 
reached(_Names, Config, Mode, Target) :- member(Mode, [naive]), Config == Target. 

flatten(config(Left, State, Item, Dir, Right), config(NewLeft, State, NewItem, NewDir, NewRight)) :-
     flatten_tape(left, Left, NLeft), flatten_tape(right, Right, NRight), flatten_tape(right, [Item], NItem), 
     balance_config(config(NLeft, State, NItem, Dir, NRight), config(NewLeft, State, NewItem, NewDir, NewRight)).

balance_config(config(Left, State, Item, Dir, Right), config(NewLeft, State, NewItem, Dir, NewRight)) :-
    Dir = l, !, append([I], Rest, Item), length([I], 1), NewLeft = Left, NewItem = I, append(Rest, Right, NewRight).

balance_config(config(Left, State, Item, Dir, Right), config(NewLeft, State, NewItem, Dir, NewRight)) :-
    Dir = r, !, append(Rest, [I], Item), length([I], 1), append(Rest, Left, NewLeft), NewItem = I, NewRight = Right.

next_config(_States, _Symbols, _Dirs, M, Config, Mode, Hops, Inputs, NewM, NewConfig, NewHops, NewInputs, simple) :-
     Mode = naive,
     Config = config(Left, State, Item, _Dir, Right), 
     simple_transition(M, State, Item, NewState, OutItem, OutDir, Steps),!, 
     updatetape(Mode, Left, OutItem, Right, OutDir, NewLeft, NewItem, NewRight, _Moves),
     NewHops is Hops + Steps,
     NewConfig = config(NewLeft, NewState, NewItem, l, NewRight),
     NewM = M,
     updateinputs(Mode, Hops, OutDir, 1, naive, Inputs, NewConfig, NewInputs).

next_config(_States, _Symbols, _Dirs, M, Config, Mode, Hops, Inputs, NewM, NewConfig, NewHops, NewInputs, modulus) :-
     member(Mode, [flex]), 
     ( member(newacc, Inputs); (\+ member(newacc, Inputs),member(ocelot,Inputs)); (\+ member(newacc, Inputs),\+member(ocelot,Inputs),member(otter,Inputs)) ),
     
     modulus_exponents(Config, Exponents), Exponents \= [], !, % So we have some moduli ... 
     
     member(variable_names(Names), Inputs), 
     member(mods(Mods), Inputs), member(initials(Progs, Regs), Inputs),
     member(diffs(LDiffs, ItemDiff, RDiffs), Inputs), 

     find_mod(Mods, Progs, Regs, Exponents, Names, NewMods), 
     ( compatible_diffs(NewMods, diffs(LDiffs, ItemDiff, RDiffs)) -> true; (format("% Moduli ~k not compatible with diffs ~k ~k ~k , so giving up otter proof attempt~n", [NewMods,LDiffs,ItemDiff,RDiffs]), fail) ),

     member(pos(Pos), Inputs), 
     rewrite_modulus(NewMods, Config, Hops, Pos, 0, Names, NewConfig, NewHops, NewPos, _), !,
    
     NewM = M,
     delete(Inputs, mods(Mods), Inputs1), append([mods(NewMods)], Inputs1, Inputs2),
     delete(Inputs2, pos(Pos), Inputs3), append([pos(NewPos)], Inputs3, Inputs4), 

     NewInputs = Inputs4.

next_config(_States, _Symbols, _Dirs, M, Config, Mode, Hops, Inputs, NewM, NewConfig, NewHops, NewInputs, ocelot(Num)) :-
     member(ocelot, Inputs), \+ member(ignorenextocelot, Inputs), 
     member(Mode, [flex]), 
     M = machine(_, _, Otters), 
     member(o(Num,Transition), Otters), 
     member(variable_names(Names), Inputs), 
     member(ocelotstart(OcelotBound), Inputs),

     ( (ground(Hops), Hops < OcelotBound) ->  fail ; apply_otter(Num, Transition, Config, Hops, Hops, Names, NewConfig, Leaps, Moves) ), % Only apply otter once a certain number of steps has been reached. 
       ttyflush, % If we can do it, do it. Otherwise try another Transition, until there are no options left, in which case the ocelot fails.

     ( reasonable_config(NewConfig) -> true; (format("% ocelot is not reasonable and so not applied ~n", [] ),fail) ), % fail when the ocelot produces a configuration containing a term like 5/3. Monster 74 crashes without this ... 
      !, 
     
      eager_sum(Hops, Leaps, NewHops),

      ( (member(otrace, Inputs) ) -> 
	 ( format("% Ocelot ~d: Hops: ~k Leaps: ~k NewHops: ~k ", [Num, Hops, Leaps, NewHops]), ttyflush, 
	   display_config(Config, []), format(" ++> ", []), ttyflush, 
  	   display_config(NewConfig, []), nl, ttyflush 
          ) ; true ) , 

      Config = config(_, _, Item, _, _), Item = grp(_I,N), ( (\+ ground(N)) -> append(Inputs, [ignorenextocelot], Inputs1); Inputs1 = Inputs ), 
     
     NewConfig = config(_Left, _State, _Item, OutDir, _Right), 
     update_ocelots(Leaps, Inputs1, NInputs),
     updateinputs(Mode, Hops, OutDir, Moves, ocelot(Num), NInputs, NewConfig, NewInputs), !, 
     NewM = M. % no change to machine for this step

next_config(_States, _Symbols, _Dirs, M, Config, Mode, Hops, Inputs, NewM, NewConfig, NewHops, NewInputs, otter) :-
     member(otter, Inputs), % Keep the otter and ocelot independent, so that when searching for a proof, the ocelot can be used but no search is made for new otters.
     \+ member(ignorenextotter, Inputs), % To allow verification but when the final config is the current step. So this will avoid a loop. 
     member(Mode, [flex]),  
    Config = config(Left, State, Item, Dir, Right),

     member(history(History), Inputs),
     set_type(Inputs, Type), 
     history_index(Type, Config, Index),
     lookup_history(Type, State, Dir, Index, s(Hops1, Config1, _, _), History), Hops1 #< Hops, %% history lookup

     Config1 = config(Left1, State, Item1, Dir, Right1), 
     same_shape(config(Left1, State, Item1, Dir, Right1), config(Left, State, Item, Dir, Right)),
     regression(config(Left1, State, Item1, Dir, Right1), config(Left, State, Item, Dir, Right), _RealConfig1, LDiffs1, ItemDiff1, RDiffs1),  

     lookup_history(Type, State, Dir, Index, s(Hops2,Config2, _, _), History), Hops2 #< Hops1, %% history lookup
     Config2 = config(Left2, State, Item2, Dir, Right2), 
     same_shape(config(Left2, State, Item2, Dir, Right2), config(Left1, State, Item1, Dir, Right1)),

     regression(config(Left2, State, Item2, Dir, Right2), config(Left1, State, Item1, Dir, Right1), _RealConfig2, LDiffs2, ItemDiff2, RDiffs2),

     LDiffs1 == LDiffs2, ItemDiff1 == ItemDiff2, RDiffs1 == RDiffs2, 

     diff_counters(LDiffs1, ItemDiff1, RDiffs1, Progressors, Regressors), length(Progressors, Progs), length(Regressors, Regs), 

     ( (member(otrace, Inputs) ) -> 
	 ( format("% Otter Observed: ~k ~k ~k Diff1 is ~k Diff2 is ~k ", [Hops2, Hops1, Hops, Hops1 - Hops2, Hops - Hops1]), 
	   display_config(config(Left2, State, Item2, Dir, Right2), []), format(" + ", []), 
	   display_config(config(Left1, State, Item1, Dir, Right1), []), format(" + ", []), 
	   display_config(config(Left, State, Item, Dir, Right), []), % Original Config
           format(" Progressors: ~k, Regressors: ~k~n", [Progs, Regs]), 
           true									
          ) ; true ) , 

     otters_in_machine(M, Number), NewNumber is Number + 1,
     verify_otter(M, NewNumber, Mode, Config, LDiffs1, ItemDiff1, RDiffs1, Progressors, Regressors, simple, Shape_Computation, Otter_Transition), % We now assume otter -> verify (why would it be anything else?) % Trying this to avoid overshooting ...
     member(variable_names(Names), Inputs),
     member(ocelotstart(OcelotBound), Inputs), 

     ( (ground(Hops), Hops < OcelotBound) -> (format("% Otter not applied due to early constraint~n", []), Leaps = 0, NewConfig = Config, NewHops = Hops )
                                                                      ; ( apply_otter(0, Otter_Transition, Config2, Hops2, Hops, Names, NewConfig, Leaps, Moves ) -> ( eager_sum(Hops2, Leaps, NewHops), format("% Otter applied to get ~k at ~k~n", [NewConfig, NewHops])) ;
                                                                                                                                                    ( format("% Otter not applied at ~k to avoid loop~n", [Hops]), Leaps = 0, NewConfig = Config, NewHops = Hops) ) ),
     % The current Config could be the final instance of the otter, and so the constraints may not be satisified ... But if it is the final one, there is no point in applying the otter! So don't apply the otter and make a note to ignore it next time around (otherwise we get a loop). Also for variable case we need to apply it to the oldest configuration ... 
     !, % Only now can commit to this case. 
     ttyflush, 
     append([ignorenextotter,ignorenextocelot], Inputs, Inputs1), % Stop finding the same otter at a later place, eg monster 18 117-120-123 otters to 126 and then finds 120-123-126

     ( (member(otrace, Inputs1),member(verify, Inputs1) ) -> (format("% Final otter is ~d:", [NewHops]), display_config(NewConfig, []), nl, ttyflush); true), 
     ( member(ocelot, Inputs1) -> update_machine(M, Hops, Otter_Transition, NewM, OtterStatus); (NewM = M, OtterStatus = new)),      
     ( member(predict, Inputs1) -> check_predictions(Hops, Inputs1, NewConfig, Inputs2); Inputs2 = Inputs1),
     
     nested_otters(Shape_Computation, Nested), 
     length(Shape_Computation, OtterProofLength), 
     ( (ground(Hops1-Hops2), ground(Hops-Hops1), OtterStatus == new)  -> (D1 is Hops1 - Hops2, D2 is Hops - Hops1, update_otters(Leaps, 0, Nested, OtterProofLength, D1, D2, Inputs2, Inputs3)); Inputs3 = Inputs2 ),

     NewConfig = config(_,_,_,OutDir,_), 

     updateinputs(Mode, Hops, OutDir, Moves, otter, Inputs3, NewConfig, NewInputs), !, 

     true. 

next_config(_States, _Symbols, _Dirs, M, Config, Mode, Hops, Inputs, NewM, NewConfig, NewHops, NewInputs, expansion) :-
     member(Mode, [flex]), member(newacc, Inputs), \+ member(ignorenextacc, Inputs), 

     ( member(ignorenextotter, Inputs) -> delete(Inputs, ignorenextotter, Inputs1) ; Inputs1 = Inputs ), % clean up otter marker, now that we are past where it is needed.
     ( member(ignorenextocelot, Inputs1) -> delete(Inputs1, ignorenextocelot, Inputs2) ; Inputs2 = Inputs1 ), % same for ocelot marker, now that we are past where it is needed.
     ( member(ignorenextlemma, Inputs2) -> delete(Inputs2, ignorenextlemma, Inputs3) ; Inputs3 = Inputs2 ), % and yet again for the lemma marker, now that we are past where it is needed.

     member(expansionstart(ExpansionBound), Inputs), 
     ( (ground(Hops), Hops < ExpansionBound) ->  (format("% Expansion ignored due to early constraint~n", []),fail) ; true ), % Only apply expansion once a certain number of steps has been reached. 

     rotate_for_accel(Config, RAConfig), 
     RAConfig = config(_Left, State, Item, Dir, _Right), 
     Item = grp(I, Number),
      number_of_states(M, States), 
     ( variable(Number); (ground(Number), N1 is Number, length(I,L), L*N1 > States ) ), % Only apply if Number is a variable, or ground and at least the number of states (so it is sensible to try an expansion). 

     expanded(I, States, [], ExpI), 
     complex_transition(M, State, ExpI, Dir, NewM, NewState, OutItem, OutDir, Steps), % Note that we must have State = NewState for this to be an acceleration transition ... 
     classify_transition(trans(State, ExpI, Dir, NewState, OutItem, OutDir, Steps), acceleration), !, % If this fails, we may retry if there is another expansion. If not, there is no acceleration transition.
     
     updatetapeflex(RAConfig, Mode, trans(State, ExpI, Dir, NewState, OutItem, OutDir, Steps), NLeft1, NItem, NewDir, NRight1, Leaps, Moves),
     
     check_blank(NLeft1, NLeft),
     check_blank(NRight1, NRight),

     NConfig = config(NLeft, NewState, NItem, NewDir, NRight), eager_sum(Hops, Leaps, NHops),

     ( member(variable_names(Names), Inputs3) -> true; Names = [] ),
     ( member(rotate, Inputs3) -> CMode = rotate; CMode = plain), 
     ( member(compress, Inputs3) -> (compress_config(CMode, NConfig, NewishConfig)) ; NewishConfig = NConfig ),

     !, 

     member(pos(Pos), Inputs3),
     ( member(mods(Mods), Inputs3) -> (rewrite_modulus(Mods, NewishConfig, NHops, Pos, Moves, Names, NewConfig, NewHops, NewPos, NewMoves)); (NewConfig = NewishConfig, NewHops = NHops, NewPos = Pos, NewMoves = Moves) ), 
     ( member(ctrace, Inputs3) -> ( NConfig \== NewishConfig -> (format("% ~d: compressed(accel) ", [NewHops]), write_term(NConfig,[]), format(" **> ", []), write_term(NewishConfig,[]),nl); true
				  ) ; true ), 

     delete(Inputs3, pos(Pos), Inputs4), append([pos(NewPos)], Inputs4, Inputs5), 
     updateinputs(Mode, Hops, OutDir, NewMoves, expansion, Inputs5, NewConfig, NewInputs),!,

     ( member(acctrace, Inputs5) ->
       ( ((length(ExpI, L1), length(I, L2), L2 < L1)) -> ( format("% Acctrans (~k --> ~k): ", [ExpI,OutItem]), format(" Hops: ~k Leaps: ~k NewHops: ~k ", [Hops, Leaps, NewHops]), display_config(Config, []), format(" ++> ", []), display_config(NewConfig, []), nl ) ; true )
       ; true ), 

     true.

next_config(_States, _Symbols, _Dirs, M, Config, Mode, Hops, Inputs, NewM, NewConfig, NewHops, NewInputs, complex(TransType)) :-
     member(Mode, [flex]), % Default case ... 

    ( member(ignorenextotter, Inputs) -> delete(Inputs, ignorenextotter, Inputs1) ; Inputs1 = Inputs ), % clean up otter marker, now that we are past where it is needed.
    ( member(ignorenextocelot, Inputs1) -> delete(Inputs1, ignorenextocelot, Inputs2) ; Inputs2 = Inputs1 ), % same for ocelot marker, now that we are past where it is needed.
    ( member(ignorenextacc, Inputs) -> delete(Inputs, ignorenextacc, Inputs3) ; Inputs3 = Inputs2 ), % and again for new acceleration case ... 

     Config = config(_Left, State, Item, Dir, _Right), 
     Item = grp(I, _Number),  

     complex_transition(M, State, I, Dir, NewM, NewState, OutItem, OutDir, Steps),  !,
     classify_transition(trans(State, I, Dir, NewState, OutItem, OutDir, Steps), TransType), 
     updatetapeflex(Config, Mode, trans(State, I, Dir, NewState, OutItem, OutDir, Steps), NLeft1, NItem, NewDir, NRight1, Leaps, Moves),

     check_blank(NLeft1, NLeft),
     check_blank(NRight1, NRight),

     set_rotation(Inputs3, CMode), 
     NConfig = config(NLeft, NewState, NItem, NewDir, NRight), 
     compression(CMode, NConfig, Inputs3, NewConfig), 

     eager_sum(Hops, Leaps, NewHops), 
     ( member(ctrace, Inputs3) -> ( NConfig \== NewConfig -> (format("% ~d: compressed(flex) ", [Hops]), write_term(NConfig,[]), format(" **> ", []), write_term(NewConfig,[]),nl); true ) ; true ), 
     updateinputs(Mode, Hops, OutDir, Moves, complex(TransType), Inputs3, NewConfig, NewInputs). 

compatible_diffs([], _).
compatible_diffs([Mod|RestMods], Diffs) :-
    Diffs = diffs(LDiffs, ItemDiff, RDiffs),
    Mod = mod(Var, N, _Value),
    reverse(LDiffs, RevLDiffs), append(RevLDiffs, [ItemDiff], Temp), append(Temp, RDiffs, AllDiffs),
    classify_diffs(AllDiffs, PosDiffs, NegDiffs),
    find_diff(Var, PosDiffs, NegDiffs, VarDiff),
    compatible_values(VarDiff, N),
    compatible_diffs(RestMods, Diffs). 

classify_diffs([], [], []).
classify_diffs([m(_L,K)|RestDiffs], PosDiffs, NegDiffs) :- K == 0, classify_diffs(RestDiffs, PosDiffs, NegDiffs). 
classify_diffs([m(L,K)|RestDiffs], [m(L,K)|PosDiffs], NegDiffs) :- K > 0, classify_diffs(RestDiffs, PosDiffs, NegDiffs).
classify_diffs([m(L,K)|RestDiffs], PosDiffs, [m(L,K)|NegDiffs]) :- K < 0, classify_diffs(RestDiffs, PosDiffs, NegDiffs). 

find_diff("X1", PosDiffs, _, VarDiff) :- PosDiffs = [VarDiff|_], !. 
find_diff("X2", PosDiffs, _, VarDiff) :- append(P1, P2, PosDiffs), length(P1, 1), P2 = [VarDiff|_], !.
find_diff("X3", PosDiffs, _, VarDiff) :- append(P1, P2, PosDiffs), length(P1, 2), P2 = [VarDiff|_], !. 
find_diff("Y1", _, NegDiffs, VarDiff) :- NegDiffs = [VarDiff|_], !. 
find_diff("Y2", _, NegDiffs, VarDiff) :- append(N1, N2, NegDiffs), length(N1, 1), N2 = [VarDiff|_], !.
find_diff("Y3", _, NegDiffs, VarDiff) :- append(N1, N2, NegDiffs), length(N1, 2), N2 = [VarDiff|_], !.

compatible_values(m(_L,K), N) :- 0 is K mod N. 

output_shape_computation(_, _, _, []).
output_shape_computation(M, Mode, Inputs, [s(Hops, Config, Pos, Step)|Rest]) :-
           output_config(M, Config, Mode, Hops, [pos(Pos)|Inputs], Step), nl, ttyflush, 
           output_shape_computation(M, Mode, Inputs, Rest). 

eager_sum(Old, Increment, New) :- ground(Old), ground(Increment), !, New is Old + Increment. 
eager_sum(Old, Increment, New) :- ground(Old), \+ ground(Increment), !, NOld is Old, simplify_exp2(Increment + NOld, New). 
eager_sum(Old, Increment, New) :- \+ ground(Old), var(Old), ground(Increment), !, NIncrement is Increment, New = Old + NIncrement.
eager_sum(Old, Increment, New) :- \+ ground(Old), \+ var(Old), ground(Increment), !, simplify_exp2(Old + Increment, New). 
eager_sum(Old, Increment, New) :- \+ ground(Old), \+ var(Old), Old = X + Y, ground(Y), ground(Increment), !, NIncrement is Y + Increment, New = X + NIncrement.
eager_sum(Old, Increment, New) :- \+ ground(Old), \+ var(Old), Old = X - Y, ground(Y), ground(Increment), !, NIncrement is Increment - Y, New = X + NIncrement.
eager_sum(Old, Increment, New) :- \+ ground(Old), \+ var(Old), \+ (Old = _ + _), \+ (Old = _ - _), ground(Increment), !, NIncrement is Increment, New = Old + NIncrement.
eager_sum(Old, Increment, New) :- \+ ground(Old), \+ ground(Increment), !, simplify_exp2(Old + Increment, New). 

set_new_count(Num, L, Count) :- integer(Num), X is Num mod L, X > 0, Count is Num // L, Count > 0. % Fail if Count = 0. 
set_new_count(Num, L, Count) :- integer(Num), X is Num mod L, X is 0, Count is (Num // L) - 1, Count > 0. % Fail if Count = 0.
set_new_count(Num, 1, Num) :- variable(Num). % , get_coefficient(Num, C), get_sum(Num, S), true.
set_new_count(Num, L, Count) :- variable(Num), L > 1, Count = Num // L. % , get_coefficient(Num, C), get_sum(Num, S), true.

% set_new_var_count(Num, L, FinalValue, Count) :-  Count = ( (Num - FinalValue) // L).
% set_new_var_count(Num, L, Count) :-  Count = (Num // L)  - (((Num//L)*L)//Num). % Second term is 0 if Num mod L > 0, 1 if Num mod L = 0. This means we can 'delay' the set_new_count calcuation until the value of Num is known. Crude, but ... 

minimum_regressor(Regs, MinNum, MinL, MinCount) :-
    all_integer(Regs), Regs = [reg(Num,L)|RestRegs], set_new_count(Num, L, Count),
    minimum_regressor1(RestRegs, Num, L, Count, MinNum, MinL, MinCount). 
minimum_regressor(Regs, Num, L, Count) :-
    \+ all_integer(Regs), Regs = [reg(Num,L)], %% Only allow a single regressor in the variable case, for now ... 
    set_new_count(Num, L, Count).

minimum_regressor1([], Num, L, Count, Num, L, Count).
minimum_regressor1([reg(Num,L)|Rest], _NumSoFar, _LSoFar, CountSoFar, MinNum, MinL, MinCount) :-
    set_new_count(Num, L, Count), Count < CountSoFar, 
    minimum_regressor1(Rest, Num, L, Count, MinNum, MinL, MinCount).
minimum_regressor1([reg(Num,L)|Rest], NumSoFar, LSoFar, CountSoFar, MinNum, MinL, MinCount) :-
    set_new_count(Num, L, Count), Count >= CountSoFar, 
    minimum_regressor1(Rest, NumSoFar, LSoFar, CountSoFar, MinNum, MinL, MinCount).

all_integer([]).
all_integer([reg(Num,_)|Rest]) :- integer(Num), all_integer(Rest). 

same_otter_config(config(Left1, State1, Item1, Dir1, Right1), config(Left2, State2, Item2, Dir2, Right2)) :-
    State1 == State2, Dir1 == Dir2,
    same_otter_tape(Left1, Left2),
    same_otter_tape([Item1], [Item2]), 
    same_otter_tape(Right1, Right2).

same_exact_shape(config(Left1, State1, Item1, Dir1, Right1), config(Left2, State2, Item2, Dir2, Right2)) :-
     State1 == State2, Dir1 == Dir2, 
     same_shape1(Left1, Left2),
     same_shape1([Item1], [Item2]),
     same_shape1(Right1, Right2).

same_shape(config(Left1, State1, Item1, Dir1, Right1), config(Left2, State2, Item2, Dir2, Right2)) :-
     State1 == State2, Dir1 == Dir2, 
     same_shape2(Left1, Left2),
     same_shape2([Item1], [Item2]),
     same_shape2(Right1, Right2).

same_shape1([], []).
same_shape1([grp(I1,_N1)|Rest1], [grp(I2,_N2)|Rest2]) :- I1 == I2, same_shape1(Rest1, Rest2).

same_shape2([], _).
same_shape2([grp(I1,_N1)|Rest1], [grp(I2,_N2)|Rest2]) :- I1 == I2, same_shape2(Rest1, Rest2), !. 
same_shape2([grp(I1,N1)|Rest1], [grp(I2,_N2)|Rest2]) :- I1 \== I2, same_shape2([grp(I1,N1)|Rest1], Rest2).

same_otter_tape([], []).
same_otter_tape([Item1|Rest1], [Item2|Rest2]) :- ground(Item1), ground(Item2), Item1 == Item2, !, same_otter_tape(Rest1, Rest2).
same_otter_tape([grp(I1,N1)|Rest1], [grp(I2,N2)|Rest2]) :- I1 == I2, same_variable_expression(N1,N2), same_otter_tape(Rest1, Rest2). % This is basically saying N1 and N2 are equivalent when variables are involved, eg Y1//2 and Y2//2. 

% Hacky but will do for now. 
same_variable_expression(E1,E2) :- copy_term_nat(E1, NewE1), copy_term_nat(E2, NewE2), \+ \+ NewE1 = NewE2. 

equiv_config(Config1, Config2) :-
    Config1 = config(_Left1, State, _Item1, _Dir1, _Right1),
    Config2 = config(_Left2, State, _Item2, _Dir2, _Right2), % Fail quickly if the states differ.  
    flattenish(Config1, C1), flattenish(Config2, C2), exact_config(C1, C2), !. % Hacky version for monster 72 issue. 
equiv_config(Config1, Config2) :-
    Config1 = config(_Left1, State, _Item1, _Dir1, _Right1),
    Config2 = config(_Left2, State, _Item2, _Dir2, _Right2), % Fail quickly if the states differ.
    variant_config(Config1, VarConfig1), flattenish(VarConfig1, Flat1), 
    variant_config(Config2, VarConfig2), flattenish(VarConfig2, Flat2),
    exact_config(Flat1, Flat2), !. 

flattenish(Config, Flat) :-
    Config = config(Left, State, Item, l, Right),
    new_leftify(Left, NLeft), 
    flattenish_tape(NLeft, NewLeft1),
    new_leftify(NewLeft1, NewLeft), 
    flattenish_tape([Item|Right], Right2),
    front_item(Right2, NewItem, NewRight),
    Flat = config(NewLeft, State, NewItem, l, NewRight), !. 

flattenish(Config, Flat) :-
    Config = config(Left, State, Item, r, Right),
    new_leftify([Item|Left], NLeft), 
    flattenish_tape(NLeft, NLeft2),
    new_leftify(NLeft2, NewLeft2), 
    front_item(NewLeft2, NewItem, NewLeft), 
    flattenish_tape(Right, NewRight),
    Flat = config(NewLeft, State, NewItem, l, NewRight), !.

flattenish_tape([], []).
flattenish_tape(Tape, Tape) :- length(Tape, 1), !. 
flattenish_tape(Tape, FlatTape) :-
    Tape = [Item1|Rest1], Rest1 = [Item2|Rest2],
    Item1 = grp(I1, N1), Item2 = grp(I2, N2),
    ground(N1), ground(N2), RealN1 is N1, RealN2 is N2, member(RealN1, [1,2]), member(RealN2, [1,2]),
    make_item(I1, N1, I2, N2, I3),
    append([grp(I3,1)], Rest2, Tape1),
    flattenish_tape(Tape1, FlatTape).

flattenish_tape(Tape, FlatTape) :-
    Tape = [Item1|Rest1], Rest1 = [Item2|_Rest2],
    Item1 = grp(_I1, N1), Item2 = grp(_I2, N2),
    \+ (ground(N1), ground(N2), RealN1 is N1, RealN2 is N2, member(RealN1, [1,2]), member(RealN2, [1,2])),
    flattenish_tape(Rest1, Result1),
    append([Item1], Result1, FlatTape). 

make_item(I1, 1, I2, 1, I3) :- append(I1, I2, I3).
make_item(I1, 1, I2, 2, I3) :- append(I1, I2, Temp), append(Temp, I2, I3).
make_item(I1, 2, I2, 1, I3) :- append(I1, I1, Temp), append(Temp, I2, I3). 
make_item(I1, 2, I2, 2, I3) :- append(I1, I1, Temp1), append(I2, I2, Temp2), append(Temp1, Temp2, I3). 

front_item([Item|Rest], grp(I,N), Rest) :- Item = grp(I,N), variable(N).
front_item([Item|Rest], grp(I,N), Rest) :- Item = grp(I,N), variable(N).
front_item([Item|Rest], grp(I,N), Rest) :- Item = grp(I,N), ground(N), length(I, 1), 1 is N. 
front_item([Item|Rest], grp(I,1), [grp(I,N1)|Rest]) :- Item = grp(I,N), ground(N), length(I, 1), N > 1, N1 is N-1.
front_item([Item|Rest], grp(I1,1), [grp(I2,1)|Rest]) :- Item = grp(I,N), ground(N), length(I, K), K > 1, 1 is N, append(I1,I2,I), length(I1,1).
front_item([Item|Rest], grp(I1,1), Result) :- Item = grp(I,N), ground(N), length(I, K), K > 1, N > 1, N1 is N-1, append(I1,I2,I), length(I1,1), append([grp(I2,1),grp(I,N1)], Temp), append(Temp, Rest, Result). 

variant_config(Config, VariantConfig) :-
    compress_config(plain, Config, VariantConfig). % Could we use rotate?
variant_config(Config, VariantConfig) :-
    Config = config(Left, State, Item, Dir, Right),
    new_leftify(Left, RealLeft), rotate(RealLeft, VarLeftTemp), new_leftify(VarLeftTemp, VarLeft),
    rotate(Right, VarRight), 
    VariantConfig = config(VarLeft, State, Item, Dir, VarRight).
variant_config(Config, VariantConfig) :- rotate_for_accel(Config, VariantConfig).

rotate_for_accel(Config, Config). % Allow no rotation for simplicity at upper level. 
rotate_for_accel(Config, RAConfig) :-
    Config = config(Left, State, Item, l, Right),
    rotate([Item|Right], NewRight),
    NewRight = [NewItem|RestNewRight],
    RAConfig = config(Left, State, NewItem, l, RestNewRight).

rotate_for_accel(Config, RAConfig) :-
    Config = config(Left, State, Item, r, Right),
    new_leftify([Item|Left], RealLeft), rotate(RealLeft, NLeft), new_leftify(NLeft, NewLeft),
    NewLeft = [NewItem|RestNewLeft],
    RAConfig = config(RestNewLeft, State, NewItem, r, Right).
    
exact_config(config(Left1, State1, Item1, Dir1, Right1), config(Left2, State2, Item2, Dir2, Right2)) :-
     State1 == State2, Dir1 == Dir2,
     exact_tape(Left1, Left2),
     exact_tape([Item1], [Item2]),
     exact_tape(Right1, Right2).

exact_tape([], []).
exact_tape([grp(I1,N1)|Rest1], [grp(I2,N2)|Rest2]) :-
     ground(N1), ground(N2),  I1 == I2, N is N1, N is N2, !, 
     exact_tape(Rest1, Rest2). 
exact_tape([grp(I1,N1)|Rest1], [grp(I2,N2)|Rest2]) :-
     nonground(N1,_), nonground(N2,_), I1 == I2, RealN1 = N1, RealN2 = N2, simplify_exp2(RealN1, R1), simplify_exp2(RealN2, R2), R1 == R2, !, 
     exact_tape(Rest1, Rest2). 

regression(config(Left1, State1, Item1, Dir1, Right1), config(Left2, _State2, Item2, _Dir2, Right2), config(RealLeft1, State1, Item1, Dir1, RealRight1), LDiffs, ItemDiff, RDiffs) :- 
     changes(Left1, Left2, RealLeft1, LDiffs), changes(Right1, Right2, RealRight1, RDiffs), changes([Item1], [Item2], _, [ItemDiff]),
     regressor_list(LDiffs, ItemDiff, RDiffs, Regressors), length(Regressors, L), L >=1. % At least one difference is negative

changes([], Rest, RealRest, Diffs) :- changes1(Rest, RealRest, Diffs). 
changes([grp(I1,N1)|R1], [grp(I2,N2)|R2], [grp(I1,N1)|RealOne], [m(1,L)|Rest]) :- I1 == I2, L #= N2 - N1, integer(L), changes(R1, R2, RealOne, Rest). % Need difference to be an integer ... 
changes([grp(I1,N1)|R1], [grp(I2,N2)|R2], [grp(I2,0)|RealOne], [m(1,N2)|Rest]) :- I1 \== I2, changes([grp(I1,N1)|R1], R2, RealOne, Rest). % Do we need N2 to be ground? 

changes1([], [], []).
changes1([grp(I,N)|Rest], [grp(I,0)|RealRest], [m(1,N)|Diffs]) :- changes1(Rest, RealRest, Diffs). 

regressive([m(_, LL)|_Diffs]) :- LL #< 0.
regressive([m(_, LL)|Diffs]) :- LL #>= 0, regressive(Diffs).

diff_counters(LDiffs, ItemDiff, RDiffs, Progressors, Regressors) :-
    regressor_list(LDiffs, ItemDiff, RDiffs, Regressors),
    progressor_list(LDiffs, ItemDiff, RDiffs, Progressors).

progressive(config(Left1, State, Item1, Dir, Right1), config(Left2, State, Item2, Dir, Right2)) :-
     changes(Left1, Left2, _RealLeft1, LDiffs),    
     changes(Right1, Right2, _RealRight1, RDiffs),  
     changes([Item1], [Item2], _, [ItemDiff]),
     append(LDiffs, [ItemDiff|RDiffs], List), 
     progressive_list(List). 

progressive_list([]).
progressive_list([m(N,K)|Rest]) :- ground(N), ground(K), N >= 0, K >= 0, progressive_list(Rest).
progressive_list([m(N,K)|Rest]) :- ground(N), ground(K), N > 1,  K < 0, progressive_list(Rest). % Weird, but I guess correct ...  

real_progression([], [], [], [], []).
real_progression([grp(I,N0)|R0], [grp(I,N1)|R1], [grp(I,N2)|R2], [grp(I,N3)|R3], [Diff|Rest]) :-
	real_progression1(N0, N1, N2, N3, Diff), 
	real_progression(R0, R1, R2, R3, Rest). 	

real_progression1(N0, N1, N2, N3, m(1,0)) :- 
	ground(N0), ground(N1), ground(N2), ground(N3), N1 is N2, N2 is N3, N1 is N0, !. 
real_progression1(N0, N1, N2, N3, m(1,L)) :- 
	ground(N0), ground(N1), ground(N2), ground(N3), 
	N3 > N2, N2 > N1, N1 > N0, L is N1 - N0, L is N2 - N1, L is N3 - N2, !. % linear progression
real_progression1(N0, N1, N2, N3, m(K,L)) :- 
        ground(N0), ground(N1), ground(N2), ground(N3), 
	N3 > N2, N2 > N1, N1 > N0, N3 - N2 > N2 - N1, N2 - N1 > N1 - N0, 0 is (N3-N2) mod (N2-N1), !, 
	K is (N3-N2) // (N2-N1), K is (N2-N1) // (N1-N0), K is (N3-N1) // (N2-N0), 
	L is N1 - K*N0, L >= 0, !. 

regression_counters([L1|Left], [LDiff|LDiffs], Right, RDiffs, SoFar, Counts) :-
      regressors(SoFar, L1, LDiff, NewSoFar), 
      regression_counters(Left, LDiffs, Right, RDiffs, NewSoFar, Counts). 
regression_counters([], [], [R1|Right], [RDiff|RDiffs], SoFar, Counts) :-
    regressors(SoFar, R1, RDiff, NewSoFar), 
      regression_counters([], [], Right, RDiffs, NewSoFar, Counts). 
regression_counters([], [], [], [], Counts, Counts). 

regressors(List, _Item, Diff, List) :-  Diff = m(_K,L), L >= 0.
regressors(List, Item, Diff, [reg(Num, LL)|List]) :- Item = grp(_I,Num), Diff = m(_K,L), L < 0, LL is 0 - L.

eval_regs([], []).
eval_regs([reg(Num,LL)|Rest], [Count|Result]) :- ground(Num), ground(LL), Count is Num // LL, eval_regs(Rest, Result). 

simple_transition(M, State, Item, NewState, NewItem, Dir, 1) :- \+ halt_state(State), M = machine(TransList, _, _), \+ atomic(TransList), member(t(State, Item, NewItem, Dir, NewState), TransList). 
simple_transition(M, State, Item, NewState, NewItem, Dir, 1) :- \+ halt_state(State), \+ (M = machine(_TransList, _, _)), \+ atomic(M), member(t(State, Item, NewItem, Dir, NewState), M).
simple_transition(riemann, State, Item, NewState, NewItem, Dir, 1)  :- \+ halt_state(State), riemann(State, Item, NewItem, Dir, NewState). 
simple_transition(goldbach, State, Item, NewState, NewItem, Dir, 1) :- \+ halt_state(State), goldbach(State, Item, NewItem, Dir, NewState). 
simple_transition(zermelo, State, Item, NewState, NewItem, Dir, 1)  :-  \+ halt_state(State), zermelo(State, Item, NewItem, Dir, NewState). 

complex_transition(M, State, Item, InDir, NewM, NewState, OutItem, OutDir, Steps) :-
    \+ halt_state(State), M = machine(_, MacroList, _), member(trans(State, Item, InDir, NewState, OutItem, OutDir, Steps), MacroList), !, 
    NewM = M.

complex_transition(M, State, Item, InDir,  NewM, NewState, OutItem, OutDir, Steps) :-
    \+ halt_state(State), M = machine(_, MacroList, _), \+ member(trans(State, Item, InDir, _NewState, _OutItem, _OutDir, _Steps), MacroList), !, 
    length(Item, Len),
    number_of_states(M, Num), 
    B is truncate(Num * Len * 2**Len), % maxbound(B, Bound), 
    find_transition(M, State, Item, InDir, B, NewM, NewState, OutItem, OutDir, Steps), !. 

new_transition(States, Symbols, _Dirs, M, Config, t(State, Item, 1, r, HaltState), NewM) :-
    number_of_states_used(M, States), number_of_symbols_used(M, Symbols), % So M is "full" ... 
    M = machine(Transitions, List2, List3), 
    Config = config(_Left, State, Item, _Dir, _Right), 
    halt_state(HaltState), 
    Trans =  t(State, Item, 1, r, HaltState), % New halt transition ... 
    append(Transitions, [Trans], NewTransitions), 
    \+ zero_dextrous(States, NewTransitions), % Make sure the new machine is not 0-dextrous ... 
    NewM = machine(NewTransitions, List2, List3).

new_transition(States, Symbols, Dirs,  M, Config, t(State, Item, Out, OutDir, NewState), NewM) :-
    % No need to be exclusive here --- we want this case to work even if the one above does ... 
    M = machine(Transitions, List2, List3), % write(Config), nl, 
    Config = config(_Left, State, Item, _Dir, _Right), 
    newstate(States, Symbols, Dirs, Transitions, NewState, Out, OutDir), 
    Trans =  t(State, Item, Out, OutDir, NewState), 
    append(Transitions, [Trans], NewTransitions), 
    \+ zero_dextrous(States, NewTransitions), % Make sure the new machine is not 0-dextrous ... 
    add_halt_trans(States, Symbols, Dirs, NewTransitions, NewTrans), % If the new machine has only one available slot, add the halt transition now. 
    NewM = machine(NewTrans, List2, List3).

newstate(States, Symbols, Dirs, List, NewState, Output, Dir) :-
    dirs_list(Dirs, abs, DirList), % Compatibility hack for now ...
    symbols_list(Symbols, List, SymbolsList), 
    legitimate_list(States, List, StateList), 
    !, 
    member(NewState, StateList), 
    member(Output, SymbolsList), 
    member(Dir, DirList). 

add_halt_trans(States, Symbols, _Dirs, Trans, Trans) :-
    length(Trans, L), Full is States * Symbols - 1, L < Full. 
add_halt_trans(States, Symbols, Dirs, Trans, NewTrans) :-
    length(Trans, L), Full is States * Symbols - 1, L == Full,
    states_list(States, StatesList),
    symbols_list1(Symbols, SymbolsList),
    once( missing(StatesList, SymbolsList, Trans, NewState, NewSymbol) ), 
    halt_trans(Dirs, abs, t(NewState, NewSymbol, NewOut, NewDir, NewNewState)),
    append(Trans, [t(NewState, NewSymbol, NewOut, NewDir, NewNewState)], NewTrans).

find_transition(M, State, Item, l, Bound, NewM, NewState, OutItem, OutDir, Steps) :-
    Item = [I|Rest], append(Rest, [right_end], NewRest), 
    wild_wombat(config([left_end], State, I, l, NewRest), M, Bound, [pos(0)], 0, NewState, OutItem, OutDir, Steps),
    M = machine(TransList, MacroList, List),
    add_to_list(trans(State, Item, l, NewState, OutItem, OutDir, Steps), MacroList, NewMacroList),
    NewM = machine(TransList, NewMacroList, List). 

find_transition(M, State, Item, r, Bound, NewM, NewState, OutItem, OutDir, Steps) :-
    splitlast(Item, L1, [I]), reverse(L1, Left), !, append(Left, [left_end], NewLeft), 
    wild_wombat(config(NewLeft, State, I, r, [right_end]), M, Bound, [pos(0)], 0, NewState, OutItem, OutDir, Steps),
    M = machine(TransList, MacroList, List), 
    add_to_list(trans(State, Item, r, NewState, OutItem, OutDir, Steps), MacroList, NewMacroList),
    NewM = machine(TransList, NewMacroList, List). 

wild_wombat(config(Left, State, Item, _Dir, Right), _M, _Bound, _Inputs, Hops, NewState, OutItem, OutDir, Hops) :-
    halt_state(State), 	NewState = State, collect(Left, Item, Right, OutI), clean_tape(OutI, OutItem), OutDir = r.

wild_wombat(config(Left, State, Item, Dir, Right), _M, Bound, _Inputs, Hops, loop, OutItem, Dir, Hops) :-
    \+ halt_state(State), Hops >= Bound, collect(Left, Item, Right, OutI), clean_tape(OutI, OutItem).

wild_wombat(config(Left, State, Item, Dir, Right), _M, Bound, _Inputs, Hops, State, OutItem, OutDir, Hops) :-
    \+ halt_state(State), Hops < Bound, out_of_bounds(Left, Item, Right, Dir, OutItem, OutDir). % This is normal termination. 

wild_wombat(config(Left, State, Item, Dir, Right), M, Bound, Inputs, Hops, NewState, OutItem, OutDir, NewHops) :-
    \+ halt_state(State), Hops < Bound, \+ out_of_bounds(Left, Item, Right, Dir, _, _), 
    simple_transition(M, State, Item, NState, NItem, NDir, Steps), 
    updatetape(naive, Left, NItem, Right, NDir, NLeft, NewItem, NRight, _Moves), 
    NewConfig = config(NLeft, NState, NewItem, NDir, NRight), 
    Hops1 is Hops + Steps, 
    wild_wombat(NewConfig, M, Bound, Inputs, Hops1, NewState, OutItem, OutDir, NewHops).
    
out_of_bounds([], left_end, Right, l, OutItem, l) :- clean_tape(Right, OutItem).
out_of_bounds(Left, right_end, [], r, OutItem, r) :- reverse(Left, New), clean_tape(New,OutItem). 

clean_tape([], []).
clean_tape([left_end|Rest], Result) :- clean_tape(Rest, Result). 
clean_tape([right_end|Rest], Result) :- clean_tape(Rest, Result). 
clean_tape([I|Rest], [I|Result]) :- I \== left_end, I\== right_end, clean_tape(Rest, Result). 

updatetape(Mode, Left, Item, Right, l, NewLeft, NewItem, NewRight, Move) :-
     append([Item], Right, NRight), check_blank(NRight, NewRight), 
     next_item(Mode, Left, NewItem, NLeft), 
     check_blank(NLeft, NewLeft), 
     Move is 0 - 1. 

updatetape(Mode, Left, Item, Right, r, NewLeft, NewItem, NewRight, 1) :-
     append([Item], Left, NLeft), check_blank(NLeft, NewLeft), 
     next_item(Mode, Right, NewItem, NRight),
     check_blank(NRight, NewRight).

updatetapeflex(Config, Mode, trans(State, I1, Dir, NewState, OutI, OutDir, Steps), NewLeft, NewItem, NewDir, NewRight, Leaps, Moves) :-
     Config = config(Left, State, Item, Dir, Right), 
     Item = grp(I2, Number), I1 == I2, 
     State == NewState, Dir == l, OutDir == r,  !, % Holkner case 1

     squash_item(grp(OutI,Number), NItem), 
     add_item(NItem, Left, NewLeft), 
     next_item(Mode, Right, NewItem, NewRight),
     NewDir = l,
     length(I1, N),

     set_leaps_moves(l,r,Number, N, Steps, Leaps, Moves).

updatetapeflex(Config, Mode, trans(State, I1, Dir, NewState, OutI, OutDir, Steps), NewLeft, NewItem, NewDir, NewRight, Leaps, Moves) :-
     Config = config(Left, State, Item, Dir, Right), 
     Item = grp(I2, Number), I1 == I2, % integer(Number), %% will work for variables too
     State == NewState, Dir == r, OutDir == l,  !, % Holkner case 2

     squash_item(grp(OutI,Number), NItem), 
     add_item(NItem, Right, NewRight), 
     
     next_item(Mode, Left, NewItem, NewLeft),
     NewDir = r,
     length(I1, N),

     set_leaps_moves(r,l,Number, N, Steps, Leaps, Moves). 

% New acceleration cases here ... 
updatetapeflex(Config, Mode, trans(State, I1, Dir, NewState, OutI, OutDir, Steps), NewLeft, NewItem, NewDir, NewRight, Leaps, Moves) :-
     Config = config(Left, State, Item, Dir, Right), 
     State == NewState, Dir == l, OutDir == r, % Holkner case 1
     Item = grp(I2, Number), length(I1, L1), length(I2, L2), L1 > L2, K is L1//L2, % So if it is an acceleration case, we must have that I1 is an expansion of I2, and so we know L1 must be a multiple of L2. 
     !, 
     safe_squash(OutI, N, NewI), % Will bind N and NewI to appropriate values so that OutI = (NewI)^N. If N = 1 and NewI = OutI, no squashing has occurred ... In any case, length(OutI) is N*length(NewI). 
     set_exponent(N, Number, K, Exp1), simplify_exp2(Exp1, Exp), 
     set_modulus(Number, K, Mod),
     add_item(grp(NewI, Exp), Left, NewLeft),
     ( (ground(Mod), Mod == 0) -> next_item(Mode, Right, NewItem, NewRight) ; (NewItem = grp(I2,Mod), NewRight = Right) ), % If N*(Number mod K) is 0, move to next item in Right. Otherwise leave Item alone. 

     NewDir = l,
     set_leaps_acc(Number, K, Steps, Leaps),
     set_moves_acc(l, r, Number, K, L1, Moves).

updatetapeflex(Config, Mode, trans(State, I1, Dir, NewState, OutI, OutDir, Steps), NewLeft, NewItem, NewDir, NewRight, Leaps, Moves) :-
     Config = config(Left, State, Item, Dir, Right),
     State == NewState, Dir == r, OutDir == l,  % Holkner case 2
     Item = grp(I2, Number), length(I1,L1), length(I2, L2), L1 > L2, K is L1//L2, % So if it is an acceleration case, we must have that I1 is an expansion of I2, and so we know L1 must be a multiple of L2.
     !, 

     safe_squash(OutI, N, NewI), % Will bind N and NewI to appropriate values so that OutI = (NewI)^N. If N = 1 and NewI = OutI, no squashing has occurred ... In any case, length(OutI) is N*length(NewI).
     set_exponent(N, Number, K, Exp1),
     set_modulus(Number, K, Mod),
     simplify_exp2(Exp1, Exp),
     add_item(grp(NewI, Exp), Right, NewRight),

     ( (ground(Mod), Mod == 0) -> next_item(Mode, Left, NewItem, NewLeft) ; (NewItem = grp(I2,Mod), NewLeft = Left) ), % If N*(Number mod K) is 0, move to next item in Left. Otherwise leave Item alone.  
     NewDir = r,
     set_leaps_acc(Number, K, Steps, Leaps),
     set_moves_acc(r, l, Number, K, L1, Moves). 

updatetapeflex(Config, Mode, trans(State, I, Dir, _NewState, OutI, OutDir, Steps), NewLeft, NewItem, NewDir, NewRight, Leaps, Moves) :-
     Config = config(Left, State, Item, Dir, Right), 
     Item = grp(I, Number), %% (State \== NewState; (State == NewState, Dir == r)), 
     ground(Number), 1 is Number, OutDir == r,  !, 

     add_item(grp(OutI, Number), Left, NewLeft), 
     next_item(Mode, Right, NewItem, NewRight),

     NewDir = l,
     Leaps = Steps, 
     length(I, L), 
     set_moves(Dir, OutDir, L, Moves).

updatetapeflex(Config, Mode, trans(State, I, Dir, _NewState, OutI, OutDir, Steps), NewLeft, NewItem, NewDir, NewRight, Leaps, Moves) :-
     Config = config(Left, State, Item, Dir, Right), 
     Item = grp(I, Number), %% (State \== NewState; (State == NewState, Dir == l)), 
     ground(Number), 1 is Number, OutDir == l,  !, 
     add_item(grp(OutI, Number), Right, NewRight),
     next_item(Mode, Left, NewItem, NewLeft),

     NewDir = r,
     Leaps = Steps, 
     length(I, L), 
     set_moves(Dir, OutDir, L, Moves).

updatetapeflex(Config, Mode, trans(State, I, Dir, _NewState, OutI, OutDir, Steps), NewLeft, NewItem, NewDir, NewRight, Leaps, Moves) :-
     Config = config(Left, State, Item, Dir, Right), 
     Item = grp(I, Number),   ( (ground(Number), NTemp is Number, NTemp > 1) ; nonground(Number,_) ),    
     Dir == r, OutDir = r, OutI == I, !, 
     add_item(grp(I, Number), Left, NewLeft), 
     next_item(Mode, Right, NewItem, NewRight),  

     NewDir = l,
     Leaps = Steps, 
     Moves = 1. 

updatetapeflex(Config, Mode, trans(State, I, Dir, _NewState, OutI, OutDir, Steps), NewLeft, NewItem, NewDir, NewRight, Leaps, Moves) :-
     Config = config(Left, State, Item, Dir, Right), 
     Item = grp(I, Number),   ( (ground(Number), NTemp is Number, NTemp > 1) ; nonground(Number,_)),  % format("Case 6~n", []), display(I), display(OutI), nl,  
     Dir = r, OutDir = r, OutI \== I, !, 

     simplify_exp2(Number-1, N1), 
     add_item(grp(I, N1), Left, NLeft),
     squash_item(grp(OutI,1), NewOut), 
     add_item(NewOut, NLeft, NewLeft), 
     next_item(Mode, Right, NewItem, NewRight),  

     NewDir = l, 
     Leaps = Steps,
     Moves = 1. 

updatetapeflex(Config, Mode, trans(State, I, Dir, _NewState, OutI, OutDir, Steps), NewLeft, NewItem, NewDir, NewRight, Leaps, Moves) :-
     Config = config(Left, State, Item, Dir, Right), 
     Item = grp(I, Number),  ( (ground(Number), NTemp is Number, NTemp > 1) ; nonground(Number,_) ), 
     Dir = l, OutDir = l, OutI == I, !, 

     add_item(grp(I, Number), Right, NewRight), 
     next_item(Mode, Left, NewItem, NewLeft),  

     NewDir = r,
     Leaps = Steps, 
     Moves = -1. 

updatetapeflex(Config, Mode, trans(State, I, Dir, _NewState, OutI, OutDir, Steps), NewLeft, NewItem, NewDir, NewRight, Leaps, Moves) :-
     Config = config(Left, State, Item, Dir, Right), 
     Item = grp(I, Number), ( (ground(Number), NTemp is Number, NTemp > 1) ; nonground(Number,_)), 
     Dir = l, OutDir = l, OutI \== I, !, 

     simplify_exp2(Number-1, N1), 
     add_item(grp(I, N1), Right, NRight),
     squash_item(grp(OutI,1), NewOut), 
     add_item(NewOut, NRight, NewRight),
     next_item(Mode, Left, NewItem, NewLeft),  

     NewDir = r,
     Leaps = Steps, 
     Moves = -1. 

updatetapeflex(Config, _Mode, trans(State, I, Dir, NewState, OutI, OutDir, Steps), NewLeft, NewItem, NewDir, NewRight, Leaps, Moves) :-
     Config = config(Left, State, Item, Dir, Right), 
     Item = grp(I, Number), ( (ground(Number), NTemp is Number, NTemp > 1) ; nonground(Number,_) ), 
     State \== NewState, Dir == l, OutDir == r, !, 

     simplify_exp2(Number-1, N1), 
     NewItem = grp(I, N1),
     squash_item(grp(OutI,1), NItem), 
     add_item(NItem, Left, NewLeft), 
     NewDir = l, 
     NewRight = Right,
     Leaps = Steps, 
     length(I, N), 
     Moves = N.
      
updatetapeflex(Config, _Mode, trans(State, I, Dir, NewState, OutI, OutDir, Steps), NewLeft, NewItem, NewDir, NewRight, Leaps, Moves) :-
     Config = config(Left, State, Item, Dir, Right), 
     Item = grp(I, Number), ( (ground(Number), NTemp is Number, NTemp > 1) ; nonground(Number,_) ), 
     State \== NewState, Dir == r, OutDir == l, !, 

     simplify_exp2(Number-1, N1), 
     NewItem = grp(I, N1),
     squash_item(grp(OutI,1), NItem),
     add_item(NItem, Right, NewRight), 
     
     NewDir = r,
     NewLeft = Left, 
     Leaps = Steps,
     length(I, N), 
     Moves = -N.

updateinputs(Mode, Hops, Dir, Moves, Used, Inputs, NewConfig, NewInputs) :-
     delete(Inputs, used(_), NInputs),
     append(NInputs, [used(Used)], Inputs0),
     update_history(Hops, Used, Inputs0, NewConfig, Inputs1), 
     update_pos(Mode, Inputs1, Dir, Moves, Inputs2), 
     update_steps(Inputs2, Inputs3), 
     NewInputs = Inputs3.

update_history(Hops, Used, Inputs, NewConfig, NewInputs) :-
     member(history(History), Inputs), member(pos(Current), Inputs), member(now(Now), Inputs),
     set_type(Inputs, Type), 
     Now = config(_, State, _, Dir, _), 
     history_index(Type, Now, Index), 
     add_to_history(Type, State, Dir, Index, s(Hops,Now,Current,Used), History, NewHistory), !, % history update
     delete(Inputs, history(History), Inps), 
     delete(Inps, now(Now), Is), 
     append(Is, [now(NewConfig),history(NewHistory)], NewInputs), !. 

update_history(_Hops, _Used, Inputs, _NewConfig, Inputs) :- \+ (member(history(_History), Inputs), member(now(_Now), Inputs)).

set_type(Inputs, trie(K)) :- member(trie(K), Inputs). 
set_type(Inputs, newtrie(K,L)) :- member(newtrie(K,L), Inputs), \+ member(trie(_), Inputs). 
set_type(Inputs, list(K)) :- member(list(K), Inputs), \+ member(newtrie(_,_), Inputs), \+ member(trie(_), Inputs). 
set_type(Inputs, list) :- \+ member(list(_), Inputs), \+ member(newtrie(_,_), Inputs), \+ member(trie(_), Inputs). 

initial_history(list, _, _, []).
initial_history(list(_), _, _, []).
initial_history(trie(_), States, Directions, History) :- empty_dir_list(Directions, DList), empty_state_list(States, DList, History).
initial_history(newtrie(_,_), States, Directions, History) :- empty_dir_list(Directions, DList), empty_state_list(States, DList, History).

empty_dir_list(2, [tr(l,[]),tr(r,[])]). % Only do two for now.

empty_state_list(2, DList, [tr(a,DList),tr(b,DList)]). 
empty_state_list(3, DList, [tr(a,DList),tr(b,DList),tr(c,DList)]). 
empty_state_list(4, DList, [tr(a,DList),tr(b,DList),tr(c,DList),tr(d,DList)]). 
empty_state_list(5, DList, [tr(a,DList),tr(b,DList),tr(c,DList),tr(d,DList),tr(e,DList)]). 
empty_state_list(6, DList, [tr(a,DList),tr(b,DList),tr(c,DList),tr(d,DList),tr(e,DList),tr(f,DList)]). 
empty_state_list(7, DList, [tr(a,DList),tr(b,DList),tr(c,DList),tr(d,DList),tr(e,DList),tr(f,DList),tr(g,DList)]). 
empty_state_list(8, DList, [tr(a,DList),tr(b,DList),tr(c,DList),tr(d,DList),tr(e,DList),tr(f,DList),tr(g,DList),tr(h,DList)]). 

% Trie structure is ... [tr(a,A),tr(b,B),tr(c,C),tr(d,D),tr(e,E)]
% A is [tr(l,Left),tr(r,Right)]
% Left is [tr(0,Zero),tr(1,One),tr(2,Two)]

history_index(list(_),  _, []) :- !. 
history_index(list,  _, []) :- !. 
history_index(trie(_),  Config, I) :-
	Config = config(_, _, Item, _, _), Item = grp(I,_), !. 
history_index(newtrie(_,L), Config, Index) :-
	Config = config(Left, _, Item, _, Right), Item = grp(I,_), 
	shapelist(L, 0, Left, [], LS), !, 
	shapelist(L, 0, Right, [], RS), !, 
	append(I, LS, Temp), append(Temp, RS, Index), !. 

shapelist(_, _, [], SoFar, SoFar) :- !. 
shapelist(Limit, Length, _, SoFar, SoFar) :- Length >= Limit, !. 
shapelist(Limit, Length, [grp(I,_)|Rest],SoFar, Result) :- Length < Limit, !, append(SoFar, I, NewSoFar), length(I, N), NewLength is Length + N, shapelist(Limit, NewLength, Rest, NewSoFar, Result).

lookup_history(list,    _, _, _, s(Hops, Config, Pos, Used), History) :- member(s(Hops, Config, Pos, Used), History). % For history as a list. 
lookup_history(list(_), _, _, _, s(Hops, Config, Pos, Used), History) :- member(s(Hops, Config, Pos, Used), History). % For history as a list. 
lookup_history(trie(_), State, Dir, Item, Step, History) :- % For history as a trie.
     member(tr(State, SList), History), !, % History is of the form [tr(a,A),tr(b,B),...]		 
     member(tr(Dir, DList), SList), !,  % SList is of the form [tr(l,L),tr(r,R)]	
     itemlist(Item, Step, DList). % DList is of the form [tr(0,Zero),tr(1,One),...]					

lookup_history(newtrie(_,_), State, Dir, Item, Step, History) :- % For history as a trie.
     member(tr(State, SList), History), !, % History is of the form [tr(a,A),tr(b,B),...]		 
     member(tr(Dir, DList), SList), !,  % SList is of the form [tr(l,L),tr(r,R)]	
     itemlist(Item, Step, DList). % DList is of the form [tr(0,Zero),tr(1,One),...]					

add_to_history(list, _, _, _, s(Hops,Now,Current,Used), History, NewHistory) :- 
     append([s(Hops,Now,Current,Used)], History, NewHist), !, 
     trim_to_size(list, NewHist, NewHistory), true. % history update.
add_to_history(list(K), _, _, _, s(Hops,Now,Current,Used), History, NewHistory) :- 
     append([s(Hops,Now,Current,Used)], History, NewHist), !, 
     trim_to_size(list(K), NewHist, NewHistory), true. % history update.
add_to_history(Type, State, Dir, Item, Step, History, NewHistory) :- % For history as a trie.
     member(Type, [newtrie(_,_), trie(_)]), 
     member(tr(State, SList), History), !, % History is of the form [tr(a,A),tr(b,B),...]		 
     member(tr(Dir, DList), SList), !,  % SList is of the form [tr(l,L),tr(r,R)]
     add_to_itemlist(Type, Item, Step, DList, NewDList), !, % DList is of the form [tr(0,Zero),tr(1,One),...]
     delete(SList, tr(Dir, DList), SListTemp), append([tr(Dir,NewDList)], SListTemp, NewSList), 
     delete(History, tr(State, SList), HistTemp), append([tr(State, NewSList)], HistTemp, NewHistory),!. 

itemlist([], Step, List) :- extract_step(Step, List). 
itemlist([I|Rest], Step, List) :- member(tr(I, IList), List), !, itemlist(Rest, Step, IList). 
itemlist([I|_Rest], Step, List) :- \+ member(tr(I, _IList), List), !, extract_step(Step, List). 

add_to_itemlist(trie(K), [], Step, List, NewIList) :- 
	append([Step], List, New), !, trim_to_size(trie(K), New, NewIList).
add_to_itemlist(trie(K), [I|Rest], Step, List, NewList) :- 
	member(tr(I, IList), List), !, 
	add_to_itemlist(trie(K), Rest, Step, IList, NewIList),
	delete(List, tr(I, IList), ListTemp), append([tr(I,NewIList)], ListTemp, NewList). 
add_to_itemlist(trie(_), [I|_Rest], Step, List, NewList) :- 
	\+ member(tr(I, _IList), List), !, append([tr(I,[Step])], List, NewList). 
add_to_itemlist(newtrie(K,L), [], Step, List, NewIList) :- 
	append([Step], List, New), !, trim_to_size(newtrie(K,L), New, NewIList).
add_to_itemlist(newtrie(K,L), [I|Rest], Step, List, NewList) :- 
	member(tr(I, IList), List), !, % format("Adding ~k~n", [I]), ttyflush, 				
	add_to_itemlist(newtrie(K,L), Rest, Step, IList, NewIList),
	delete(List, tr(I, IList), ListTemp), append([tr(I,NewIList)], ListTemp, NewList). 
add_to_itemlist(newtrie(K,L), [I|Rest], Step, List, NewList) :- 
	\+ member(tr(I, _IList), List), !, % format("Adding new ~k~n", [I]), ttyflush, 				
	add_to_itemlist(newtrie(K,L), Rest, Step, [], TList), !, append([tr(I,TList)], List, NewList). 

extract_step(Step, [Step|_List]) :- Step = s(_,_,_,_).
extract_step(Step, [Step1|_List]) :- Step1 = tr(_, Rest), extract_step(Step, Rest). 
extract_step(Step, [_|List]) :- extract_step(Step, List). 

trim_to_size(Type, History, History) :- length(History, Len), history_memory(Type, Size), Len =< Size, !. 
trim_to_size(Type, History, Trimmed) :- length(History, Len), history_memory(Type, Size), Len > Size, !, append(Trimmed, _, History), length(Trimmed, Size). 

display_history(_, []). 
display_history(N, [s(Hops, Config, Pos)|Rest]) :- 
      output_blanks(N), 
      format("~d: ", [Hops]), display_config(Config, []), format(" Pos: ~d~n", [Pos]), 
      display_history(N, Rest).
display_history(N, [s(Hops, Config, Pos, Used)|Rest]) :- 
      output_blanks(N), 
      format("~d: ", [Hops]), display_config(Config, []), format(" Pos: ~d    Used: ~k~n", [Pos,Used]), 
      display_history(N, Rest).
display_history(N, [tr(I, List)|Rest]) :- 
      N1 is N+1, output_blanks(N), 
      format("~k: ~n", [I]), 
      display_history(N1, List), display_history(N, Rest).

update_steps(Inputs, NewInputs) :- member(steps(S), Inputs), !, delete(Inputs, steps(S), Inputs1), eager_sum(S, 1, S1), append([steps(S1)], Inputs1, NewInputs). 
update_steps(Inputs, Inputs). 

update_otters(Leaps, ExtraSteps, Nested, ProofLength, Diff1, Diff2, Inputs, NewInputs) :-
     member(otters(Otters), Inputs), NewOtters is Otters + 1, 
     member(ottersnested(OttersNested), Inputs), NewOttersNested is OttersNested + Nested, 
     member(ottersteps(OtterSteps), Inputs), ( ground(Leaps) -> NewOtterSteps is OtterSteps + Leaps; NewOtterSteps = OtterSteps ), 
     member(otterjumps(OtterJumps), Inputs), ( ground(Leaps) -> NewOtterJumps is OtterJumps + Leaps + ExtraSteps; NewOtterJumps = OtterJumps ),  
     member(otterproofs(OtterProofs), Inputs), append(OtterProofs, [o(NewOtters,ProofLength)], NewOtterProofs),
     member(otterdiffs(OtterDiffs), Inputs), append(OtterDiffs, [o(NewOtters,d(Diff1,Diff2))], NewOtterDiffs), !, 
     delete(Inputs, otters(Otters), Inputs1), 
     delete(Inputs1, ottersnested(OttersNested), Inputs2), 
     delete(Inputs2, ottersteps(OtterSteps), Inputs3), 
     delete(Inputs3, otterjumps(OtterJumps), Inputs4),
     delete(Inputs4, otterproofs(OtterProofs), Inputs5),
     delete(Inputs5, otterdiffs(OtterDiffs), Inputs6), 
     append([otters(NewOtters),ottersnested(NewOttersNested),ottersteps(NewOtterSteps),otterjumps(NewOtterJumps),otterproofs(NewOtterProofs),otterdiffs(NewOtterDiffs)], Inputs6, NewInputs),!.
update_otters(_,_,_,_,_,_, Inputs, Inputs). 
% update_otters(_,_,_,_,_,_, Inputs, Inputs) :- \+ member(ottersnested(_OttersNested), Inputs). 
% update_otters(_,_,_,_,_,_, Inputs, Inputs) :- \+ member(ottersteps(_OtterSteps), Inputs). 
% update_otters(_,_,_,_,_,_, Inputs, Inputs) :- \+ member(otterjumps(_OtterJumps), Inputs).
% update_otters(_,_,_,_,_,_, Inputs, Inputs) :- \+ member(otterproofs(_OtterJumps), Inputs).
% update_otters(_,_,_,_,_,_, Inputs, Inputs) :- \+ member(otterdiffs(_OtterJumps), Inputs). 

update_ocelots(Leaps, Inputs, NewInputs) :-
     member(ocelots(Ocelots), Inputs), ground(Leaps), 
     member(ocelotsteps(OcelotSteps), Inputs), !, 
     NewOcelots is Ocelots + 1,
     NewOcelotSteps is OcelotSteps + Leaps, 
     delete(Inputs, ocelots(Ocelots), Inputs1), 
     delete(Inputs1, ocelotsteps(OcelotSteps), Inputs2), 
     append([ocelots(NewOcelots),ocelotsteps(NewOcelotSteps)], Inputs2, NewInputs).
update_ocelots(Leaps, Inputs, NewInputs) :-
     member(ocelots(Ocelots), Inputs), \+ ground(Leaps), 
     member(ocelotproofs(OcelotProofs), Inputs), !, 
     NewOcelots is Ocelots + 1,
     NewOcelotProofs is OcelotProofs + 1, 
     delete(Inputs, ocelots(Ocelots), Inputs1), 
     delete(Inputs1, ocelotproofs(OcelotProofs), Inputs2), 
     append([ocelots(NewOcelots),ocelotproofs(NewOcelotProofs)], Inputs2, NewInputs).
update_ocelots(Inputs, Inputs).
% update_ocelots(Inputs, Inputs) :- \+ member(ocelotsteps(_OcelotSteps), Inputs). 

update_lemmas(Lemma, Inputs, NewInputs) :-
    member(lemmas(OldLemmas), Inputs), delete(Inputs, lemmas(OldLemmas), Inputs1),
    append([Lemma], OldLemmas, NewLemmas), append([lemmas(NewLemmas)], Inputs1, NewInputs). 

update_pos(Mode, Inputs, _Dir, Num, Inputs1) :-
      \+ member(Mode, [naive]), member(pos(Current), Inputs),
      delete(Inputs, pos(Current), Is),
      eager_sum(Current, Num, NCurrent), simplify_exp2(NCurrent, NewCurrent), 
      append([pos(NewCurrent)], Is, Inputs1). 
update_pos(Mode, Inputs, Dir, _Num, Inputs1) :-
      member(Mode, [naive]), member(pos(Current), Inputs),
      delete(Inputs, pos(Current), Is),
      set_new_current(Current, Dir, 1, NewCurrent), 
      append([pos(NewCurrent)], Is, Inputs1). 
update_pos(Inputs, _Dir, _Num, Inputs) :- \+ member(pos(_Current), Inputs).

update_machine(M, _Hops, Transition, NewM, new) :-
    M = machine(Trans, Macro, Ocelots),
    new_otter(Transition, Ocelots), !, 
    length(Ocelots, L), L1 is L+1,
    append(Ocelots, [o(L1,Transition)], NewOcelots),
    NewM = machine(Trans, Macro, NewOcelots). 
update_machine(M, Hops, _Transition, M, old) :- format("% Otter found at ~k not added due to being previously known~n", [Hops]). 

new_otter(_Transition, []). 
new_otter(Transition, Ocelots) :-
    Ocelots \== [], Transition = oo(Initial, _Diffs, _, _, _, _, _),
    \+ ( member(o(_Num, Ocelot), Ocelots), Ocelot = oo(Initial1, _Diffs1, _, _, _, _, _), same_otter(Initial1, Initial) ). 

% same_otter(Config1, Config2, Diffs1, Diffs2) :- same_shape(Config1, Config2), trim_diffs(Config1, Diffs1, Trim1), trim_diffs(Config2, Diffs2, Trim2), Trim1 == Trim2.  % same_shape is not precise enough ...
same_otter(Config1, Config2) :- same_otter_config(Config1, Config2).

trim_diffs(Config, Diffs, Trim) :-
    Config = config(Left, _State, _Item, _Dir, Right),
    Diffs = diffs(LDiffs, ItemDiff, RDiffs),
    trim_diffs1(Left, LDiffs, TrimLDiffs),
    trim_diffs1(Right, RDiffs, TrimRDiffs),
    Trim = diffs(TrimLDiffs, ItemDiff, TrimRDiffs). 

trim_diffs1(Tape, Diffs, TrimDiffs) :- length(Tape, L), length(Diffs, D), L =< D, !, append(TrimDiffs, _, Diffs), length(TrimDiffs, L). % Get first L elements of Diffs
trim_diffs1(Tape, Diffs, Diffs) :-     length(Tape, L), length(Diffs, D), L > D. % Otherwise, get all the Diffs. This case is for padding. 

set_new_current(Current, l, Leap, NewCurrent) :- Amount is 0 - Leap, eager_sum(Current, Amount, NewCurrent). % simplify_exp2(Current - Leap, NewCurrent)??
set_new_current(Current, r, Leap, NewCurrent) :- eager_sum(Current, Leap, NewCurrent). % simplify_exp2(Current + Leap, NewCurrent)??

otters_in_machine(M, Number) :- M = machine(_, _, Otters), length(Otters, Number). 

output_status(M, Config, Mode, Hops, Inputs, NewInputs, Used) :-  yes_to_output(Config, Hops, Inputs, NewInputs), !, output_config(M, Config, Mode, Hops, NewInputs, Used), nl, flush_output. 
output_status(_M, _Config, _Mode, _Hops, Inputs, Inputs, _Used).

yes_to_output(_Config, _Hops, Inputs, Inputs) :- member(trace, Inputs),!.
yes_to_output(Config, _Hops, Inputs, Inputs)  :- member(watch(State), Inputs), Config = config(_Left, State, _Item, _Dir, _Right), !. 
yes_to_output(Config, _Hops, Inputs, Inputs)  :- member(watch(State, Item), Inputs), Config = config(_Left, State, Item, _Dir, _Right), !. 
yes_to_output(Config, _Hops, Inputs, Inputs)  :- member(match(Pattern), Inputs), match_config(Pattern, Config), !. 
yes_to_output(_Config, Hops, Inputs, Inputs)  :- member(trace(Number), Inputs), Hops >= Number, !.
yes_to_output(Config, _Hops, Inputs, Inputs)  :- member(rend, Inputs), Config = config(_Left, _State, _Item, _Dir, []), !. 
yes_to_output(Config, _Hops, Inputs, Inputs) :-  member(lend, Inputs), Config = config([], _State, _Item, _Dir, _Right), !. 
yes_to_output(_Config, Hops, Inputs, NewInputs) :- member(over(Number), Inputs), Hops >= Number, !, delete(Inputs, over(Number), Inputs1), member(every(Every), Inputs1), NewOver is Number + Every, append([over(NewOver)], Inputs1, NewInputs).  

output_config(M, Config, Mode, Hops, Inputs, Used) :-
      member(flatout, Inputs), % format("Flat out ... drinking lizards!~n", []), ttyflush, 
      flatten(Config, FlatConfig), output_config1(FlatConfig, Mode, Hops, Inputs, Used), output_nonzeroes(M, FlatConfig).
output_config(_M, Config, Mode, Hops, Inputs, Used) :- \+ member(flatout, Inputs), output_config1(Config, Mode, Hops, Inputs, Used).

output_config1(Config, Mode, Hops, Inputs, Used) :-
      Config = config(Left, State, Item, Dir, Right), !,
      count_ones(Config, Ones), 
      member(pos(Pos), Inputs), 
      member(variable_names(Names), Inputs),
      ( member(starttime(T1), Inputs) -> Time = T1; Time = 0 ), 
      prettyprint(Names, State, Left, Item, Dir, Pos, Mode, Right, Hops, Ones, Used, Time),
      ( member(shapeout, Inputs) -> (extract_shape(Config, Shape), format(" Shape: ", []), display_config(Shape, []) ) ; true ), 
      !.

output_nonzeroes(M, Config) :- count_nonzeroes(M, Config, Profile), format("	Profile ", []), output_nonzeroes1(Profile). 

output_nonzeroes1([]).
output_nonzeroes1([count(I,Num)|Rest]) :- format("#~d: ~d ", [I, Num]), output_nonzeroes1(Rest). 

count_nonzeroes(M,config(Left, _State, Item, _Dir, Right), Profile) :-
     number_of_symbols(M, N), 
     initialise_counts(N, Counts0), 
     count_nonzeroes1(N, Left, Counts0, Counts1), 
     count_nonzeroes1(N, [Item], Counts1, Counts2), 
     count_nonzeroes1(N, Right, Counts2, Counts3), 
     Profile = Counts3. 

count_nonzeroes1(_, [], Counts, Counts).
count_nonzeroes1(N, [Item|Rest], Counts, NewCounts) :-
      is_blank(Item), !, 
      count_nonzeroes1(N, Rest, Counts, NewCounts).
count_nonzeroes1(N, [Item|Rest], Counts, NewCounts) :-
      is_input(Item), 
      count_item(Item, Counts, Counts1), 
      count_nonzeroes1(N, Rest, Counts1, NewCounts),
      true. 
count_nonzeroes1(N, [grp(I,Num)|Rest], Counts, NewCounts) :-
      initialise_counts(N, Initial),
      count_nonzeroes1(N, I, Initial, Count1), 
      multiply_counts(Num, Count1, Count2), 
      add_counts(Counts, Count2, Counts3), 
      count_nonzeroes1(N, Rest, Counts3, NewCounts).

count_item(Item, Counts, NewCounts) :-
      member(count(Item, Count), Counts),
      delete(Counts, count(Item, Count), Counts1),
      NewCount is Count + 1,
      append([count(Item, NewCount)], Counts1, NewCounts).

multiply_counts(_N, [], []).
multiply_counts(N, [count(I,Num)|Rest], [count(I,NewNum)|Result]) :- NewNum is N*Num, multiply_counts(N, Rest, Result). 

add_counts([], Counts, Counts). 
add_counts([count(I,Num)|Rest], Counts, NewCounts) :- add_counts1(I, Num, Counts, Counts1), add_counts(Rest, Counts1, NewCounts). 

add_counts1(I, Num, Counts, NewCounts) :-
      member(count(I,N), Counts),
      delete(Counts, count(I,N), Counts2),
      NewN is N+Num, 
      append([count(I,NewN)], Counts2, NewCounts). 

initialise_counts(2, [count(0,0),count(1,0)]).
initialise_counts(N, Counts) :- N > 2, N1 is N-1, initialise_counts(N1, Counts1), append(Counts1, [count(N1,0)], Counts). 

display_config(config(Left, State, Item, Dir, Right), Names) :-
     % (Names1 = [] -> (term_variables(config(Left, State, Item, Dir, Right), Vs), name_variables(normal, 1, Vs, [], Names)) ; Names = Names1 ), 
     reverse(Left, RevLeft),
     pprint(Names, RevLeft), ttyflush, format("{~k}", [State]), ttyflush, pprint(Names, [Item]), ttyflush, format("{~k}", [Dir]), ttyflush, pprint(Names, Right), true. 

new_leftify(Tape, Left) :- reverse(Tape, Rev), reverse_items(Rev, Temp), reverse(Temp, Left). 

reverse_items([], []).
reverse_items([Item|Rest], [Item|Result]) :- is_input(Item), reverse_items(Rest, Result).
reverse_items([Item|Rest], [grp(RevI,Number)|Result]) :- Item = grp(I,Number), reverse(I, RevI), reverse_items(Rest, Result).

display_trans([]).
display_trans([Trans|Rest]) :-
      Trans = trans(State, In, InDir, NewState, Out, OutDir, Steps),
      format("% Trans: ~k ", [State]), display_item(In), format(" ~k -> ~k ", [InDir,NewState]), display_item(Out), format(" ~k Steps: ~d~n", [OutDir,Steps]),
      display_trans(Rest). 

display_otters([], _).
display_otters([Otter|Rest], N) :-
         Otter = oo(InitConfig, FinalConfig, constraints(Mins,Mods), Leaps, Moves, Names),
         Mins = mins(_X, Xmin, _Y, Ymin),
         format("Otter: ", []), display_config(InitConfig, Names), 
         format(" ++> ", []),   display_config(FinalConfig, Names),
         format(" Mins Xs: ~k Ys: ~k", [Xmin,Ymin]),
         format(" Mods are ", []), write_term(Mods, [variable_names(Names)]), 
         display_leaps(Leaps, Names), display_moves(Moves, Names), 
         N1 is N+1, 
         display_otters(Rest, N1).
display_otters([Otter|Rest], N) :- 
       Otter = o(ONum, oo(InitConfig, FinalConfig, constraints(Mins,Mods), Leaps, Moves, Names)), 
       Mins = mins(_X, Xmin, _Y, Ymin), 
       format("Otter ~d: ", [ONum]), display_config(InitConfig, Names), 
       format(" ++> ", []),   display_config(FinalConfig, Names),
       format(" Mins Xs: ~k Ys: ~k", [Xmin,Ymin]),
       format(" Mods ~k ", [Mods]), 
       display_leaps(Leaps, Names), display_moves(Moves, Names), 
       N1 is N+1, 
       display_otters(Rest, N1).
display_otters([Otter|Rest], N) :-
       Otter = o(ONum, oo(InitConfig, Diffs, FinalConfig, constraints(Mins,Mods), Leaps, Moves, Names)), 
       Mins = mins(_X, Xmin, _Y, Ymin),
       format("Otter ~d: ", [ONum]), ttyflush, display_config(InitConfig, Names), ttyflush, 
       format(" ++> ", []), display_config(FinalConfig, Names), ttyflush, 
       format(" Diffs: ~k", [Diffs]), ttyflush, 
       format(" Mins Xs: ~k Ys: ~k", [Xmin,Ymin]),
       format(" Mods ~k ", [Mods]), 
       display_leaps(Leaps, Names), display_moves(Moves, Names), 
       N1 is N+1, 
       display_otters(Rest, N1).

display_leaps(Leaps, _Names) :- ground(Leaps), L is Leaps, format(" in ~d steps ", [L]).  
display_leaps(Leaps, Names) :- \+ ground(Leaps), format(" in ", []), write_term(Leaps, [numbervars(true), variable_names(Names)]), format(" steps ", []). 

display_moves(Moves, _Names) :- ground(Moves), M is Moves, format(" and movement ~d.~n", [L]).  
display_moves(Moves, Names) :- \+ ground(Moves), format(" and movement ", []), write_term(Moves, [numbervars(true), variable_names(Names)]), format(".~n", []). 

display_item([]).
display_item([A|Rest]) :- display(A), display_item(Rest).

count_ones(config(Left, _State, Item, _Dir, Right), Ones) :- count_ones1(Left, C1), count_ones1([Item], C2), count_ones1(Right, C3), Ones is C1 + C2 + C3. 
count_ones1([], 0).
count_ones1([Item|Rest], Count) :- is_blank(Item), !, count_ones1(Rest, Count).
count_ones1([Item|Rest], Count) :- is_input(Item), count_ones1(Rest, Count1), Count is Count1 + 1. 
count_ones1([grp(I,N)|Rest], Count) :- integer(N), count_ones1(I, C), count_ones1(Rest, Count1), Count is Count1 + N*C. 
count_ones1([grp(_I,N)|Rest], Count1) :- \+ integer(N), count_ones1(Rest, Count1).

% Compression

set_rotation(Inputs, rotate) :- member(rotate, Inputs), !.
set_rotation(_, plain).

compression(CMode, Config, Inputs, NewConfig) :- member(compress, Inputs), !, compress_config(CMode, Config, NewConfig), !.
compression(_CMode, Config, _Inputs, Config) :- !. 

compress_config(Mode, config(Left, State, Item, l, Right), config(NewLeft, State, NewItem, l, NewRight)) :-
       new_leftify(Left, NLeft), compresstape(Mode, NLeft, NLeft2),  new_leftify(NLeft2, NewLeft), 
       compresstape(Mode, [Item|Right], NRight), !, 
       append([NewItem], NewRight, NRight).

compress_config(Mode, config(Left, State, Item, r, Right), config(NewLeft, State, NewItem, r, NewRight)) :-
       new_leftify([Item|Left], NLeft), compresstape(Mode, NLeft, NLeft2), new_leftify(NLeft2, NLeft1), 
       append([NewItem], NewLeft, NLeft1), 
       compresstape(Mode, Right, NewRight), !. 

compresstape(Mode, Tape, NewTape) :-
    	new_squish_tape(Tape, Tape1), !, squash_tape(Tape1, Tape2), !, merge_tape(Tape2, Tape3), !,
	(Mode = rotate -> rotate_to_smaller(Tape3, Tape4); Tape4 = Tape3), 
        NewTape = Tape4.

rotate_to_smaller([], []).
rotate_to_smaller(Tape, Tape) :- ground(Tape), !. 
rotate_to_smaller(Tape, Tape2) :- \+ ground(Tape), length(Tape, N), rotate(Tape, Tape2), length(Tape2, L), L < N.
rotate_to_smaller(Tape, Tape) :- !. % All smaller rotations found by now, so nothing to do.

% rotate(Tape, Tape).
rotate(Tape, Tape) :- ground(Tape). 
rotate(Tape, NewTape) :- \+ ground(Tape), rotation([], Tape, Tape1), new_squish_tape(Tape1, Tape2), squash_tape(Tape2,Tape3), merge_tape(Tape3, Tape4), NewTape = Tape4. 

rotation(Previous, [], Previous).
rotation(Previous, [grp(I,N)|Rest], Result) :-
    length(I, 1), !, % No point unless I is at least length 2 ... 
    append(Previous, [grp(I,N)], NewPrevious), 
    rotation(NewPrevious, Rest, Result). 
rotation(Previous, [grp(I,N)|Rest], Result) :-
    length(I, M), M > 1, ground(N), !, % 1 is N,  !, % Also no point if N is 1 ... 
    append(Previous, [grp(I,N)], NewPrevious), 
    rotation(NewPrevious, Rest, Result). 
rotation(Previous, [grp(I,N)|Rest], Result) :-
     length(I, M), M > 1, \+ ground(N), % \+ (ground(N), 1 is N), % allow 'no change' case here
     % append(I1, I2, I), length(I1, N1), length(I2, N2), N1 > 0, N2 > 0, % So it is a 'real' split ...
     append(Previous, [grp(I,N)], NewPrevious), 
     rotation(NewPrevious, Rest, Result). 
rotation(Previous, [grp(I,N)|Rest], Result) :-
    length(I, M), M > 1, \+ ground(N), % \+ (ground(N), 1 is N), 
    append(I1, I2, I), length(I1, N1), length(I2, N2), N1 > 0, N2 > 0, % So it is a 'real' split ...
    append(I2, I1, NewI), simplify_exp2(N-1, NewN), % NewN = N - 1, 
    append(Previous, [grp(I1,1),grp(NewI, NewN),grp(I2,1)], NewPrevious), 
    rotation(NewPrevious, Rest, Result). 

merge_tape(Tape, Tape) :- length(Tape, N), N < 2, !. 
merge_tape([Item|Rest], Result) :-
        length([Item|Rest], N), N >= 2, 
	Item = grp(I,N1),
	Rest = [Item2|Rest2],
	Item2 = grp(I,N2), !,
	eager_sum(N1, N2, NewN), 
	merge_tape([grp(I,NewN)|Rest2], Result). 
merge_tape([Item|Rest], [Item|Result]  ) :-
        length([Item|Rest], N), N > 1, 
	Item = grp(I,N1),
	Rest = [Item2|Rest2],
	( Item2 \== grp(I,_); ( Item2 = grp(I,N2), \+ ( ground(N1), ground(N2) ) ) ), !, 
	merge_tape([Item2|Rest2], Result  ). 

squash_tape([], []) :- !. 
squash_tape([Item|Rest], [NewItem|Result]) :- squash_item(Item, NewItem), !, squash_tape(Rest, Result). 

squash_item(grp(I,Num), grp(L, NewNum)) :-  ground(Num), squash(I, K, L), !, NewNum is K*Num. 
squash_item(grp(I,Num), grp(L, NewNum)) :-  \+ ground(Num), squash(I, K, L), !, NewNum = K*Num. 
squash_item(grp(I,Num), grp(I, Num)). % Assume other cases failed ... 
	
% squash(List, K, NewList)
% Can squash List into K copies of NewList. So squash([1,1,1], 3, [1]) and squash([0,1,0,1], 2, [0,1]) both hold. 

safe_squash(List, K, NewList) :- squash(List, K, NewList), !.
safe_squash(List, 1, List). % Only if previous case failed ... 

squash(List, K, NewList) :-
      length(List, N), 
      N > 1, % can't squash anything smaller
      sublists(N, NList), member(M, NList),
      all_sublists_same(List, M, NewList),
      K is N // M, !. 

all_sublists_same(List, M, One) :- split(List, M, [One|Rest]), all_same(One, Rest).

split(List, M, [List]) :-  length(List, N), N =< M, !. 
split(List, M, [First|Lists]) :-  length(List, N), N > M, append(First, Rest, List), length(First, M), split(Rest, M, Lists). 

all_same(One, [One]) :- !. 
all_same(One, Rest) :- length(Rest, N), N > 1, Rest = [First|Others], One == First, all_same(One, Others).

% sublists(N,M)
% For a list of length N, consider only sublists of length M. There are more compact ways to do this, but this keeps it simple!
% 30 is probably excessive ... 
sublists(2,[1]).
sublists(3,[1]).
sublists(4,[1,2]).
sublists(5,[1]).
sublists(6,[1,2,3]).
sublists(7,[1]).
sublists(8,[1,2,4]).
sublists(9,[1,3]).
sublists(10,[1,2,5]).
sublists(11,[1]).
sublists(12,[1,2,3,4,6]).
sublists(13,[1]).
sublists(14,[1,2,7]).
sublists(15,[1,3,5]).
sublists(16,[1,2,4,8]).
sublists(17,[1]).
sublists(18,[1,2,3,6,9]).
sublists(19,[1]).
sublists(20,[1,2,4,5,10]).
sublists(21,[1,3,7]).
sublists(22,[1,2,11]).
sublists(23,[1]).
sublists(24,[1,2,3,4,6,8,12]).
sublists(25,[1,5]).
sublists(26,[1,2,13]).
sublists(27,[1,3,9]).
sublists(28,[1,2,4,7,14]).
sublists(29,[1]).
sublists(30,[1,2,3,5,6,10,15]).

new_squish_tape([], []) :- !. 
new_squish_tape(Tape, Tape) :- length(Tape, 1), Tape = [grp(I,N)], ground(N), I \== [], N1 is N, N1 > 0, !. 
new_squish_tape(Tape, Tape) :- length(Tape, 1), Tape = [grp(I,N)], nonground(N,_), I \== [], !. 
new_squish_tape(Tape, Result) :-
    length(Tape, N), N > 1, !, 
    purify_tape(Tape, Tape1),
    dominants(Tape1, Dominants),
    squish_to_dominants(Dominants, Tape1, Tape2),
    squish_rest(Tape2, Tape3),
    Result = Tape3. 

squish_after_dominants([], []) :- !.
squish_after_dominants(Tape, Tape) :- length(Tape,1), !. 
squish_after_dominants(Tape, NewTape) :-
    length(Tape, N), N > 1,
    Tape = [Item|_Rest], Item = grp(_I,Num), ground(Num),
    % Find appropriate pattern, if any ...   %% Sort out this case for variables ... 
    pattern_size(Size),
    next_items(Size, Tape, List1, Rest1, [ground]),  
    next_items(Size, Rest1, List2, Rest2, [ground]), 
    next_items(Size, Rest2, List3, _Rest3, [ground]), 
    List1 == List2, List2 == List3, !, 
    squish_to_pattern(grp(List1,0), Rest2, List1, grp(List1, Extra), NewRest), %% What about variables??

    squish_after_dominants(NewRest, NewSquish), !,

    eager_sum(Extra, 2, NewN), 
    append([grp(List1,NewN)], NewSquish, NewTape), !.

squish_after_dominants(Tape, NewTape) :- 
	length(Tape, N), N > 1,
        Tape = [Item|Rest],
	squish_after_dominants(Rest, NewSquish), 
        append([Item], NewSquish, NewTape), !.

squish_rest(Tape, NewTape) :- predominants(Tape, [], Prev, Post), squish_after_dominants(Prev, NewPrev), squish_next(NewPrev, Post, NewTape).

squish_next(Prev, [], Prev).
squish_next(Prev, [Item|Rest], Result) :- append(Prev, [Item], NewPrev), squish_rest(Rest, RestResult), append(NewPrev, RestResult, Result). 

% predominants(Dominants, Tape, PreDom, Rest)
% Divide Tape into PreDom, part before any Dominants, and then the Rest.
predominants([], SoFar, SoFar, []).
predominants([Item|Tape], SoFar, SoFar, [Item|Tape]) :- dominant_item(Item, _). % SoFar is what we want, Rest is [Item|Tape]
predominants([Item|Tape], SoFar, PreDom, Rest) :- \+ dominant_item(Item, _), append(SoFar, [Item], NewSoFar), predominants(Tape, NewSoFar, PreDom, Rest). 
    
purify_tape([], []) :- !. 
purify_tape(Tape, Result) :- Tape = [Item|Rest], Item = grp([], _N), purify_tape(Rest, Result), !. 
purify_tape(Tape, Result) :- Tape = [Item|Rest], Item = grp(_I, N), ground(N), 0 is N, purify_tape(Rest, Result), !. 
purify_tape(Tape, [Item|Result]) :- Tape = [Item|Rest], Item = grp(I,N), I \== [], \+ (ground(N), 0 is N), purify_tape(Rest, Result). 

squish_to_dominants(_, [], []) :- !. 
squish_to_dominants([], Tape, Tape) :- !.
squish_to_dominants([Dominant|Rest], Tape, NewTape) :- squish_to_dominant(Dominant, Tape, Tape1), squish_to_dominants(Rest, Tape1, NewTape).

squish_to_dominant(_, [], []). 
squish_to_dominant(Dominant, Tape, NewTape) :-
        length(Dominant, Size),
	next_items(Size, Tape, I2, Rest1, [ground]),
	I2 == Dominant, dominant_free(Size, Tape), % no dominants in next Size symbols on Tape
	squish_to_pattern(grp(Dominant,1), Rest1, Dominant, grp(Dominant, NewN), NewRest), %% What about variables??
	append([grp(Dominant,NewN)], NewRest, NewTape), !.
squish_to_dominant(Dominant, Tape, NewTape) :- 
	Tape = [grp(Dominant,Number)|Rest], nonground(Number, _), !, 
	squish_to_pattern(grp(Dominant,Number), Rest, Dominant, grp(Dominant, NewN), NewRest),
	append([grp(Dominant,NewN)], NewRest, NewTape), !.
% Give up and move on .... 
squish_to_dominant(Dominant, Tape, [Item|NewTape]) :- 
        Tape = [Item|Rest], !,
	squish_to_dominant(Dominant, Rest, NewTape). 

rotation_base(I, I). 
rotation_base(I, Rot) :- length(I, L), L > 1, rotation_base1(I, Rot).

rotation_base1(I, RotI) :- append(I1, I2, I), length(I1, L1), length(I2, L2), L1 > 0, L2 > 0, append(I2,I1,RotI).

dominant_free(_Size, []).
dominant_free(Size, [Item|_]) :-
	\+ dominant_item(Item, _), % If this is a dominant, then fail. So we only get further if this is ground. 
	Item = grp(I, N), length(I, L), Size =< L*N. % Item covers the next Size items, and it is not dominant, so done. 
dominant_free(Size, [Item|Rest]) :-
	\+ dominant_item(Item, _), 
	Item = grp(I, N), length(I, L), Size > L*N, % Need to check further in ...
	NewSize is Size - L*N,
	dominant_free(NewSize, Rest). 
        
pattern_size(N) :- between(1,30,N). % May need more? 

dominants([], []).
dominants([Item|Rest], [Pattern|Dominants]) :- dominant_item(Item, Pattern), dominants(Rest, Dominants). 
dominants([Item|Rest], Dominants) :- \+ dominant_item(Item, _),  dominants(Rest, Dominants). 

dominant_item(Item, I) :- Item = grp(I, Number), ground(Number), N1 is Number, N1 > 20. % Experiment with this value ...
dominant_item(Item, I) :- Item = grp(I, N), nonground(N, _). % ???

squish_to_pattern(grp(Pattern,Number), Tape, Pattern, Item, NewTape) :-
    Tape = [grp(Pattern, N)|Rest], % nonground(N,_), % I \== Pattern, 
    eager_sum(Number, N, N1), !, 
    squish_to_pattern(grp(Pattern,N1), Rest, Pattern, Item, NewTape). 
squish_to_pattern(grp(Pattern,Number), Tape, Pattern, Item, NewTape) :-
    Tape = [grp(_I, N)|_Rest], ground(N), % I \== Pattern, 
    length(Pattern, L), 
    next_items(L, Tape, List, NTape, [ground]),
    List == Pattern, % keep going --- need to differentitate here between ground and variables.
    eager_sum(Number, 1, N1), !, 
    squish_to_pattern(grp(Pattern,N1), NTape, Pattern, Item, NewTape). 
squish_to_pattern(SoFar, Tape, _Pattern, SoFar, Tape) :- !. % time to stop, including when there is a variable (and hence next_items fails ... 

next_items(_, [], [], [], _).
next_items(N, Tape, List, Rest, Options) :-
	Tape = [grp(_I,Number)|Rest1], ground(Number), 0 is Number, 
	next_items(N, Rest1, List, Rest, Options), !. 
next_items(N, Tape, List, Rest, _Options) :-
	Tape = [grp(I,Number)|Rest1], ground(Number), length(I,L), NTemp is Number, NTemp > 0, 
	N =< L, !, 
	append(I1, I2, I), length(I1, N), 
	List = I1,
	N1 is Number-1,
	add_item(grp(I,N1), Rest1, T1),
	add_item(grp(I2,1), T1, T2),
	Rest = T2, !. 
next_items(N, Tape, List, Rest, Options) :-
	Tape = [grp(I,Number)|Rest1], ground(Number), length(I,L), NTemp is Number, NTemp > 0, 
	N > L, !,
	NewN is N - L, N1 is Number-1,
	add_item(grp(I,N1), Rest1, T1),!, 
	next_items(NewN, T1, List2, Rest2, Options),
	append(I, List2, List), 
	Rest = Rest2.
next_items(N, Tape, List, Rest, Options) :-
    Tape = [grp(I,Number)|Rest1], nonground(Number, _), \+ member(ground, Options),
    length(I,L), N =< L, !, 
    append(I1, I2, I), length(I1, N),
    List = I1,
    N1 = Number-1,
    add_item(grp(I,N1), Rest1, T1),
    add_item(grp(I2,1), T1, T2),
    Rest = T2, !. 
next_items(N, Tape, List, Rest, Options) :-
    Tape = [grp(I,Number)|Rest1], nonground(Number, _), \+ member(ground, Options),
    length(I,L), N > L, !,
    NewN is N - L, N1 = Number-1,
    add_item(grp(I,N1), Rest1, T1),!,
    next_items(NewN, T1, List2, Rest2, Options),
    append(I, List2, List), 
    Rest = Rest2. 

%% This can be simplified ... 
padding(Difference, Original, _MinPos, _MaxPos, LDiffs, RDiffs, _Count, Original, Original, LDiffs, RDiffs) :- RealDifference #= Difference, ground(RealDifference), 0 is RealDifference. %format("No Padding~n", []). 
padding(Difference, Original, MinPos, MaxPos, LDiffs, RDiffs, Count, PaddedOriginal, FinalPaddedOriginal, NewLDiffs, NewRDiffs) :-
    RealDifference #= Difference, RealDifference #> 0,
    Original = config(Left, State, Item, Dir, Right),
    tape_lengths(Original, LeftLength, RightLength), 
    NLeft = 0 - MinPos - LeftLength, 
    NRight = MaxPos - RightLength,
    pad_tape_new(NLeft,  Left,  LDiffs, Count, PadLeft,  FinalPadLeft,  NewLDiffs),
    pad_tape_new(NRight, Right, RDiffs, Count, PadRight, FinalPadRight, NewRDiffs),
    PaddedOriginal = config(PadLeft, State, Item, Dir, PadRight), % writeln(PaddedOriginal), 
    FinalPaddedOriginal = config(FinalPadLeft, State, Item, Dir, FinalPadRight).
% padding(Difference, Original, MinPos, MaxPos, LDiffs, RDiffs, NewOriginal, NewLDiffs, NewRDiffs) :-
%     Difference #< 0,
%     ... cross that bridge when we come to it

tape_lengths(Config, LeftLength, RightLength) :-
    Config = config(Left, _State, Item, Dir, Right),
    tape_size(Left, LeftSize),
    tape_size(Right, RightSize),
    tape_size([Item], ItemSize),
    config_sum(LeftSize, ItemSize, RightSize, Dir, LeftLength, RightLength).

config_sum(LeftSize, ItemSize, RightSize, _, LeftSize, RightSize) :-   ground(ItemSize), 1 is ItemSize. 
config_sum(LeftSize, ItemSize, RightSize, l, LeftSize, RightLength) :- ground(ItemSize), \+ (1 is ItemSize), RightLength = ItemSize - 1 + RightSize. 
config_sum(LeftSize, ItemSize, RightSize, l, LeftSize, RightLength) :- \+ ground(ItemSize), RightLength = ItemSize + RightSize. 
config_sum(LeftSize, ItemSize, RightSize, r, LeftLength, RightSize) :- ground(ItemSize), \+ (1 is ItemSize), LeftLength = ItemSize - 1 + LeftSize. 
config_sum(LeftSize, ItemSize, RightSize, r, LeftLength, RightSize) :- \+ ground(ItemSize), LeftLength = ItemSize + LeftSize. 

pad_tape_new(N, Tape, Diffs, Count, Pad, FinalPad, NewDiffs) :-
    RealN #= N, ground(RealN), RealN > 0, 
    inputs(RealN, 0, List), 
    append(Tape, [grp(List,1)], Pad),
    append(Tape, [grp(List,Count)], FinalPad), 
    append(Diffs, [m(1,-1)], NewDiffs).
pad_tape_new(N, Tape, Diffs, _Count, Tape, Tape, Diffs) :-
    RealN #= N, (N #=< 0; nonground(RealN,_)), !. 

pad_tape(left, Left, LeftLength, MinPos, LDiffs, Count, PadLeft, FinalPadLeft, NewLDiffs) :-
    % add 0 - MinPos - LeftLength blanks to Left to get PadLeft and FinalPadLeft and update NewLDiffs appropriately
    N #= 0 - MinPos - LeftLength, N #> 0, ground(N),
    NValue is N, inputs(NValue, 0, List), 
    append(Left, [grp(List,1)], PadLeft),
    append(Left, [grp(List,Count)], FinalPadLeft), 
    append(LDiffs, [m(1,-1)], NewLDiffs).
pad_tape(left, Left, LeftLength, MinPos, LDiffs, _Count, Left, Left, LDiffs) :-
    N #= 0 - MinPos - LeftLength, (N #=< 0; nonground(N,_)), !. 
pad_tape(right, Right, RightLength, MaxPos, RDiffs, Count, PadRight, FinalPadRight, NewRDiffs) :-
    % add MaxPos - RightLength blanks to Right to get PadRight and FinalPadRight and update NewRDiffs appropriately
    N #= MaxPos - RightLength, N #> 0, ground(N),
    NValue is N, inputs(NValue, 0, List), 
    append(Right, [grp(List,1)], PadRight),
    append(Right, [grp(List,Count)], FinalPadRight),
    append(RDiffs, [m(1,-1)], NewRDiffs).
pad_tape(right, Right, RightLength, MaxPos, RDiffs, _Count, Right, Right, RDiffs) :-
    N #= MaxPos - RightLength, (N #=< 0; nonground(N,_)), !.

inputs(0, _, []).
inputs(N, I, [I|Rest]) :- N > 0, N1 is N-1, inputs(N1, I, Rest). 

regressor_list(LDiffs, ItemDiff, RDiffs, Regressors) :- append(LDiffs, [ItemDiff], Temp), append(Temp, RDiffs, Diffs), find_regressors(Diffs, Regressors).

find_regressors([], []).
find_regressors([Diff|Diffs], [m(N,K)|Result]) :-  Diff = m(N, K), K < 0, find_regressors(Diffs, Result). 
find_regressors([Diff|Diffs], Result) :-      Diff = m(_N, K), K >= 0, find_regressors(Diffs, Result). 

regressor_values([], []).
regressor_values([m(_N,K)|Rest], [Value|Result]) :- Value is 0-K, regressor_values(Rest, Result). 

progressor_list(LDiffs, ItemDiff, RDiffs, Progressors) :- append(LDiffs, [ItemDiff], Temp), append(Temp, RDiffs, Diffs), find_progressors(Diffs, Progressors).

find_progressors([], []).
find_progressors([Diff|Diffs], [m(N,K)|Result]) :- Diff = m(N, K), K > 0, find_progressors(Diffs, Result).
find_progressors([Diff|Diffs], [m(N,K)|Result]) :- Diff = m(N, K), N > 1, K == 0, find_progressors(Diffs, Result). % Weird case! 
find_progressors([Diff|Diffs], Result) :-     Diff = m(N, K), N == 1, K =< 0, find_progressors(Diffs, Result). 

find_mins(Shape_Computation, Transitions, Names, P, Regressors, machine(_Raw, Extended, _Otter), Xmins, Ymins) :- 
    regressor_values(Regressors, Ymins0),
    make_list(P, 0, Xmins0),
    find_mins1(Shape_Computation, Transitions, Names, Extended, Xmins0, Ymins0, Xmins, Ymins).
    
find_mins1(SC, _, _Names,  _Extended, XminsSoFar, YminsSoFar, XminsSoFar, YminsSoFar) :- length(SC, L), L =< 1,!. % Don't need to check the target as no transition is applied to it. 
find_mins1([Step|Rest], [_Trans|Transitions], Names, Extended, XminsSoFar, YminsSoFar, Xmins, Ymins) :-
    % Only need to check cases where there is a variable and either a normal or otter transition. 
    Step = s(_S, Config, _, _Used),
    Config = config(_, _State, Item, _InDir, __), Item = grp(_I,N), \+ variable(N), !, % So nothing to check here.
    find_mins1(Rest, Transitions, Names, Extended, XminsSoFar, YminsSoFar, Xmins, Ymins).
find_mins1([Step|Rest], [Trans|Transitions], Names, Extended, XminsSoFar, YminsSoFar, Xmins, Ymins) :-
    % Only need to check cases where there is a variable and either a normal or otter transition. 
    Step = s(_S, _Config, _, _Used),
    member(Trans, [expansion, complex(acceleration), init, naive, cycle]), !, % Overkill apart from the first two ... 
    find_mins1(Rest, Transitions, Names, Extended, XminsSoFar, YminsSoFar, Xmins, Ymins).
find_mins1([Step|Rest], [Trans|Transitions], Names, Extended, XminsSoFar, YminsSoFar, Xmins, Ymins) :-
    % Only need to check cases where there is a variable and either a normal or otter transition. 
    Step = s(_S, Config, _, _Used),
    member(Trans, [complex(normal), otter, ocelot, ocelot(_OcelotNum), otter(_OtterNum)]), 
    Config = config(_, _State, Item, _InDir, __), Item = grp(_I,N), variable(N), % So need to check here.
    check_mins(N, Names, XminsSoFar, YminsSoFar, NewXminsSoFar, NewYminsSoFar), 
    find_mins1(Rest, Transitions, Names, Extended, NewXminsSoFar, NewYminsSoFar, Xmins, Ymins).

check_mins(N, Names, XminsSoFar, YminsSoFar, NewXminsSoFar, NewYminsSoFar) :-
    term_variables(N, NVars),  
    term_string(NVars, NString, [variable_names(Names)]), string_compose("[", String, "]", NString), % To work out which variables are involved
    copy_term(N, NNew, _), term_variables(NNew, Variables), constrain_expression(NNew, 1), % Need N to be at least 1 to apply transition ... but this needs to be the expression, not just the variables in it ... 
    minimal_value(String, Variables, XminsSoFar, YminsSoFar, NewXminsSoFar, NewYminsSoFar). 

minimal_value("", _, XminsSoFar, YminsSoFar, XminsSoFar, YminsSoFar). 
minimal_value(String, [V|Variables], XminsSoFar, YminsSoFar, Xmins, Ymins) :-
    variable_name(Name), next_var_name(Name, String, RestString), 
    check_minimum(Name, V, XminsSoFar, YminsSoFar, NewXminsSoFar, NewYminsSoFar),
    minimal_value(RestString, Variables, NewXminsSoFar, NewYminsSoFar, Xmins, Ymins). 

check_minimum("X1", V, XminsSoFar, YminsSoFar, NewXminsSoFar, YminsSoFar) :- XminsSoFar = [X1Min|Rest], check_minimum_var(V, X1Min, NewX1Min), NewXminsSoFar = [NewX1Min|Rest].
check_minimum("X2", V, XminsSoFar, YminsSoFar, NewXminsSoFar, YminsSoFar) :- append(T1, Rest, XminsSoFar), length(T1, 1), Rest = [X2Min|Rest2], check_minimum_var(V, X2Min, NewX2Min), append(T1, [NewX2Min|Rest2], NewXminsSoFar).
check_minimum("X3", V, XminsSoFar, YminsSoFar, NewXminsSoFar, YminsSoFar) :- append(T1, Rest, XminsSoFar), length(T1, 2), Rest = [X3Min|Rest2], check_minimum_var(V, X3Min, NewX3Min), append(T1, [NewX3Min|Rest2], NewXminsSoFar).
check_minimum("Y1", V, XminsSoFar, YminsSoFar, XminsSoFar, NewYminsSoFar) :- YminsSoFar = [Y1Min|Rest], check_minimum_var(V, Y1Min, NewY1Min), NewYminsSoFar = [NewY1Min|Rest].
check_minimum("Y2", V, XminsSoFar, YminsSoFar, XminsSoFar, NewYminsSoFar) :- append(T1, Rest, YminsSoFar), length(T1, 1), Rest = [Y2Min|Rest2], check_minimum_var(V, Y2Min, NewY2Min), append(T1, [NewY2Min|Rest2], NewYminsSoFar).
check_minimum("Y3", V, XminsSoFar, YminsSoFar, XminsSoFar, NewYminsSoFar) :- append(T1, Rest, YminsSoFar), length(T1, 2), Rest = [Y3Min|Rest2], check_minimum_var(V, Y3Min, NewY3Min),  append(T1, [NewY3Min|Rest2], NewYminsSoFar).

check_minimum_var(V, MinSoFar, NewMin) :- fd_dom(V, Dom) , Dom = Min.._, Min \== inf, NewMin is max(MinSoFar, Min). 
check_minimum_var(V, MinSoFar, NewMin) :- fd_dom(V, Dom) , Dom = Min.._, Min == inf,  NewMin is MinSoFar. 

% variable_name("X").
variable_name("X1").  % Hacky but should do for now. 
variable_name("X2").
variable_name("X3").
% variable_name("Y").
variable_name("Y1").
variable_name("Y2").
variable_name("Y3").

next_var_name(Name, String, RestString) :- string_compose(Name, ",", RestString, String), !. 
next_var_name(Name, String, RestString) :- Name = String, RestString = "". % Last string only 

classify_transition(trans(_State, _I, l, _NewState, _Out, l, _Steps), normal).
classify_transition(trans(_State, _I, r, _NewState, _Out, r, _Steps), normal).
classify_transition(trans(State, _I, l, NewState, _Out, r, _Steps), normal) :- State \= NewState.
classify_transition(trans(State, _I, r, NewState, _Out, l, _Steps), normal) :- State \= NewState. 
classify_transition(trans(State, _I, l, State, _Out, r, _Steps), acceleration).
classify_transition(trans(State, _I, r, State, _Out, l, _Steps), acceleration).

dynamic_config(PaddedOriginal, FinalPaddedOriginal, LDiffs, RDiffs, PaddedDynamic, FinalPaddedDynamic, NewLDiffs, NewRDiffs) :-
    PaddedOriginal = config(Left, State, Item, InDir, Right),
    FinalPaddedOriginal = config(FinalLeft, State, Item, InDir, FinalRight),
    dynamic_tape(Left, LDiffs, DynamicLeft, NewLDiffs),
    dynamic_tape(FinalLeft, LDiffs, DynamicFinalLeft, NewLDiffs),
    dynamic_tape(Right, RDiffs, DynamicRight, NewRDiffs),
    dynamic_tape(FinalRight, RDiffs, DynamicFinalRight, NewRDiffs),
    PaddedDynamic = config(DynamicLeft, State, Item, InDir, DynamicRight),
    FinalPaddedDynamic = config(DynamicFinalLeft, State, Item, InDir, DynamicFinalRight).

dynamic_tape(Tape, Diffs, DTape, DDiffs) :-
    reverse(Tape, RevTape),
    reverse(Diffs, RevDiffs),
    dynamic_tape1(RevTape, RevDiffs, RevDTape, RevDDiffs),
    reverse(RevDTape, DTape),
    reverse(RevDDiffs, DDiffs).

dynamic_tape1([], [], [], []).
dynamic_tape1([Item|IRest], [Diff|DRest], [Item|IRest], [Diff|DRest]) :-
    Diff = m(_, N), N \== 0. % dynamic, so stop. 
dynamic_tape1([_Item|IRest], [Diff|DRest], TapeResult, DiffsResult) :-
    Diff = m(_, 0), % static, so keep looking for dynamic part
    dynamic_tape1(IRest, DRest, TapeResult, DiffsResult). 

final_configuration(Original, FinalOriginal, Count, LDiffs, ItemDiff, RDiffs, MinEffect, MaxEffect, Final) :-
    tape_lengths(Original, LeftPadLength, RightPadLength), % writeln(Original), writeln([LeftPadLength, RightPadLength, MinEffect, MaxEffect]), 
    RealLeft #= LeftPadLength,
    RealRight #= RightPadLength,
    LeftInterval #= 0 - MinEffect,
    RightInterval #= MaxEffect,
    RealLeft #>= LeftInterval,
    RealRight #>= RightInterval, !, % check what this does to X and Y values
    apply_k(FinalOriginal, LDiffs, ItemDiff, RDiffs, Count, Final).
final_configuration(Original, FinalOriginal, Count, LDiffs, ItemDiff, RDiffs, MinEffect, MaxEffect, Final) :-
    tape_lengths(Original, LeftPadLength, RightPadLength), 
    RealLeft #= LeftPadLength,
    RealRight #= RightPadLength,
    LeftInterval #= 0 - MinEffect,
    RightInterval #= MaxEffect,
    \+ (RealLeft #>= LeftInterval, RealRight #>= RightInterval) , % check what this does to X and Y values
    % dynamic subset does not include the visited interval, so add static cases until it does .....
    % ... cross that bridge when we come to it ... 
    format("% ABNORMAL case for final instance: ", []), writeln([Original, LeftPadLength, RightPadLength, MinEffect, MaxEffect]), 
    apply_k(FinalOriginal, LDiffs, ItemDiff, RDiffs, Count, Final).

apply_k(Config, LDiffs, ItemDiff, RDiffs, K, General_Instance) :-
    Config = config(Left, State, Item, InDir, Right),
    apply_k_tape(Left, LDiffs, K, GILeft),
    apply_k_tape([Item], [ItemDiff], K, [GIItem]), 
    apply_k_tape(Right, RDiffs, K, GIRight),
    General_Instance = config(GILeft, State, GIItem, InDir, GIRight).

apply_k_tape([], [], _, []).
apply_k_tape([Item|IRest], [Diff|DRest], K, [GI_Item|GIRest]) :-
    Item = grp(I,Num),
    apply_diff(Diff, Num, K, NewNum),
    GI_Item = grp(I, NewNum),
    apply_k_tape(IRest, DRest, K, GIRest). 

apply_diff(m(M,N), Num, _K, Num) :- M == 1, N == 0.
apply_diff(m(M,N), Num, K, NewNum) :- M == 1, N is 1, NewNum = Num + K. % simplify_exp2(Num + K, NewNum).  % 
apply_diff(m(M,N), Num, K, NewNum) :- M == 1, N > 1, NewNum = Num + N*K. % simplify_exp2(Num + N*L, NewNum). % 
apply_diff(m(M,N), Num, K, NewNum) :- M == 1, -1 is N, simplify_exp2(Num - K, NewNum). % NewNum = Num - K. % perhaps this is overkill ...
apply_diff(m(M,N), Num, K, NewNum) :- M == 1, N < -1, NewN is 0-N, NewNum = Num - NewN*K. % simplify_exp2(Num - NewN*K, NewNum). % . % perhaps this is overkill ...
apply_diff(m(M,N), Num, K, NewNum) :- M > 1, N \== 0, NewNum = (M**K)*Num + N*(M**K - 1)//(M - 1). 

% set_count(RegMins, RegVars, Regressors, CountSoFar, Count)
% RegMins is the minimum for each regressor, RegVars is the regressive variables, Regressors is the list of regressors
% Returns minimum formula for Count, which will need to be evaluated once the transition is applied. 

set_count([], _, _, CountsSoFar, Count) :- apply_mins(CountsSoFar, Count). 
set_count([Ymin|RestMins], [Y|RestVars], [m(_N,K)|RestRegs], CountsSoFar, Count) :-
    Reg is 0-K, 
    set_count1(Ymin, Y, Reg, CountNext),
    append(CountsSoFar, [CountNext], NewCountsSoFar), 
    set_count(RestMins, RestVars, RestRegs, NewCountsSoFar, Count). 

set_count1(Ymin, Y, Reg, Count) :- 0 is Reg - Ymin, Reg == 1, Count = Y. 
set_count1(Ymin, Y, Reg, Count) :- 0 is Reg - Ymin, Reg > 1, Count = Y//Reg. 
set_count1(Ymin, Y, Reg, Count) :- Ymin \== Reg, Reg == 1, simplify_exp2(Y + 1 - Ymin, Count). % Count = Y + 1 - Ymin.
set_count1(Ymin, Y, Reg, Count) :- Ymin \== Reg, Reg > 1, simplify_exp2(Y + Reg - Ymin, Count1), Count = (Count1)//Reg.

apply_mins(List, Count) :- length(List, 1), List = [Count]. % Only one element, so no minimum required.
apply_mins(List, Count) :- length(List, 2), List = [Count1,Count2], Count = min(Count1, Count2). % Exactly two elements.
apply_mins(List, Count) :- length(List, L), L > 2, List = [Count1|Rest], apply_mins(Rest, Count2), Count = min(Count1, Count2). % More than two elements, so use nested mins.

otter_steps(Hops, HopsString, ExprVars, Progressors, Regressors, ProgVars, RegVars, Count, OtterSteps) :-
    length(Progressors, P), length(Regressors, R), 
    set_ab(Hops, HopsString, ExprVars, P, R, As, B),
    set_otter_steps(As, B, Progressors, Regressors, ProgVars, RegVars, Count, OtterSteps). %ATTENTION!

set_ab(Hops, _, _, P, R, As, B) :- ground(Hops), B is Hops, L is P+R, make_list(L, 0, As). %% need to adjust for appropriate number of A's here ... 
set_ab(Hops, HopsString, ExprVars, P, R, As, B) :-
    \+ ground(Hops), % Work it out from the string (!!)
    % ExprVars is a list of strings in the expression in order, so this tells us which is A, B etc.
    % Do we need this if we have already converted the string to be in terms of the progressive variables? Yes in case we have something in terms of say X2 and X3 but not X1 
    extract_abs(HopsString, ExprVars, [], AsTemp, B), 
    pad_as(P, R, ExprVars, AsTemp, As). 

extract_abs(HopsString, [], AsSoFar, AsSoFar, B) :- find_b(HopsString, B). 
extract_abs(HopsString, [ThisVar|RestVars], AsSoFar, As, B) :-
     string_compose(CoeffString, ThisVar, _RestofString, HopsString), 
     coefficient(CoeffString, ThisA), 
     append(AsSoFar, [ThisA], NewAsSoFar),
     extract_abs(HopsString, RestVars, NewAsSoFar, As, B).

coefficient(String, A) :- convert_number(String, A). % First term
coefficient(String, A) :-
    string_compose(_, "+", Coeff, String), convert_number(Coeff, A). % Subsequent term

% This seems fiddly! Surely there is a simpler way!
pad_as(P, R, Vars, As, Result) :-
    divide_vars(Vars, PVars, RVars), length(PVars, PL), length(RVars, RL), 
    append(PAs, RAs, As), length(PAs, PL), length(RAs, RL),
    pad_prog_as(PAs, PVars, P, NewPAs),
    pad_reg_as(RAs, RVars, R, NewRAs),
    append(NewPAs, NewRAs, Result). 

pad_prog_as(As, Vars, Num, As) :- length(Vars, Num).
pad_prog_as(As, Vars, Num, Result) :-
    length(Vars, L), L < Num,
    prog_vars(Num, _, _, PVars),
    pad_as1(PVars, Vars, As, Result). 
pad_reg_as(As, Vars, Num, As) :- length(Vars, Num).
pad_reg_as(As, Vars, Num, Result) :-
    length(Vars, L), L < Num,
    reg_vars(Num, _, _, RVars),
    pad_as1(RVars, Vars, As, Result). 

pad_as1([], _, _, []). 
pad_as1([V|Vars], [ThisVar|Rest], As, [0|Result]) :-
    V \== ThisVar, % So this A value must be 0
    pad_as1(Vars, [ThisVar|Rest], As, Result). 
pad_as1([_V|Vars], [], As, [0|Result]) :-
    % No more variables, so this A value must be 0
    pad_as1(Vars, [], As, Result). 
pad_as1([V|Vars], [ThisVar|Rest], [ThisA|RestAs], [ThisA|Result]) :-
    V == ThisVar, % So use this A value 
    pad_as1(Vars, Rest, RestAs, Result). 

divide_vars([], [], []).
divide_vars([V|Vars], [V|PVars], RVars) :-  prog_var(V), divide_vars(Vars, PVars, RVars). 
divide_vars([V|Vars], PVars, [V|RVars]) :-  reg_var(V),  divide_vars(Vars, PVars, RVars). 

prog_var(V) :- prog_vars(_, _,_, L), member(V, L),!.
reg_var(V) :- reg_vars(_, _,_, L), member(V, L),!. 

convert_number(String, Number) :-
    % delete trailing "*"
    string_concat(RealString, "*", String), convert_number1(RealString, Number).
convert_number(String, Number) :-
    % if none, just process what is there. 
    \+ string_concat(_RealString, "*", String), convert_number1(String, Number).

convert_number1(String, Number) :- String \== "", number_string(Number, String).
convert_number1("", 1). 

find_b(String, B) :- string_compose(_, "+", BString, String), BString \== "", convert_number(BString, B). 
find_b("", 0). % Should never have + and then nothing ... 

% When all A's are 0, OtterSteps is Count * B.
% Otherwise we find the progressor term P, which is (a1xn1 + a2xn2 + a3xn3)//2, and the variable term V, which his a1X1+a2X2+a3X3 and OtterSteps is then Count * (V + P*Count + B - P)
set_otter_steps(As, B, _Progressors, _Regressors, _ProgVars, _RegVars, Count, OtterSteps) :-
     all_zeroes(As), OtterSteps = B * Count.
set_otter_steps(As, B, Progressors, Regressors, ProgVars, RegVars, Count, OtterSteps) :-
     \+ all_zeroes(As),
     sum_term(As, Progressors, Regressors, 0, S), 
     variable_term(As, ProgVars, RegVars, 0, V),
     T is B - S, 
     OtterSteps = Count * (V + S*Count + T).

sum_term([], _, _, SoFar, Result) :- Result is SoFar//2. 
sum_term([0|RestAs], [_|RestProgressors], Regressors, SoFar, Result) :-
     sum_term(RestAs, RestProgressors, Regressors, SoFar, Result). 
sum_term([A|RestAs], [Progressor|RestProgressors], Regressors, SoFar, Result) :-
     A \== 0, Progressor = m(N,K), N == 1, % Not sure what happens if N > 1 !!
     NewSoFar is A*K + SoFar, 
     sum_term(RestAs, RestProgressors, Regressors, NewSoFar, Result). 
sum_term([0|RestAs], [], [_Regressor|RestRegressors], SoFar, Result) :-
     sum_term(RestAs, [], RestRegressors, SoFar, Result). 
sum_term([A|RestAs], [], [Regressor|RestRegressors], SoFar, Result) :-
     A \== 0, Regressor = m(N,K), N == 1, % Not sure what happens if N > 1 !!
     NewSoFar is A*K + SoFar, 
     sum_term(RestAs, [], RestRegressors, NewSoFar, Result). 

variable_term([], _, _, SoFar, SoFar).
variable_term([0|RestAs], [_|RestProgVars], RegVars, SoFar, Result) :-
     variable_term(RestAs, RestProgVars, RegVars, SoFar, Result). 
variable_term([A|RestAs], [ProgVar|RestProgVars], RegVars, SoFar, Result) :-
     A == 1, SoFar == 0, 
     NewSoFar = ProgVar, 
     variable_term(RestAs, RestProgVars, RegVars, NewSoFar, Result). 
variable_term([A|RestAs], [ProgVar|RestProgVars], RegVars, SoFar, Result) :-
     A == 1, SoFar \== 0, 
     NewSoFar = SoFar + ProgVar, 
     variable_term(RestAs, RestProgVars, RegVars, NewSoFar, Result). 
variable_term([A|RestAs], [ProgVar|RestProgVars], RegVars, SoFar, Result) :-
     A > 1, SoFar == 0, 
     NewSoFar = A*ProgVar, 
     variable_term(RestAs, RestProgVars, RegVars, NewSoFar, Result). 
variable_term([A|RestAs], [ProgVar|RestProgVars], RegVars, SoFar, Result) :-
     A > 1, SoFar \== 0, 
     NewSoFar = SoFar + A*ProgVar, 
     variable_term(RestAs, RestProgVars, RegVars, NewSoFar, Result). 
variable_term([0|RestAs], [], [_|RestRegVars], SoFar, Result) :-
     variable_term(RestAs, [], RestRegVars, SoFar, Result). 
variable_term([A|RestAs],  [], [RegVar|RestRegVars], SoFar, Result) :-
     A == 1, SoFar == 0, 
     NewSoFar = RegVar, 
     variable_term(RestAs, [], RestRegVars, NewSoFar, Result). 
variable_term([A|RestAs],  [], [RegVar|RestRegVars], SoFar, Result) :-
     A == 1, SoFar \== 0, 
     NewSoFar = SoFar + RegVar, 
     variable_term(RestAs, [], RestRegVars, NewSoFar, Result). 
variable_term([A|RestAs],  [], [RegVar|RestRegVars], SoFar, Result) :-
     A > 1, SoFar == 0, 
     NewSoFar = A*RegVar, 
     variable_term(RestAs, [], RestRegVars, NewSoFar, Result). 
variable_term([A|RestAs],  [], [RegVar|RestRegVars], SoFar, Result) :- 
     A > 1, SoFar \== 0, 
     NewSoFar = SoFar + A*RegVar, 
     variable_term(RestAs, [], RestRegVars, NewSoFar, Result). 
    
all_zeroes([]).
all_zeroes([0|Rest]) :- all_zeroes(Rest). 

% Assumes hops constraint string only contains progressive variables, ie X, X1, X2, X3, etc. 
identify_hops_constraint("[]", "").
identify_hops_constraint(ConstraintString, RealConstraint) :-
    ConstraintString \== "[]",
    string_compose("[", Temp1, "]", ConstraintString), % get rid of leading "[" and trailing "]"
    split_string(Temp1, ",", ",", StringList),
    member(String, StringList),
    string_compose("clpfd:(", Temp2, ")", String),
    hops_constraint_string(Temp2),
    RealConstraint = Temp2, 
    true.


hops_expression("[]", _, "").
hops_expression(ConstraintString, ExprVars, Expr) :-
    ConstraintString \== "[]",
    string_compose("[", Temp1, "]", ConstraintString), % get rid of leading "[" and trailing "]"
    split_string(Temp1, ",", ",", StringList),
    member(String, StringList),
    string_compose("clpfd:(", Temp2, ")", String),
    string_concat("RealHops#=", Expr0, Temp2), vars_in_string(Expr0, Vars), 
    mapping_list(ExprVars, Vars, ToList), % Hacky! May need more! Otter found in monster 12 at 113 146 207 gives "RealHops#=4*C+17" for "X1", which crashes convert_value ... 
    convert_value(Expr0, ToList, Expr), 
    true.

vars_in_string(S, Vars) :- string_chars(S, List),  process_vars(List, Vars).

process_vars([], []).
process_vars([X|Rest], [S|Vars]) :-
    member(X, ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']),
    string_chars(S, [X]), 
    process_vars(Rest, Vars). 
process_vars([X|Rest], Vars) :-
    \+ member(X, ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']),
    process_vars(Rest, Vars). 

mapping_list([], _, []).
mapping_list([V1|Rest], [Next|VarRest], [to(Next,V1)|Result]) :- mapping_list(Rest, VarRest, Result). 

% For hacky reasons, we want to identify the string "X3#2*A+10" or similar (ie 2 and 10 could be other positive integers
hops_constraint_string(String) :- string_concat("X3#=", _Term, String).
hops_constraint_string(String) :- string_compose(_, "#=", _Term, String). 

hops_value(Expr, _ConstraintString, _Conversion, Value) :- ground(Expr), Value is Expr.
hops_value(Expr, ConstraintString, Conversion, Value) :- nonground(Expr,_), convert_value(ConstraintString, Conversion, Value).

convert_value(ConstraintString, [], ConstraintString). 
convert_value(ConstraintString, [to(From, To)|Conversion], Value) :-
    convert_value1(ConstraintString, to(From, To), NewString),
    convert_value(NewString, Conversion, Value).

convert_value1(String, to(From, To), NewString) :-
    string_compose(S1, From, S2, String),
    string_compose(S1, To, S2, NewString). 

replace_char([], _, _, []).
replace_char([X|Rest], From, To,  [X|NewRest]) :- X \== From, replace_char(Rest, From, To, NewRest).
replace_char([X|Rest], From, To, [To|NewRest]) :- X == From, replace_char(Rest, From, To, NewRest).

output_value(X) :- integer(X), format("~d ", [X]).
output_value(X) :- \+ integer(X), format("~s ", [X]).

configuration_size(Config, LeftSize, ItemSize, RightSize) :- Config = config(Left, _, Item, _Dir, Right), tape_size(Left, LeftSize), tape_size(Right, RightSize), tape_size([Item], ItemSize).

tape_size([], 0).
tape_size([Item|Rest], Size) :- item_size(Item, ItemSize), tape_size(Rest, RestSize), Size = ItemSize + RestSize. 

item_size(I, 1) :- is_input(I).
item_size(grp(I,_N), 0) :- length(I, 0).
item_size(grp(I,N), 0) :- length(I, L), L > 0, ground(N), 0 is N.
item_size(grp(I,N), Size) :- length(I, L), L > 0, ground(N), N1 is N, N1 > 0, Size is L*N1. 
item_size(grp(I,N), Size) :- nonground(N,_), length(I, 1), Size = N.
item_size(grp(I,N), Size) :- nonground(N,_), length(I, L), L > 1, Size = L * N. 

add_sizes(LeftSize1, RightSize1, ItemSize, l, LeftSize, RightSize, TotalSize) :- LeftSize = LeftSize1, RightSize = RightSize1 + ItemSize, TotalSize = LeftSize1 + ItemSize + RightSize1.
add_sizes(LeftSize1, RightSize1, ItemSize, r, LeftSize, RightSize, TotalSize) :- LeftSize = LeftSize1 + ItemSize, RightSize = RightSize1, TotalSize = LeftSize1 + ItemSize + RightSize1.

constrain_expression(Expression, Min) :- Expression #>= Min. 

% Ensure all variables have the constraint >= Min ...
constrain_variables(Term, _Min) :- ground(Term). 
constrain_variables(Term, Min) :- \+ground(Term), term_variables(Term, Vars), all_vars(Vars, Min). % Hackers-R-Us ... 

all_vars([], _).
all_vars([X|Rest], Min) :- X #>= Min, all_vars(Rest, Min). % Only when necessary!

find_min_pos([], _Names, MinSoFar, MinSoFar, MinEffectSoFar, MinEffectSoFar).
find_min_pos([s(_, _Config, NewPos, _)|Rest], Names, MinSoFar, MinPos, MinEffectSoFar, MinEffect) :-
    minimum(MinSoFar, NewPos, NewMinSoFar),
    minimum(MinEffectSoFar, NewPos, NewMinEffectSoFar),
    find_min_pos(Rest, Names, NewMinSoFar, MinPos, NewMinEffectSoFar, MinEffect).

find_max_pos([], _Names, MaxSoFar, MaxSoFar, MaxEffectSoFar, MaxEffectSoFar).
find_max_pos([s(_, Config, NewPos, _)|Rest], Names, MaxSoFar, MaxPos, MaxEffectSoFar, MaxEffect) :-
    Config = config(_Left, _State, Item, _Dir, _Right), Item = grp(I,_N), length(I, L),
    set_new_max(NewPos, L, Max),
    maximum(MaxSoFar, NewPos, NewMaxSoFar),
    maximum(MaxEffectSoFar, Max, NewMaxEffectSoFar),
    find_max_pos(Rest, Names, NewMaxSoFar, MaxPos, NewMaxEffectSoFar, MaxEffect).

minimum(X, Y,Min) :- X #> Y, !, Min = Y. % Assumes minimum so far is X.
minimum(X,_Y,Min) :- Min = X, !. 

maximum(X, Y,Max) :- Y #> X, !, Max = Y. % Assumes maximum so far is X.
maximum(X,_Y,Max) :- Max = X, !. 

neg_position(Pos) :- ground(Pos), P is Pos, P < 0.
neg_position(Pos) :- \+ ground(Pos), termify(Pos, Terms), classify_terms(Terms, Simple, _Complex, _Ground), negatives(Simple). % Don't know how to do complex!

pos_position(Pos) :- ground(Pos), P is Pos, P > 0.
pos_position(Pos) :- \+ ground(Pos), termify(Pos, Terms), classify_terms(Terms, Simple, _Complex, _Ground), positives(Simple). % Don't know how to do complex!

negatives([]).
negatives([T|Rest]) :- term_val(T, V), V < 0, negatives(Rest). 

positives([]).
positives([T|Rest]) :- term_val(T, V), V > 0, positives(Rest). 

% Can ignore configurations when Pos is 0. 
separate_shape_computation([], NegSoFar, NegSoFar, PosSoFar, PosSoFar). 
separate_shape_computation([Step|Rest], NegSoFar, NegSC, PosSoFar, PosSC) :-
    Step = s(_, _, Pos, _), neg_position(Pos),!, 
    append(NegSoFar, [Step], NewNegSoFar),
    separate_shape_computation(Rest, NewNegSoFar, NegSC, PosSoFar, PosSC). 
separate_shape_computation([Step|Rest], NegSoFar, NegSC, PosSoFar, PosSC) :-
    Step = s(_, _, Pos, _), pos_position(Pos),!, 
    append(PosSoFar, [Step], NewPosSoFar),
    separate_shape_computation(Rest, NegSoFar, NegSC, NewPosSoFar, PosSC).
separate_shape_computation([_Step|Rest], NegSoFar, NegSC, PosSoFar, PosSC) :-
    % Steps at 0 are ignored, as are any which are "mixed". Will need to address this case if any arise. 
    separate_shape_computation(Rest, NegSoFar, NegSC, PosSoFar, PosSC).

set_new_max(NewPos, L, NewPos) :- L = 1.
set_new_max(NewPos, L, Max) :- L > 1, MaxDiff is L - 1, simplify_exp2(NewPos + MaxDiff, Max). % Max = NewPos + MaxDiff. 

% replace with simplify_exp2? Intension is to rewrite N*(X//N) to X - (X mod N) ... 
reduce1(Expr, Simple) :- ground(Expr), simplify_ground(Expr, Simple), !. 
reduce1(Expr, Expr)   :- var(Expr), !.
reduce1(Expr, Simple) :- \+ var(Expr), copy_term_nat(Expr, Expr1), RealExpr #= Expr1, ground(RealExpr), Simple = RealExpr,!. % allows for cases like Expr = Y-Y.
reduce1(Expr, Simple) :- \+ var(Expr), Expr = Y//N - Y//N,  ground(N), Simple = 0, ! . % hacky ... % case for Y//N  - Y//N
reduce1(Expr, Simple) :- \+ var(Expr), Expr = Y-N*(Z//N), ground(N), reduce1(Z, Z1), reduce1(Y-Z1, Z2), Simple = Z2+Z1 mod N, !. % This is correct but not optimal.
reduce1(Expr, Simple) :- \+ var(Expr), Expr = N*(Y//N),   ground(N), reduce1(Y,Z), Simple = Z - (Z mod N), !. 
reduce1(Expr, Expr). % Give up otherwise ... this omits cases like  Y-2*((Y+2-3)//2) ???

simplify_transition(oo(Initial, Diffs, Final, Constraints, Steps, Moves, VarNames), Names, oo(Initial, Diffs, NewFinal, Constraints, Steps, Moves, VarNames)) :-
    simplify_config(Names, Final, Final1), remove_zeroes(Final1, NewFinal). 

simplify_config(_Names, Config, SimpleConfig) :-
    del_attrs(Config), % Not sure this does anything ... 
    Config = config(Left, State, Item, Dir, Right), 
    simplify_tape(Left, SimpleLeft), 
    simplify_tape([Item], [SimpleItem]),
    simplify_tape(Right, SimpleRight),
    SimpleConfig = config(SimpleLeft, State, SimpleItem, Dir, SimpleRight).

% Simplifies exponents in tape blocks. Could also do some other clean-up if required.
simplify_tape([], []).
simplify_tape([grp(I,N)|Rest], [grp(I,N2)|Result]) :- reduce1(N,N2), simplify_tape(Rest, Result). 
 
remove_zeroes(Config, NewConfig) :-
    Config = config(Left, State, Item, Dir, Right),
    remove_zeroes_tape(Left, NewLeft),
    remove_zeroes_tape([Item], NItem), process_item(NItem, NewItem), % Do we ever need to remove zeroes from Item? Check known otters. Yes! So need to account for the case when NewItem is []. But don't reconfigure here -- only once the otter has been applied. 
    remove_zeroes_tape(Right, NewRight),
    NewConfig = config(NewLeft, State, NewItem, Dir, NewRight).

process_item([], grp([],0)). 
process_item([NewItem], NewItem).

remove_zeroes_tape([], []).
remove_zeroes_tape([grp(_I,N)|Rest], Result) :- ground(N), 0 is N, remove_zeroes_tape(Rest, Result). % so delete it
remove_zeroes_tape([grp(I,N)|Rest], [grp(I,N)|Result]) :-  ground(N), N1 is N, N1 \== 0, remove_zeroes_tape(Rest, Result). 
remove_zeroes_tape([grp(I,N)|Rest], [grp(I,N)|Result]) :-  nonground(N, _), remove_zeroes_tape(Rest, Result). 

verify_otter(M, Number, Mode, Config, LDiffs, ItemDiff, RDiffs, Progressors, Regressors, Type, Shape_Computation, Otter_Transition) :-
    variable_bound(VarBound), % format("Bound done~n", []), 
    set_configs(Config, LDiffs, ItemDiff, RDiffs, Progressors, Regressors, InitConfig, FinalConfig, ProgVars, RegVars, ProgVarsInit, RegVarsInit, VarNames), % format("Configs done~n", []), 
    search_for_shape_computation(M, Number, otter, InitConfig, "otter", VarBound, Mode, FinalConfig, ProgVarsInit, RegVarsInit, LDiffs, ItemDiff, RDiffs, VarNames, Type, Shape_Computation, VHops, VOutputs), % format("Search done~n", []), b
    generate_transition(ProgVars, RegVars, VarNames, LDiffs, ItemDiff, RDiffs, VHops, VOutputs, Shape_Computation, Otter_Transition).

search_for_shape_computation(M, Number, otter,     InitConfig, String, Bound, Mode, FinalConfig, ProgVarsInit, RegVarsInit, LDiffs, ItemDiff, RDiffs, VarNames, simple, Shape_Computation, VHops, VOutputs) :-
    % member(Version, [lemma,otter]), 
    Attempt = 1, Options = [],              Message = "trying next",     shape_computation(Attempt, Options, Message, M, Number, InitConfig, String, Bound, Mode, FinalConfig, ProgVarsInit, RegVarsInit, LDiffs, ItemDiff, RDiffs, VarNames, _, Shape_Computation, VHops, VOutputs), !.  

search_for_shape_computation(M, Number, otter,     InitConfig, String, Bound, Mode, FinalConfig, ProgVarsInit, RegVarsInit, LDiffs, ItemDiff, RDiffs, VarNames, _, Shape_Computation, VHops, VOutputs) :-
    % member(Version, [lemma,otter]), 
    Attempt = 2, Options = [newacc],        Message = "trying next",     shape_computation(Attempt, Options, Message, M, Number, InitConfig, String, Bound, Mode, FinalConfig, ProgVarsInit, RegVarsInit, LDiffs, ItemDiff, RDiffs, VarNames, _, Shape_Computation, VHops, VOutputs), !. 

search_for_shape_computation(M, Number, otter,     InitConfig, String, Bound, Mode, FinalConfig, ProgVarsInit, RegVarsInit, LDiffs, ItemDiff, RDiffs, VarNames, _, Shape_Computation, VHops, VOutputs) :-
    % member(Version, [lemma,otter]), 
    Attempt = 3, Options = [ocelot],        Message = "trying next",     shape_computation(Attempt, Options, Message, M, Number, InitConfig, String, Bound, Mode, FinalConfig, ProgVarsInit, RegVarsInit, LDiffs, ItemDiff, RDiffs, VarNames, _, Shape_Computation, VHops, VOutputs), !. 
    
search_for_shape_computation(M, Number, otter,     InitConfig, String, Bound, Mode, FinalConfig, ProgVarsInit, RegVarsInit, LDiffs, ItemDiff, RDiffs, VarNames, _, Shape_Computation, VHops, VOutputs) :-
    % member(Version, [lemma,otter]), 
    Attempt = 4, Options = [ocelot,newacc], Message = "trying next",   shape_computation(Attempt, Options, Message, M, Number, InitConfig, String, Bound, Mode, FinalConfig, ProgVarsInit, RegVarsInit, LDiffs, ItemDiff, RDiffs, VarNames, _, Shape_Computation, VHops, VOutputs), !. 

shape_computation(Attempt, Options, Message, M, Number, InitConfig, String, Bound, Mode, FinalConfig, ProgVarsInit, RegVarsInit, LDiffs, ItemDiff, RDiffs, VarNames, _, Shape_Computation, VHops, VOutputs) :-
    append([mods([]),initials(ProgVarsInit,RegVarsInit),diffs(LDiffs,ItemDiff,RDiffs),variable_names(VarNames),trace,maxsteps(Bound),compress,history,list], Options, NewOptions), 
    format("% ~s ~d proof attempt ~k (~k): ", [String, Number, Attempt, Options]), display_config(InitConfig, VarNames), format(" -->> " , []), display_config(FinalConfig, VarNames), nl,
    ( member(ocelot, Options) -> (format("% Otter transitions are: ~n", []), M = machine(_, _, Otters), display_otters(Otters, 1) ) ; true ), 
    emulate_general(M, InitConfig, Bound, Mode, verify, [targets([FinalConfig])], NewOptions, VHops, VStatus, VOutputs),
    produce_shape_computation(VHops, VStatus, String, Number, Bound, Attempt, Options, Message, VOutputs, InitConfig, FinalConfig, VarNames, _Mods, Shape_Computation).

produce_shape_computation(Hops, Status, String, Number, _Bound, Attempt, _Options, _Message, Outputs, InitConfig, FinalConfig, Names, _, Shape_Computation) :-
    Status = target(_), format("% ~s ~d verified (attempt ~d)~n", [String,Number,Attempt]), 
    ( member(mods(Mods), Outputs) -> true; Mods = []), 
    ( check_constraints(InitConfig, FinalConfig, Names, Mods) -> true ; (format("% Constraints not satisfied; Mods are ", []), write(Mods), format(" FinalConfig is ", []), write_term(FinalConfig, [variable_names(Names)]), nl, fail) ),
    member(history(History), Outputs), member(now(Target), Outputs), member(pos(Pos), Outputs),
    member(used(Trans), Outputs), % Sneaky addition! 
    Complete_History = [s(Hops, Target, Pos, Trans)|History], % adds final step into history to get the complete shape computation
    reverse(Complete_History, Shape_Computation). % First element of Shape_Computation is the original shape, and the final element is the target shape.

generate_transition(ProgVars, RegVars, VarNames, LDiffs, ItemDiff, RDiffs, Hops, VOutputs, Shape_Computation, Otter_Transition) :-

    member(history(History), VOutputs), member(now(Target), VOutputs), member(pos(VPos), VOutputs),
    member(used(Trans), VOutputs), % Sneaky addition! 
    
    Complete_History = [s(Hops, Target, VPos, Trans)|History], % adds final step into history to get the complete shape computation
    reverse(Complete_History, Shape_Computation), % First element of Shape_Computation is the original shape, and the final element is the target shape.

    Shape_Computation = [s(_, Original, _, _)|_],
    constrain_variables(f(ProgVars, RegVars), 0), % Make sure all variables and Y are \geq 0 ... hack-a-saurus! Don't think this is needed anymore ... 
    member(variable_names(ShapeNames), VOutputs),

    Names1 = ShapeNames, % Why anything else? 

    separate_shape_computation(Shape_Computation, [], NegSC, [], PosSC),
    find_min_pos(NegSC, Names1, 0, MinPos, 0, MinEffect), 
    find_max_pos(PosSC, Names1, 0, MaxPos, 0, MaxEffect), 
    
    regressor_list(LDiffs, ItemDiff, RDiffs, Regressors), % length(Regressors, R), 
    progressor_list(LDiffs, ItemDiff, RDiffs, Progressors), length(Progressors, P), 
    reverse(History, RevHistory), 
    extract_applications(RevHistory, Applied_Transitions),
    member(machine(Raw, Extended, Otter), VOutputs), 
    find_mins(Shape_Computation, Applied_Transitions, Names1, P, Regressors, machine(Raw, Extended, Otter), ProgMins, RegMins),!, 

    set_count(RegMins, RegVars, Regressors, [], Count), 

    append(Names1, ['Count' = Count], Names2),

    configuration_size(Original, LeftOriginal, ItemOriginal, RightOriginal),
    configuration_size(Target, LeftTarget, ItemTarget, RightTarget),
    Difference = (LeftTarget + ItemTarget + RightTarget) - (LeftOriginal + ItemOriginal + RightOriginal),

    padding(Difference, Original, MinPos, MaxPos, LDiffs, RDiffs, Count, PaddedOriginal, FinalPaddedOriginal, NewLDiffs, NewRDiffs),
    dynamic_config(PaddedOriginal, FinalPaddedOriginal, NewLDiffs, NewRDiffs, PaddedDynamic, FinalPaddedDynamic, NextLDiffs, NextRDiffs),
    final_configuration(PaddedDynamic, FinalPaddedDynamic, Count, NextLDiffs, ItemDiff, NextRDiffs, MinEffect, MaxEffect, Final_Config), 
    get_hops_expression(Hops, Names2, HopsExpr, ExprVars), % To keep, replacing the lines below to %XXX
    otter_steps(Hops, HopsExpr, ExprVars, Progressors, Regressors, ProgVars, RegVars, Count, OtterSteps),

    OtterMoves = VPos * Count, % Simple as that!
    
    ( member(mods(Mods), VOutputs) -> true; Mods = []),
    mod_names(Mods, ModNames), find_names(Names2, ModNames, [], SubNames), modvars(Mods, SubNames, ModsVar), % hacky stuff to convert strings in Mods to real variables to simplify checking on application  ... 
    Raw_Otter_Transition = oo(FinalPaddedDynamic, diffs(LDiffs, ItemDiff, RDiffs), Final_Config, constraints(mins(ProgVars,ProgMins,RegVars,RegMins), ModsVar), OtterSteps, OtterMoves, VarNames),%ATTENTION!
    simplify_transition(Raw_Otter_Transition, ShapeNames, Otter_Transition),%ATTENTION!
    format("% Otter transition is ", []), write_term(Otter_Transition, [numbervars(true), variable_names(ShapeNames)]), nl.

set_configs(Config, LDiffs, ItemDiff, RDiffs, _Progressors, _Regressors, InitConfig, FinalConfig, ProgVars, RegVars, ProgVarsInit, RegVarsInit, Names3) :-
    Config = config(Left, State, Item, Dir, Right), 
    initial_final(Left, LDiffs, [], [], [], ProgVars1, RegVars1, [], [], ProgVarsInit1, RegVarsInit1, Names1, InitLeft, FinalLeft), 
    initial_final([Item], [ItemDiff], ProgVars1, RegVars1, Names1, ProgVars2, RegVars2, ProgVarsInit1, RegVarsInit1, ProgVarsInit2, RegVarsInit2, Names2, [InitItem], [FinalItem]), 
    initial_final(Right, RDiffs, ProgVars2, RegVars2, Names2, ProgVars3, RegVars3, ProgVarsInit2, RegVarsInit2, ProgVarsInit3, RegVarsInit3, Names3, InitRight, FinalRight),

    ProgVars = ProgVars3,
    RegVars  = RegVars3,
    ProgVarsInit = ProgVarsInit3,
    RegVarsInit  = RegVarsInit3, 
    InitConfig = config(InitLeft, State, InitItem, Dir, InitRight),
    FinalConfig = config(FinalLeft, State, FinalItem, Dir, FinalRight).

mod_names([], "").
mod_names([mod(Var,_,_)|Rest], Result) :- mod_names(Rest, Temp), string_concat(Var, Temp, Result). 

modvars([], _, []). 
modvars([mod(Var,N,Value)|Mods], Names, [mod(VarV,N,Value)|ModVars]) :-
    term_string(VarV, Var, [variable_names(Names)]),
    modvars(Mods, Names, ModVars). 
    
prog_vars(1, [X1], ['X1'=X1], ["X1"]). % Keep it simple for now. May be a way to re-instate the default for 1 variable to be "X", but ... 
prog_vars(2, [X1,X2], ['X1'=X1,'X2'=X2], ["X1","X2"]).
prog_vars(3, [X1,X2,X3], ['X1'=X1,'X2'=X2,'X3'=X3], ["X1","X2","X3"]). % Hacky, but should do for now.

reg_vars(1, [Y1], ['Y1'=Y1], ["Y1"]). % As above, ... 
reg_vars(2, [Y1,Y2], ['Y1'=Y1,'Y2'=Y2], ["Y1","Y2"]).
reg_vars(3, [Y1,Y2,Y3], ['Y1'=Y1,'Y2'=Y2,'Y3'=Y3], ["Y1","Y2","Y3"]). % Hacky, but should do for now.

extract_applications([], []).
extract_applications([s(_,_,_,Trans)|Rest], [Trans|Result]) :- extract_applications(Rest, Result). 

get_hops_expression(Hops, _Names, HopsExpr, ExprVars) :-
    ground(Hops), % So Hops must be just constant.
    RealHops is Hops, % In case evaluation is needed. 
    number_string(RealHops, HopsExpr),
    ExprVars = [], !.

get_hops_expression(Hops, Names, HopsExpr, ExprVars) :-
    term_variables(Hops, Vars), term_string(Vars, ExprVarString, [variable_names(Names)]), % format("ExprVarString is ~s~n", [ExprVarString]), 
    convert_varstring(ExprVarString, EVars), sort(EVars, ExprVars), % make sure they are sorted appropriately, ie Progs then Regs % format("ExprVars is ", []), writeln(ExprVars), % Convert string version of list into list of strings. 
    % Will need VarString in in otter_steps to work out which subset of the progressive variables are in it, eg what if the formula is aX2 + b? So need to have a map of the variables in Hops to A, B, C etc. 
    RealHops #= Hops, 
    copy_term(RealHops, RealHops, V), numbervars(V, 0, _Last, [attvar(skip)]), term_string(V, Vstring, [variable_names(['RealHops'=RealHops]),numbervars(true)]), % format("VString is ~s~n", [Vstring]),
    hops_expression(Vstring, ExprVars, HopsExpr).

% Converts strings like "[X1,X2]" to a list of strings like ["X1","X2"]
convert_varstring(VarString, StringList) :-
    string_compose("[", Temp1, "]", VarString), % get rid of leading "[" and trailing "]"
    split_string(Temp1, ",", ",", StringList).

initial_final([], [], ProgVars, RegVars, Names, ProgVars, RegVars, ProgVarsInit, RegVarsInit, ProgVarsInit, RegVarsInit, Names, [], []).
initial_final([Item|Tape], [Diff|Diffs], ProgVars, RegVars, Names, NewProgVars, NewRegVars, ProgVarsInit1, RegVarsInit1, ProgVarsInit, RegVarsInit, NewNames, [Item|InitTape], [Item|FinalTape]) :-
    Diff = m(1,0), % static item so no variables involved
    initial_final(Tape, Diffs, ProgVars, RegVars,  Names, NewProgVars, NewRegVars, ProgVarsInit1, RegVarsInit1, ProgVarsInit, RegVarsInit, NewNames, InitTape, FinalTape).
initial_final([Item|Tape], [Diff|Diffs], ProgVars, RegVars, Names, NewProgVars, NewRegVars, ProgVarsInit1, RegVarsInit1, ProgVarsInit, RegVarsInit, NewNames, [NewInitItem|InitTape], [NewFinalItem|FinalTape]) :-
    Diff = m(1,K), K < 0, K1 is 0-K, % regressor (linear only for now)
    Item = grp(I,Num),
    append(RegVars, [Y], RegVars1), %  RegVars = [Y|RegVarsRest], 
    NewInitItem = grp(I,Y),
    NewFinalItem = grp(I,Y-K1),
    next_name(reg, Names, N1), 
    append(Names, [N1=Y], Names1), append(RegVarsInit1, [Num], RegVarsInit2), 
    initial_final(Tape, Diffs, ProgVars, RegVars1, Names1, NewProgVars, NewRegVars, ProgVarsInit1, RegVarsInit2, ProgVarsInit, RegVarsInit, NewNames, InitTape, FinalTape).
initial_final([Item|Tape], [Diff|Diffs], ProgVars, RegVars, Names, NewProgVars, NewRegVars, ProgVarsInit1, RegVarsInit1, ProgVarsInit, RegVarsInit, NewNames, [NewInitItem|InitTape], [NewFinalItem|FinalTape]) :-
    Diff = m(N,K), N == 1, K >= 1, % linear progressor (almost always will be)
    Item = grp(I,Num),
    append(ProgVars, [X], ProgVars1), % ProgVars = [X|ProgVarsRest], 
    NewInitItem = grp(I,X),
    NewFinalItem = grp(I,X+K),
    next_name(prog, Names, N1), 
    append(Names, [N1=X], Names1), append(ProgVarsInit1, [Num], ProgVarsInit2), 
    initial_final(Tape, Diffs, ProgVars1, RegVars, Names1, NewProgVars, NewRegVars, ProgVarsInit2, RegVarsInit1, ProgVarsInit, RegVarsInit, NewNames, InitTape, FinalTape).
initial_final([Item|Tape], [Diff|Diffs], ProgVars, RegVars, Names, NewProgVars, NewRegVars, ProgVarsInit1, RegVarsInit1, ProgVarsInit, RegVarsInit, NewNames, [NewInitItem|InitTape], [NewFinalItem|FinalTape]) :-
    Diff = m(N,K), N > 1, K >= 1, % non-linear progressor (almost never will be)
    Item = grp(I,Num),
    append(ProgVars, [X], ProgVars1), % ProgVars = [X|ProgVarsRest], 
    NewInitItem = grp(I,X),
    simplify_exp2(N*X+K, NewX), 
    NewFinalItem = grp(I,NewX),
    next_name(prog, Names, N1), 
    append(Names, [N1=X], Names1), append(ProgVarsInit1, [Num], ProgVarsInit2), 
    initial_final(Tape, Diffs, ProgVars1, RegVars, Names1, NewProgVars, NewRegVars, ProgVarsInit2, RegVarsInit1, ProgVarsInit, RegVarsInit, NewNames, InitTape, FinalTape).

next_name(prog, Names, 'X1') :- \+ member('X1'=_, Names). % Hacky. Can do better than this presumably ... 
next_name(prog, Names, 'X2') :- member('X1'=_, Names), \+ member('X2'=_, Names).
next_name(prog, Names, 'X3') :- member('X1'=_, Names),  member('X2'=_, Names). 
next_name(reg,  Names, 'Y1') :- \+ member('Y1'=_, Names). 
next_name(reg,  Names, 'Y2') :- member('Y1'=_, Names), \+ member('Y2'=_, Names).
next_name(reg,  Names, 'Y3') :- member('Y1'=_, Names),  member('Y2'=_, Names). 

contains_nested(Shape_Computation, Step) :- member(Step, Shape_Computation), (Step = s(_, _, _, ocelot(_N)); Step = s(_, _, _, ocelot)). 

nested_otters(Shape_Computation, Nested) :- findall(Step, contains_nested(Shape_Computation, Step), List), length(List, Nested). 

%% Need to be able to apply this when the Config is part of a shape computation, ie contains variables. 
apply_otter(_Number, Otter_Transition, Config, OldHops, Hops, Names, NewConfig, Leaps, Moves) :-

    %%%% Need to work out Moves as well as Leaps ...
    
    copy_term_nat(Otter_Transition, Transition),
    Transition = oo(Initial, _, Final, constraints(Mins, Mods), Steps, BasicMoves, _),
    Initial = config(InitialLeft, InitialState, InitialItem, InitialDir, InitialRight),
    Config = config(Left, State, Item, Dir, Right), 

    State = InitialState, dirs_match(Dir, InitialDir, Item, InitialItem, RealDir), % (Dir = InitialDir; ( Dir \= InitialDir, Item = grp(I, 1), length(I,1) ) ), % Direction doesn't matter if the item is a single character.
    
    % format("Initial is ", []), write(Initial), format(" Final is ", []), write(Final), nl, 
    
    Mins = mins(ProgVars, Xmins, RegVars, Ymins), % convert_vars(ProgVars, ProgVarsList),
    stringify(ProgVars, ProgVarsList, ProgVarsNames), 
    stringify(RegVars, RegVarsList, RegVarsNames), 

    % format("% Config is ", []), write_term(Config, [variable_names(Names)]), format(" Hops is ", []), write(OldHops), nl, 

    % format("Left is ", []), write(Left), format(" InitialLeft is ", []), write(InitialLeft), format("ProgVarsList is ", []), write_term(ProgVarsList,[]), format("Xmins is ", []), write(Xmins), nl,
    % NEED TO PASS ALL VARIABLES TO matches_pattern

    % format("Left is ", []), write(Left), format(" InitialLeft is ", []), write(InitialLeft), format(" Item is ",[]), write(Item), format(" InitialItem is ", []), write(InitialItem), format(" ProgVarsList is ", []), write_term(ProgVarsList,[]), format("Xmins is ", []), write(Xmins), nl,
    % format("Initial is ", []), write(Initial), format(" Final is ", []), write(Final), nl, 
    % format("Right is ", []), write(Right), format(" InitialRight is ", []), write(InitialRight), format("ProgVarsList is ", []), write_term(ProgVarsList,[]), format("Xmins is ", []), write(Xmins), nl, 
    
    % matches_pattern([Item], [InitialItem], ProgVarsList, Xmins, Final, ItemExtra, MinVarsItem),
    % format("Item matched with extra ", []), writeln(ItemExtra), 

    % Add Item to left or right as appropriate

    adjust_for_item(RealDir, Config, RealLeft, RealRight),
    adjust_for_item(RealDir, Initial, RealInitialLeft, RealInitialRight), 
    
    matches_pattern(RealLeft, RealInitialLeft, ProgVarsList, Xmins, Final, LeftExtra, MinVarsLeft), % format("Matched~n", []),  
    matches_pattern(RealRight, RealInitialRight, ProgVarsList, Xmins, Final, RightExtra, MinVarsRight), % format("Matched~n", []),

    append(MinVarsLeft, MinVarsRight, MinVars),
    % append(MinVarsLeft, MinVarsItem, TempVars), append(TempVars, MinVarsRight, MinVars), 
    append(ProgVarsList, RegVarsList, VarsList), append(ProgVarsNames, RegVarsNames, VarsNames), append(Xmins, Ymins, AllMins), 

    assign_minima(MinVars, VarsList, VarsNames, AllMins, Final), 
    % format("Initial is ", []), write(Initial), format(" Final1 is ", []), write(Final), nl, 
    ( unreasonable_term(Mins) -> fail; check_minima(Mins) ),  
    % format("Initial is ", []), write(Initial), format(" Final2  is ", []), write(Final), nl, 
    check_mods(Mods), % Can presumably only do this when Config is ground, ie not in a shape computation. Otherwise new variable inherits this constraint?
    % Transition now is applicable, so work out details 
    % format("Initial is ", []), write(Initial), format(" Final is ", []), write(Final), nl, 
    eval_steps(Steps, Leaps), % Steps should now be specified, but needs to be evaluated if ground.
    RealLeaps #= Leaps, RealLeaps #> 0, % Clearly no point if it gets us nowhere ... do we need to make sure this is ground to do this? 
    OldHops + RealLeaps #> Hops, % As we now apply this to the oldest otter, we also need to check that we get "far enough". This probably makes the earlier check redundant when OldHops = Hops ... do we need to make sure this is ground to do this?

    % format("Final is ", []), write(Final), nl, format("LeftExtra is ", []), write(LeftExtra), format("RightExtra is ", []), write(RightExtra), nl, 

    %%% Work out Moves here ...
    eval_steps(BasicMoves, Moves), % As for Steps. 
    
    add_extra(Final, LeftExtra, RightExtra, NFinal), % add in unchanged parts of the configuration -- may need to reconfigure, but the case below should catch it.
    % format("NFinal is ", []), write(NFinal), 
    eval_powers(NFinal, NewFinal), % Some exponents may need simplification if ground.
    % format(" NewFinal is ", []), write(NewFinal), nl, 
    reconfigure(NewFinal, NewConfig), %% Need to check for Item with 0 power and reconfigure as necessary
    % format("% NewConfig is ", []), write_term(NewConfig, [variable_names(Names)]), nl, 
    !.

dirs_match(Dir1, Dir2, _, _, RealDir) :- Dir1 == Dir2, !, RealDir = Dir1. 
dirs_match(Dir1, Dir2, Item1, Item2, RealDir) :- Dir1 \== Dir2, Item1 = grp(I1,1), length(I1,1), !, RealDir = Dir2.
dirs_match(Dir1, Dir2, Item1, Item2, RealDir) :- Dir1 \== Dir2, Item2 = grp(I2,1), length(I2,1), !, RealDir = Dir1. 

check_minima(Mins) :- Mins = mins(ProgVars, Xmins, RegVars, Ymins), check_min_list(ProgVars, Xmins), check_min_list(RegVars,  Ymins).

check_min_list([], _).
check_min_list([Var|Rest], [Min|RestMins]) :- RealVar #= Var, RealVar #>= Min, check_min_list(Rest, RestMins). 

check_mods([]).
check_mods([mod(Var,N,Value)|Rest]) :- ground(Var), Value is Var mod N, check_mods(Rest).

adjust_for_item(l, Config, Left, NewRight) :- Config = config(Left, _, Item, _, Right), append([Item], Right, NewRight).
adjust_for_item(r, Config, NewLeft, Right) :- Config = config(Left, _, Item, _, Right), append([Item], Left,  NewLeft). 

eval_powers(Config, NewConfig) :-
    Config = config(Left, State, Item, Dir, Right),
    eval_tape(Left, NewLeft),
    eval_tape([Item], [NewItem]),
    eval_tape(Right, NewRight), 
    NewConfig = config(NewLeft, State, NewItem, Dir, NewRight).

eval_tape([], []).
eval_tape([grp(I,N)|Rest], [grp(I,NewN)|NewRest]) :- simplify_exp2(N, NewN), eval_tape(Rest, NewRest).

mins(X, Xmin, Y, Ymin) :- X #>= Xmin, Y #>= Ymin. 

eval_steps(Steps, Leaps) :- ground(Steps), Leaps is Steps. % simplify_exp2??
eval_steps(Steps, Leaps) :- \+ ground(Steps), Leaps = Steps. 

convert_vars([], []).
convert_vars([V|Vars], [String|Rest]) :- term_string(V, String), convert_vars(Vars, Rest). 

% matches_pattern(Tape, Pattern, Extra)
% True if Tape matches Pattern, with Extra after the Pattern. Pattern may include padding.

matches_pattern(Tape, Pattern, ProgVars, Xmins, Final, Extra, MinVars) :-
    remove_zeroes_tape(Tape, RealTape),
    matches_pattern1(RealTape, Pattern, ProgVars, Xmins, Final, Extra, [], MinVars).

matches_pattern1(Extra, [], _, _, _, Extra, MinVars, MinVars). % Once the pattern is empty, any excess tape will be unchanged.
matches_pattern1([Item|Rest], [PatternItem|PatternRest], ProgVarList, Xmins, Final, Extra, MinSoFar, MinVars) :-
    subsumes_term(PatternItem, Item), ! ,
    Item = PatternItem, % Simple instance so unification is okay.
    matches_pattern1(Rest, PatternRest, ProgVarList, Xmins, Final, Extra, MinSoFar, MinVars).

% % New case ... check this
matches_pattern1([Item|Rest], [PatternItem|PatternRest], ProgVarList, Xmins, Final, Extra, MinSoFar, MinVars) :-
    Item = grp([I], N), PatternItem = grp([I], M), ground(M), (variable(N); (ground(N), N1 is N, M1 is M, N1 > M1)), !, % cases in which Pattern matches but does not subsume Item ... 
    next_items(M, [Item], _List, Extra1, []),  zeroable(PatternRest, ProgVarList, Xmins), % Need condition that PatternRest is zeroable ... 
    matches_pattern1(Rest, PatternRest, ProgVarList, Xmins, Final, Extra2, MinSoFar, MinVars),
    append(Extra1, Extra2, Extra). 

matches_pattern1([Item|Rest], [PatternItem|PatternRest], ProgVarList, Xmins, Final, Extra, MinSoFar, MinVars) :-
    \+ (Item = PatternItem), % Doesn't have to match PatternItem if its minimum is zero ...
    pattern_minimum_zero(PatternItem, ProgVarList, Xmins), !, 
    matches_pattern1([Item|Rest], PatternRest, ProgVarList, Xmins, Final, Extra, MinSoFar, MinVars).
matches_pattern1([], [PatternItem|PatternRest], ProgVarList, Xmins, Final, [], MinSoFar, MinVars) :-
    PatternItem = grp(I, N), all_blank(I), ground(N), !, % Blank matches all padding patterns ...
    matches_pattern1([], PatternRest, ProgVarList, Xmins, Final, [], MinSoFar, MinVars). 
matches_pattern1([], [PatternItem|PatternRest], ProgVarList, Xmins, Final, [], MinSoFar, MinVars) :-
    PatternItem = grp(I, N), all_blank(I), variable(N), term_variables(N, NVars), % Padding may have exponent which is an expression containing a variable ... 
    append(MinSoFar, NVars, Temp), sort(Temp, NewMinSoFar), % to deal with any duplicates .. 
    matches_pattern1([], PatternRest, ProgVarList, Xmins, Final, [], NewMinSoFar, MinVars). 
matches_pattern1([], [PatternItem|PatternRest], ProgVarList, Xmins, Final, [], MinSoFar, MinVars) :-
    PatternItem = grp(I, N), \+ all_blank(I),  % Must do this to ensure that the variable must be 0 to match the empty tape, unless I is all blanks (like in padding)
    term_variables(N, NVars), length(NVars, L), length(Values, L), all_zeroes(Values), assign_vars(NVars, Values),  % Otherwise ... 
    matches_pattern1([], PatternRest, ProgVarList, Xmins, Final, [], MinSoFar, MinVars). 

pattern_minimum_zero(PatternItem, ProgVarsList, Mins) :-
    PatternItem = grp(_I,N), var(N),
    term_string(N, NString), find_min(NString, ProgVarsList, Mins, Min),
    0 is Min,
    N = 0. % As in matching an empty tape, we need to make sure that the Pattern variable is set to zero in this case. 

zeroable([], _, _).
zeroable([Item|Rest], ProgVarList, Xmins) :- pattern_minimum_zero(Item, ProgVarList, Xmins), zeroable(Rest, ProgVarList, Xmins). 

assign_minima([], _, _, _, _). 
assign_minima([V|Rest], VarList, VarsNames, AllMins, Final) :-
    var(V), term_string(V, VString, [variable_names(VarsNames)]), find_min(VString, VarList, AllMins, Min),
    V = Min,
    assign_minima(Rest, VarList, VarsNames, AllMins, Final).
assign_minima([V|Rest], VarList, VarsNames, AllMins, Final) :-
    ground(V), assign_minima(Rest, VarList, VarsNames, AllMins, Final).

assign_vars([], _).
assign_vars([V|Rest], [Val|Values]):- V = Val, assign_vars(Rest, Values). 

find_min(N, [Var|_Vars], [Xmin|_Xmins], Xmin) :- N == Var.
find_min(N, [Var|Vars], [_Xmin|Xmins], Min) :- N \== Var, find_min(N, Vars, Xmins, Min). 

add_extra(Config, LeftExtra, RightExtra, NewConfig) :-
    Config = config(Left, State, Item, Dir, Right),
    append(Left, LeftExtra, NewLeft),
    append(Right, RightExtra, NewRight), 
    NewConfig = config(NewLeft, State, Item, Dir, NewRight). 

reconfigure(Config, Config) :-    Config = config(_Left, _State, Item, _Dir, _Right), Item = grp(I,N), I \== [], \+ ground(N). 
reconfigure(Config, Config) :-    Config = config(_Left, _State, Item, _Dir, _Right), Item = grp(I,N), I \== [], ground(N), integer_safe(N), N1 is N, N1 > 0.
reconfigure(Config, Config) :-    Config = config(_Left, _State, Item, _Dir, _Right), Item = grp(_I,N),  ground(N), \+ integer_safe(N). % Not sure so don't do it. 
reconfigure(Config, NewConfig) :- Config = config(_Left, _State, Item, _Dir, _Right), Item = grp(_I,N), ground(N), integer_safe(N), 0 is N, !, move_item(Config, NewConfig). 
reconfigure(Config, NewConfig) :- Config = config(_Left, _State, Item, _Dir, _Right), Item = grp(I,_N), I == [], !, move_item(Config, NewConfig). 

move_item(Config, NewConfig) :-
    Config = config(Left, State, _Item, l, Right),
    purify_tape(Left, PLeft), purify_tape(Right, PRight), 
    % not quite the same as next_item ...
    move_item1(PRight, NewItem, NewRight),
    NewConfig = config(PLeft, State, NewItem, l, NewRight). 
move_item(Config, NewConfig) :-
    Config = config(Left, State, _Item, r, Right),
    purify_tape(Left, PLeft), purify_tape(Right, PRight), 
    % not quite the same as next_item ...
    move_item1(PLeft, NewItem, NewLeft),
    NewConfig = config(NewLeft, State, NewItem, r, PRight). 

move_item1([Item|NewTape], Item, NewTape).
move_item1([], grp(Blanks,1), []) :- blank_symbol(B), blanks(B, 1, Blanks). % If this happens, there is probably an error somewhere ... 

