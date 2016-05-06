emulate_bb(M, Bound, Mode, Inputs, Ones, Hops, Status, Outputs) :-
    emulate_general(M, blank, Bound, Mode, [loop,escape], Inputs, Hops, Status, Outputs),
    member(config(Left, State, Item, Dir, Right), Outputs),
    count_ones(config(Left, State, Item, Dir, Right), Ones),
    final_output(config(Left,State,Item,Dir,Right), Ones, Hops, Outputs), 
    true. 

final_output(config(Left,State,Item,Dir,Right), Ones, Hops, Outputs) :- 
    format("Final configuration is ", []), 
    display_config(config(Left, State, Item, Dir, Right)), !, % nl,
    format(" Ones: ", []), print_bignum(Ones), !, ttyflush, 
    format(" Hops: ", []), print_bignum(Hops), !, ttyflush, 
    member(steps(Steps), Outputs), 
    format(" Steps: ", []),  print_bignum(Steps), !, ttyflush, 
    member(otters(Otters), Outputs), 
    format(" Otters: ", []), print_bignum(Otters), !, ttyflush, 
    member(ottersteps(OtterSteps), Outputs), 
    format(" Otter Steps: ", []), print_bignum(OtterSteps), format("<br>~n", []), !, ttyflush, 
    nl, 
    true.

initial_config(naive, blank, config([], a, I, l, [])) :- blank_symbol(I), !. 
initial_config(naive, config(Left, State, Item, Dir, Right), config(Left, State, Item, Dir, Right)). 
initial_config(macro(K), blank, config([], a, grp(List,1), l, [])) :- integer(K), blanks(K, List). 
initial_config(macro(_K), config(Left, State, Item, Dir, Right), config(Left, State, Item, Dir, Right)). 
initial_config(adapt, blank, config([], a, I, l, [])) :- blank_symbol(I), !. 
initial_config(adapt, config(Left, State, Item, Dir, Right), config(Left, State, Item, Dir, Right)). 
initial_config(flex, blank, config([], a, grp(List,1), l, [])) :- blanks(1, List), !. % Act like macro(1) initially
initial_config(flex, config(Left, State, Item, Dir, Right), config(Left, State, Item, Dir, Right)). 

% Status is one of halts, going, abort, loops(cycle), blank, loops(road_runner).
emulate_general(M, Start, B, Mode, Termination, Inputs, Hops, Status, NewOutputs) :-
     initial_config(Mode, Start, Config), 
     process_inputs(Inputs, Config, Inputs1), 
     process_bound(B, Bound), 
     machine(Mode, M, Machine), 
     output_status(Config, Mode, 0, Inputs1, NewInputs, init), 
     execute(Machine, Config, Bound, Mode, 0, Termination, NewInputs, Result), 
     Result = result(Machine1, Config1, Bound, Mode, Hops, _, Outputs, Status), 
     append(Outputs, [Machine1], Outputs1),  
     append([Config1], Outputs1, Outputs2),
     NewOutputs = Outputs2, 
     !. 

process_inputs(Inputs, Config, Inputs2) :-
    (member(loop, Inputs); member(history, Inputs); member(otter, Inputs); member(ocelot, Inputs)), 
    append([now(Config), history([])], Inputs, Inputs1),
    append([pos(0), steps(0), otters(0), ottersteps(0)], Inputs1, Inputs2).
process_inputs(Inputs, _Config, Inputs1) :-
    \+ member(loop, Inputs), \+ member(history, Inputs), \+ member(otter, Inputs), \+ member(ocelot, Inputs), 
    append([pos(0), steps(0), otters(0), ottersteps(0)], Inputs, Inputs1).

process_bound(N, N) :- integer(N). 
process_bound(mill(N), Num) :- integer(N), Num is N*1000000. % Num is N million.
process_bound(bill(N), Num) :- integer(N), Num is N*1000000000. % Num is N billion.
process_bound(trill(N), Num) :- integer(N), Num is N*1000000000000. % Num is N trillion.
process_bound(tens(N), Num) :- integer(N), Num is 10 ** N. % Num is 10**N.

machine(_, M, M) :- M = machine(_,_,_), !. 
machine(naive, M, machine(M, [], [])).
machine(macro(_K), M, machine(M, [], [])).
machine(adapt, M, machine(M, [], [])).
machine(flex, M, machine(M, [], [])).

execute(M, Config, Bound, Mode, Hops, Termination, Inputs, Result) :-
    halt_execution(M, Config, Bound, Mode, Hops, Termination, Inputs, Result), !. 
execute(M, Config, Bound, Mode, Hops, Termination, Inputs, Result) :-
    next_config(M, Config, Mode, Hops, Inputs, NewM, NewConfig, NewHops, NewInputs, Used), !, 
    output_status(NewConfig, Mode, NewHops, NewInputs, NewerInputs, Used),!, 
    execute(NewM, NewConfig, Bound, Mode, NewHops, Termination, NewerInputs, Result). 

halt_execution(M, Config, Bound, Mode, Hops, Termination, Inputs, Result) :-
    Config = config(_Left, State, _Item, _Dir, _Right),
    halt_state(State), 
    Result = result(M, Config, Bound, Mode, Hops, Termination, Inputs, halts).
    
halt_execution(M, Config, Bound, Mode, Hops, Termination, Inputs, Result) :-
    Config = config(_Left, State, _Item, _Dir, _Right),
    looping_state(State), 
    Result = result(M, Config, Bound, Mode, Hops, Termination, Inputs, loops(cycle)).
    
halt_execution(M, Config, Bound, Mode, Hops, Termination, Inputs, Result) :-
    Hops >= Bound, 
    Result = result(M, Config, Bound, Mode, Hops, Termination, Inputs, going).

halt_execution(M, Config, Bound, Mode, Hops, Termination, Inputs, Result) :-
    % format("Halt 4~n", []), 
    member(maxsteps(Max), Inputs), member(steps(Steps), Inputs), Steps >= Max, !, 
    % format("% Bound of ~d exceeded~n", [Bound]), !, 
    Result = result(M, Config, Bound, Mode, Hops, Termination, Inputs, going(Hops)).

halt_execution(M, Config, Bound, Mode, Hops, Termination, Inputs, Result) :-
    no_transition(M, Config, Mode),     
    Result = result(M, Config, Bound, Mode, Hops, Termination, Inputs, abort).

halt_execution(M, Config, Bound, Mode, Hops, Termination, Inputs, Result) :-
    member(loop, Termination),
    looping(Config, Inputs), 
    Result = result(M, Config, Bound, Mode, Hops, Termination, Inputs, loops(cycle)).

halt_execution(M, Config, Bound, Mode, Hops, Termination, Inputs, Result) :-
    member(blank, Termination), Hops > 0, 
    blank_tape(Config), 
    Result = result(M, Config, Bound, Mode, Hops, Termination, Inputs, blank).

halt_execution(M, Config, Bound, Mode, Hops, Termination, Inputs, Result) :-
    member(escape, Termination),
    road_runner(M, Config), 
    Result = result(M, Config, Bound, Mode, Hops, Termination, Inputs, loops(road_runner)).

no_transition(M, Config, Mode) :-
    Mode = naive,
    Config = config(_Left, State, Item, _Dir, _Right), 
    \+ simple_transition(M, State, Item, _NewState, _OutItem, _OutDir, _Steps),
    true. 
no_transition(M, Config, Mode) :-
    member(Mode, [macro(_K),adapt,flex]), 
    Config = config(_Left, State, Item, Dir, _Right), 
    first_item(Item, Dir, I), 
    \+ simple_transition(M, State, I, _NewState, _OutItem, _OutDir, _Steps),
    true. 

first_item(I, _, I) :- is_input(I). 
first_item([I|_], l, I).
first_item(Item, r, I) :- reverse(Item, RevItem), RevItem = [I|_]. 

looping(Config, Inputs) :-
    member(history(History), Inputs),
    member(s(_Hops, Config, _), History). 

blank_tape(config(Left, _State, Item, _Dir, Right)) :-
    is_blank(Item),
    all_blank(Left), all_blank(Right).

is_blank(I) :- blank_symbol(I).
is_blank(grp(I,_)) :- all_blank(I).

all_blank([]).
all_blank([I|Rest]) :- is_blank(I), all_blank(Rest). 

road_runner(M, Config) :-
     M = machine(Translist, _, _),
     Config = config(Left, State, Item, _Dir, Right), 
     blank_symbol(Item), 
     road_runner1(Translist, Left, State, Right), 
     true.

road_runner1(Translist, _Left, State, Right) :-
     all_blank(Right),
     blank_cycle(Translist, State, r).
road_runner1(Translist, Left, State, _Right) :-
     all_blank(Left),
     blank_cycle(Translist, State, l).

blank_cycle(Translist, State, Dir) :-
     blank_symbol(Item),
     member(t(State, Item, Item, Dir, State), Translist),  % cycle length 1
     true. 
blank_cycle(Translist, State, Dir) :-
     blank_symbol(Item),
     member(t(State, Item, Item, Dir, NS), Translist), State \== NS, % cycle length 2
     member(t(NS, Item, Item, Dir, State), Translist),
     true. 
blank_cycle(Translist, State, Dir) :-
     blank_symbol(Item),
     member(t(State, Item, Item, Dir, NS), Translist), State \== NS, % cycle length 3
     member(t(NS, Item, Item, Dir, NS2), Translist), State \== NS2, NS \== NS2, 
     member(t(NS2, Item, Item, Dir, State), Translist),
     true. 
blank_cycle(Translist, State, Dir) :-
     blank_symbol(Item),
     member(t(State, Item, Item, Dir, NS), Translist), State \== NS, % cycle length 4
     member(t(NS, Item, Item, Dir, NS2), Translist), State \== NS2, NS \== NS2, 
     member(t(NS2, Item, Item, Dir, NS3), Translist), State \== NS3, NS \== NS3, NS2 \== NS3, 
     member(t(NS3, Item, Item, Dir, State), Translist), 
     true. 
blank_cycle(Translist, State, Dir) :-
     blank_symbol(Item),
     member(t(State, Item, Item, Dir, NS), Translist), State \== NS, % cycle length 5
     member(t(NS, Item, Item, Dir, NS2), Translist), State \== NS2, NS \== NS2, 
     member(t(NS2, Item, Item, Dir, NS3), Translist), State \== NS3, NS \== NS3, NS2 \== NS3, 
     member(t(NS3, Item, Item, Dir, NS4), Translist), State \== NS4, NS \== NS4, NS2 \== NS4, NS3 \== NS4, 
     member(t(NS4, Item, Item, Dir, State), Translist), 
     true. 

next_config(M, Config, Mode, Hops, Inputs, NewM, NewConfig, NewHops, NewInputs, simple) :-
     Mode = naive,
     Config = config(Left, State, Item, _Dir, Right), 
     % format("Outputting status~n", []), 
     
     % format("Finding transition~n", []), 
     simple_transition(M, State, Item, NewState, OutItem, OutDir, Steps),
     % format("Updating tape~n", []), 
     updatetape(Mode, Left, OutItem, Right, OutDir, NewLeft, NewItem, NewRight, _Moves),
     NewHops is Hops + Steps,
     NewConfig = config(NewLeft, NewState, NewItem, l, NewRight),
     NewM = M,
     % format("Updating pos~n", []), display(Inputs), nl, 
     % display(Inputs1), nl, 
     % format("Updating inputs~n", []), 
     updateinputs(Config, Mode, Hops, OutDir, 1, Inputs, NewConfig, NewHops, NewInputs), 
     true. 

next_config(M, Config, Mode, Hops, Inputs, NewM, NewConfig, NewHops, NewInputs, otter) :-
     Mode = macro(_K),
     Config = config(Left, State, Item, Dir, Right), 
     (member(otter, Inputs); (\+ member(otter, Inputs), member(ocelot, Inputs))), 
     Item = grp(_I, _Number), 
     % Observant Otter, ie calculated, not found from stored pattern

     member(history(History), Inputs),
     member(s(Hops1, config(Left1, State, Item1, Dir, Right1), _), History), Hops1 < Hops, 
     % Config1 = config(Left1, State, Item1, Dir, Right1), 
     same_shape(config(Left1, State, Item1, Dir, Right1), config(Left, State, Item, Dir, Right)),

     regression(config(Left1, State, Item1, Dir, Right1), config(Left, State, Item, Dir, Right), LDiffs1, ItemDiff1, RDiffs1),
     member(s(Hops2,config(Left2, State, Item2, Dir, Right2), _), History), Hops2 < Hops1, 

     same_shape(config(Left2, State, Item2, Dir, Right2), config(Left1, State, Item1, Dir, Right1)),
     regression(config(Left2, State, Item2, Dir, Right2), config(Left1, State, Item1, Dir, Right1), LDiffs2, ItemDiff2, RDiffs2),

     LDiffs1 == LDiffs2, ItemDiff1 == ItemDiff2, RDiffs1 == RDiffs2, 

     % find minimum regressor
     regression_counters(Left, LDiffs1, [Item|Right], [ItemDiff1|RDiffs1], [], Regs), 

     length(Regs, N), N > 0, 
     % format("Diffs are ",[]), display(LDiffs1), display([ItemDiff1|RDiffs1]), nl, 
     % format("Regressors are ", []), display(Regs), nl, 
     ( (N > 1) -> (format("Multiple regressors found~n", []),fail) ; true), 
     Regs = [reg(Num,L)|_], 

     Diff1 is Hops1 - Hops2, 
     Diff2nd is (Hops - Hops1) - Diff1, 
     % format("Arguments to setleaps are ~d ~d ~d ~d~n", [Diff1, Diff2nd, Num, L]), 
     setleaps(Diff1, Diff2nd, Num, L, Leaps), Leaps > 0, set_new_count(Num, L, Count), Count > 0, % format("Count is ~d~n", [Count]), 
     !, % commit to this case

     ( (member(otrace, Inputs) ) -> 
	 ( format("Otter Observed: ~d ~d ~d Diff1 is ~d Diff2 is ~d Num is ~d L is ~d Count is ~d Leaps is ~d ", [Hops2, Hops1, Hops, Hops1 - Hops2, Hops - Hops1, Num, L, Count, Leaps]), % Hops2 < Hops1 < Hops
	   display_config(config(Left2, State, Item2, Dir, Right2)), format(" + ", []), 
	   display_config(config(Left1, State, Item1, Dir, Right1)), format(" + ", []), 
	   display_config(config(Left, State, Item, Dir, Right)), nl 
          ) ; true ) , 
  
     % member(pos(Pos), Inputs), % format("Pos is ~d~n", [Pos]), 

     % find final configuration by changing Config by Count appropriately

     % format("Calling final_config with ~d ~d ", [Num,L]), write(config(Left, State, Item, Dir, Right)), write(LDiffs1), write(ItemDiff1), write(RDiffs1), nl, 

     final_config(Mode, Num, L, config(Left, State, Item, Dir, Right), LDiffs1, ItemDiff1, RDiffs1, NewConfig), 

     NewHops is Hops + Leaps, 
     (member(otrace, Inputs) -> (format("Final config is ~d:", [NewHops]), display_config(NewConfig), nl, ttyflush); true), 
     % append([s(Hops,Config,Pos)], History, History_Now), 

     %% Verify otter here???? 
    
     % write(NewConfig), nl, 

     NewM = M, % no ocelot, so no updates to M ... 
     NewConfig = config(_,_,_,OutDir,_), 

     update_otters(Leaps, Inputs, NInputs), 
     updateinputs(Config, Mode, Hops, OutDir, Leaps, NInputs, NewConfig, NewHops, NewInputs), 
     true. 

next_config(M, Config, Mode, Hops, Inputs, NewM, NewConfig, NewHops, NewInputs, complex) :-
     Mode = macro(_K),
     Config = config(_Left, State, Item, Dir, _Right), 

     Item = grp(I, _Number), 

     complex_transition(M, State, I, Dir, NewM, NewState, OutItem, OutDir, Steps),!, 
     updatetapemacro(Config, Mode, trans(State, I, Dir, NewState, OutItem, OutDir, Steps), NewLeft, NewItem, NewDir, NewRight, Leaps, Moves),
     NewHops is Hops + Leaps,
     NewConfig = config(NewLeft, NewState, NewItem, NewDir, NewRight),
     updateinputs(Config, Mode, Hops, OutDir, Moves, Inputs, NewConfig, NewHops, NewInputs), 
     true. 

setleaps(Diff1, Diff2nd, Num, L, Leaps) :-
     set_new_count(Num, L, Count), 
     Leaps is ( Count*Count  + Count) *Diff2nd/2 + Count*(Diff2nd + Diff1). 

set_new_count(Num, L, Count) :- X is Num mod L, X > 0, Count is Num // L.
set_new_count(Num, L, Count) :- X is Num mod L, X is 0, Count is (Num // L) - 1.

set_new_var_count(Num, L, Count) :- 
       Count = (Num // L)  - (((Num//L)*L)//Num). % Second term is 0 if Num mod L > 0, 1 if Num mod L = 0. This means we can 'delay' the set_new_count calcuation until the value of Num is known. Crude, but ... 

predictleaps(Diff1, Diff2nd, Num, L, Leaps) :-
     % Uses formula for a_n - a_0 
     var(Num), 
     set_new_var_count(Num, L, Count), 
     Leaps = (Count)*Diff1 + (Count - 1)*(Count)*Diff2nd/2.

opposite_direction(l,r). 
opposite_direction(r,l). 

same_shape(config(Left1, State1, Item1, Dir1, Right1), config(Left2, State2, Item2, Dir2, Right2)) :-
     State1 == State2, 
     Dir1 == Dir2, 
     same_shape1(Left1, Left2),
     same_shape1([Item1], [Item2]),
     same_shape1(Right1, Right2).

same_shape1([], []).
same_shape1([grp(I1,N1)|Rest1], [grp(I2,N2)|Rest2]) :-
     integer(N1), integer(N2),
     I1 == I2,
     same_shape1(Rest1, Rest2). 
same_shape1([grp(I1,N1)|Rest1], [grp(I2,N2)|Rest2]) :-
     variable(N1), variable(N2),
     I1 == I2,
     same_shape1(Rest1, Rest2). 

regression(config(Left1, _State1, Item1, _Dir1, Right1), config(Left2, _State2, Item2, _Dir2, Right2), LDiffs, ItemDiff, RDiffs) :- 
     changes(Left1, Left2, LDiffs),    
     changes(Right1, Right2, RDiffs),  
     changes([Item1], [Item2], [ItemDiff]),
     regressive(LDiffs, [ItemDiff|RDiffs]).

changes([], [], []).
changes([grp(I1,N1)|R1], [grp(I2,N2)|R2], [m(1,L)|Rest]) :-
        I1 == I2, 
	integer(N1), integer(N2), 
	L is N2 - N1, % L is 1, % stick to decreasing by 1 until we work out more details of the larger case 
	changes(R1, R2, Rest), !. 
changes([grp(I1,N1)|R1], [grp(I2,N2)|R2], [m(1,L)|Rest]) :-
        I1 == I2, 
	variable(N1), variable(N2), 
        value(N1, V1), value(N2, V2),
        L is V2 - V1,
	changes(R1, R2, Rest), !. 

regressive([m(_, LL)|_LDiffs], _RDiffs) :-
        LL < 0. 

regressive([m(_,LL)|_LDiffs], [m(_,RR)|_RDiffs]) :-
        LL >= 0, RR < 0.
regressive([m(_,LL)|LDiffs], [m(_,RR)|RDiffs]) :-
        LL >= 0, RR >= 0,
        regressive(LDiffs, RDiffs).

regression_counters([L1|Left], [LDiff|LDiffs], Right, RDiffs, SoFar, Counts) :-
      regressors(SoFar, L1, LDiff, NewSoFar), 
      regression_counters(Left, LDiffs, Right, RDiffs, NewSoFar, Counts). 
regression_counters([], [], [R1|Right], [RDiff|RDiffs], SoFar, Counts) :-
      regressors(SoFar, R1, RDiff, NewSoFar), 
      regression_counters([], [], Right, RDiffs, NewSoFar, Counts). 
regression_counters([], [], [], [], Counts, Counts). 

regressors(List, _Item, Diff, List) :-
      Diff = m(_K,L), L >= 0.
regressors(List, Item, Diff, [reg(Num, LL)|List]) :-
      Item = grp(_I,Num),  
      Diff = m(_K,L), L < 0, 
      LL is 0 - L,
      true. 
      % setcount(Num, LL, Count). 
      %%  Count is Num // LL.

eval_regs([], []).
eval_regs([reg(Num,LL)|Rest], [Count|Result]) :- integer(Num), integer(LL), Count is Num // LL, eval_regs(Rest, Result). 

% setcount(Num, L, Count). 
setcount(Num, L, Count) :-
      T is Num mod L, T > 0, Count is Num // L.
setcount(Num, L, Count) :-
       T is Num mod L, T = 0, Count is Num // L. % Trying to 0 ... 

minimum_list([List], List) :- length([List], 1).
minimum_list([Item|List], Minimum) :- minimum1(Item, List, Minimum).
minimum1(Item, [], Item).
minimum1(Item, [Item2|Rest], Minimum) :-
      Item =< Item2,
      minimum1(Item, Rest, Minimum). 
minimum1(Item, [Item2|Rest], Minimum) :-
      Item > Item2,
      minimum1(Item2, Rest, Minimum). 

final_config(_, Num, _L, Config, _LDiffs, _ItemDiff, _RDiffs, Config) :-
      integer(Num), Num is 0. 
final_config(Mode, Num, L, config(Left, State, Item, Dir, Right), LDiffs, ItemDiff, RDiffs, NewConFig) :-
      integer(Num), 
      Num > 0,
      configlist(Num, L, Left, LDiffs, NewL), %% check what happens in the variable case here
      configlist(Num, L, [Item], [ItemDiff], [NewI]), 
      configlist(Num, L, Right, RDiffs, NewR),
      reconfigurate(Mode, config(NewL, State, NewI, Dir, NewR), NewConFig),
      true. 
final_config(Mode, Num, L, config(Left, State, Item, Dir, Right), LDiffs, ItemDiff, RDiffs, NewConFig) :-
      \+ integer(Num), % assume it is an expression
      configlist(Num, L, Left, LDiffs, NewL), 
      configlist(Num, L, [Item], [ItemDiff], [NewI]), 
      configlist(Num, L, Right, RDiffs, NewR),
      reconfigurate(Mode, config(NewL, State, NewI, Dir, NewR), NewConFig),
      true. 

configlist(_Num, _LL, [], _Diffs, []). 
configlist(Num, LL, [Item|Rest], [Diff|Diffs], [NewItem|Result]) :-
      Item = grp(I,Num1),
      integer(Num), 
      integer(Num1), %%% Make sure a non-zero amount is left, for now ... 
      Diff = m(_K,L),
      set_new_count(Num, LL, Count), 
      NewNum is ( Count * (L)) + Num1, 
      NewItem = grp(I,NewNum), 
      configlist(Num, LL, Rest, Diffs, Result).       

configlist(Num, LL, [Item|Rest], [Diff|Diffs], [NewItem|Result]) :-
      Item = grp(I,Num1), \+ (integer(Num), integer(Num1)), 
      Diff = m(_K,L), L is 0, 
      NewNum = Num1, %% Own expression evaluator here?
      NewItem = grp(I,NewNum), 
      configlist(Num, LL, Rest, Diffs, Result).       

configlist(Num, LL, [Item|Rest], [Diff|Diffs], [NewItem|Result]) :-
      Item = grp(I,Num1), \+ (integer(Num), integer(Num1)), 
      Diff = m(_K,L), \+ (L is 0), %%%% Need to use predictleaps here,  to get the count correct ... 
      set_new_var_count(Num, LL, Count), 
      NewNum = ( (Count) * (L)) + Num1, %% Own expression evaluator here?
      NewItem = grp(I,NewNum), 
      configlist(Num, LL, Rest, Diffs, Result).       

display_config(config(Left, State, Item, Dir, Right)) :-
     reverse(Left, RevLeft), 
     pprint(RevLeft), format("{~k}", [State]), pprint([Item]), format("{~k}", [Dir]), pprint(Right), true. 

simple_transition(M, State, Item, NewState, NewItem, Dir, 1):-
    \+ halt_state(State), 
     M = machine(TransList, _, _),
     member(t(State, Item, NewItem, Dir, NewState), TransList). 

% Think about how this would work for more than two symbols ... 
complex_transition(M, State, Item, InDir, NewM, NewState, OutItem, OutDir, Steps) :-
    \+ halt_state(State), 
    % format("Case 1~n", []), 
    M = machine(_, MacroList, _), 
    member(trans(State, Item, InDir, NewState, OutItem, OutDir, Steps), MacroList), !, 
    NewM = M. 

complex_transition(M, State, Item, InDir,  NewM, NewState, OutItem, OutDir, Steps) :-
    \+ halt_state(State), 
    % format("Case 2~n", []), 
    M = machine(_, MacroList, _), 
    \+ member(trans(State, Item, InDir, _NewState, _OutItem, _OutDir, _Steps), MacroList), !, 
    % find new transition
    % format("Finding new transition~n", []), ttyflush, 
    length(Item, Len),
    number_of_states(M, Num), 
    B is truncate(Num * Len * 2**Len), maxbound(B, Bound), 
    find_transition(M, State, Item, InDir, Bound, NewM, NewState, OutItem, OutDir, Steps). 

find_transition(M, State, Item, l, Bound, NewM, NewState, OutItem, OutDir, Steps) :-
    Item = [I|Rest], append(Rest, [right_end], NewRest), 
    % format("calling wombat left~n", []), ttyflush, 
    wild_wombat(config([left_end], State, I, l, NewRest), M, Bound, [pos(0)], 0, NewState, OutItem, OutDir, Steps),
    % format("wombat done~n", []), ttyflush, 
    M = machine(TransList, MacroList, List), 
    add(trans(State, Item, l, NewState, OutItem, OutDir, Steps), MacroList, NewMacroList),
    NewM = machine(TransList, NewMacroList, List). 

find_transition(M, State, Item, r, Bound, NewM, NewState, OutItem, OutDir, Steps) :-
    splitlast(Item, L1, [I]), reverse(L1, Left), !, append(Left, [left_end], NewLeft), 
    % format("Calling wombat right~n", []), display(NewLeft), display(I), nl, 
    wild_wombat(config(NewLeft, State, I, r, [right_end]), M, Bound, [pos(0)], 0, NewState, OutItem, OutDir, Steps),
    % format("wombat done~n", []), ttyflush, 
    M = machine(TransList, MacroList, List), 
    add(trans(State, Item, r, NewState, OutItem, OutDir, Steps), MacroList, NewMacroList),
    NewM = machine(TransList, NewMacroList, List). 

wild_wombat(config(Left, State, Item, _Dir, Right), _M, _Bound, _Inputs, Hops, NewState, OutItem, OutDir, Hops) :-
    halt_state(State), 
    NewState = State, 
    collect(Left, Item, Right, OutI), clean_tape(OutI, OutItem), 
    OutDir = r.

wild_wombat(config(_Left, State, Item, Dir, _Right), _M, Bound, _Inputs, Hops, loop, Item, Dir, Hops) :-
    \+ halt_state(State), 
    Hops >= Bound, 
    true. 

wild_wombat(config(Left, State, Item, Dir, Right), _M, Bound, _Inputs, Hops, State, OutItem, OutDir, Hops) :-
    \+ halt_state(State), 
    Hops < Bound,
    out_of_bounds(Left, Item, Right, Dir, OutItem, OutDir), 
    % This is normal termination. 
   true. 

wild_wombat(config(Left, State, Item, Dir, Right), M, Bound, Inputs, Hops, NewState, OutItem, OutDir, NewHops) :-
    \+ halt_state(State), 
    Hops < Bound,
    \+ out_of_bounds(Left, Item, Right, Dir, _, _), 
    simple_transition(M, State, Item, NState, NItem, NDir, Steps), 
    updatetape(naive, Left, NItem, Right, NDir, NLeft, NewItem, NRight, _Moves), 
    NewConfig = config(NLeft, NState, NewItem, NDir, NRight), 
    Hops1 is Hops + Steps, 
    wild_wombat(NewConfig, M, Bound, Inputs, Hops1, NewState, OutItem, OutDir, NewHops), 
    true. 
    
collect(Left, Item, Right, OutItem) :-
    append(Left, [Item], Temp), append(Temp, Right, OutItem). 

out_of_bounds([], left_end, Right, l, OutItem, l) :- clean_tape(Right, OutItem).
out_of_bounds(Left, right_end, [], r, OutItem, r) :- reverse(Left, New), clean_tape(New,OutItem). 

clean_tape([], []).
clean_tape([left_end|Rest], Result) :- clean_tape(Rest, Result). 
clean_tape([right_end|Rest], Result) :- clean_tape(Rest, Result). 
clean_tape([I|Rest], [I|Result]) :- I \== left_end, I\== right_end, clean_tape(Rest, Result). 

reconfigurate(_Mode, config(Left, State, Item, Dir, Right), config(Left, State, Item, Dir, Right)) :-
     Item = grp(I,Num), I \== [], integer(Num), Num > 0. 
reconfigurate(_Mode, config(Left, State, Item, Dir, Right), config(Left, State, Item, Dir, Right)) :-
     Item = grp(I,Num), I \== [], \+ integer(Num).

reconfigurate(Mode, config(Left, State, Item, l, Right), config(Left, State, NewItem, NewDir, NewRight)) :-
     Item = grp(I,Num), (I = []; (I \== [], (integer(Num), Num = 0);\+ integer(Num))), % format("Case1 ~n"i, []), 
     next_item(Mode, Right, NewItem, NewRight), % format("Case1 ~n", []), 
     NewDir = l. 
reconfigurate(Mode, config(Left, State, Item, r, Right), config(NewLeft, State, NewItem, NewDir, Right)) :-
     Item = grp(I,Num), (I = []; (I \== [], (integer(Num), Num = 0);\+ integer(Num))), % format("Case2 ~n", []), 
     next_item(Mode, Left, NewItem, NewLeft), % format("Case2 ~n", []), 
     NewDir = r. 
reconfigurate(Mode, config(Left, State, Item, _Dir, Right), config(Left, State, NewItem, NewDir, NewRight)) :-
     Item = right, 
     next_item(Mode, Right, NewItem, NewRight),
     NewDir = l. 
reconfigurate(Mode, config(Left, State, Item, _Dir, Right), config(NewLeft, State, NewItem, NewDir, Right)) :-
     Item = left, 
     next_item(Mode, Left, NewItem, NewLeft),
     NewDir = r. 

updatetape(Mode, Left, Item, Right, l, NewLeft, NewItem, NewRight, Move) :-
     append([Item], Right, NewRight),
     % display(Mode), display(Left), nl, 
     next_item(Mode, Left, NewItem, NewLeft), Move is 0 - 1. 

updatetape(Mode, Left, Item, Right, r, NewLeft, NewItem, NewRight, 1) :-
     append([Item], Left, NewLeft),
     % display(Mode), display(Right), nl, 
     next_item(Mode, Right, NewItem, NewRight). 

updatetapemacro(Config, Mode, trans(State, I, Dir, NewState, OutI, OutDir, Steps), NewLeft, NewItem, NewDir, NewRight, Leaps, Moves) :-
     Config = config(Left, State, Item, Dir, Right), 
     Item = grp(I, Number), % integer(Number),  %% will work for variables too
     State == NewState, Dir = l, OutDir = r,  !, % Holkner case 1
     % format("Case 1~n", []), 
     add_item(grp(OutI, Number), Left, NewLeft),
     next_item(Mode, Right, NewItem, NewRight),
     NewDir = l,
     Leaps is Steps * Number,
     length(I, N), 
     Moves is N * Number. 

updatetapemacro(Config, Mode, trans(State, I, Dir, NewState, OutI, OutDir, Steps), NewLeft, NewItem, NewDir, NewRight, Leaps, Moves) :-
     Config = config(Left, State, Item, Dir, Right), 
     Item = grp(I, Number), % integer(Number), %% will work for variables too
     State == NewState, Dir = r, OutDir = l,  !, % Holkner case 2
     % format("Case 2~n", []), 
     add_item(grp(OutI, Number), Right, NewRight),
     % display(Mode), display(Left), nl, 
     next_item(Mode, Left, NewItem, NewLeft),
     NewDir = r,
     Leaps is Steps * Number,
     length(I, N), 
     Moves is 0 - N * Number. 
% add two extra cases here like the above for variables. % Should be okay given the way add_item works. 

updatetapemacro(Config, Mode, trans(State, I, Dir, _NewState, OutI, OutDir, Steps), NewLeft, NewItem, NewDir, NewRight, Leaps, Moves) :-
     Config = config(Left, State, Item, Dir, Right), 
     Item = grp(I, Number), integer(Number),  %% (State \== NewState; (State == NewState, Dir == r)), 
     Number = 1, OutDir = r,  !, 
     % format("Case 3~n", []), display(grp(OutI,Number)), display(Left), nl, 
     add_item(grp(OutI, Number), Left, NewLeft), 
     % format("Added~n", []), 
     next_item(Mode, Right, NewItem, NewRight),
     % format("Next~n", []), 
     NewDir = l,
     Leaps is Steps, 
     length(I, N), 
     set_moves(Dir, OutDir, N, Moves), 
     true. 

updatetapemacro(Config, Mode, trans(State, I, Dir, _NewState, OutI, OutDir, Steps), NewLeft, NewItem, NewDir, NewRight, Leaps, Moves) :-
     Config = config(Left, State, Item, Dir, Right), 
     Item = grp(I, Number), integer(Number), %% (State \== NewState; (State == NewState, Dir == l)), 
     Number = 1, OutDir = l,  !, 
     % format("Case 4~n", []), 
     add_item(grp(OutI, Number), Right, NewRight),
     % format("Added~n", []), 
     next_item(Mode, Left, NewItem, NewLeft),
     % format("Next~n", []), 
     NewDir = r,
     Leaps is Steps, 
     length(I, N), 
     set_moves(Dir, OutDir, N, Moves), 
     true. 

updatetapemacro(Config, Mode, trans(State, I, Dir, _NewState, OutI, OutDir, Steps), NewLeft, NewItem, NewDir, NewRight, Leaps, Moves) :-
     Config = config(Left, State, Item, Dir, Right), 
     Item = grp(I, Number), ( (integer(Number), Number > 1) ; variable(Number) ), 
     Dir = r, OutDir = r, OutI == I, !, 
     % format("Case 5~n", []), 
     add_item(grp(I, Number), Left, NewLeft), 
     next_item(Mode, Right, NewItem, NewRight),  
     NewDir = l,
     Leaps is Steps, 
     Moves is 1. 

updatetapemacro(Config, Mode, trans(State, I, Dir, _NewState, OutI, OutDir, Steps), NewLeft, NewItem, NewDir, NewRight, Leaps, Moves) :-
     Config = config(Left, State, Item, Dir, Right), 
     Item = grp(I, Number),  ( (integer(Number), Number > 1) ; variable(Number)),  % format("Case 6~n", []), display(I), display(OutI), nl,  
     Dir = r, OutDir = r, OutI \== I, !, 
     % format("Case 6~n", []), 
     decrement_exponent(grp(I, Number), grp(I, N1)), 
     add_item(grp(I, N1), Left, NLeft), 
     add_item(grp(OutI,1), NLeft, NewLeft), 
     next_item(Mode, Right, NewItem, NewRight),  
     NewDir = l, 
     Leaps is Steps,
     Moves is 1. 

updatetapemacro(Config, Mode, trans(State, I, Dir, _NewState, OutI, OutDir, Steps), NewLeft, NewItem, NewDir, NewRight, Leaps, Moves) :-
     Config = config(Left, State, Item, Dir, Right), 
     Item = grp(I, Number), ( (integer(Number), Number > 1) ; variable(Number) ), 
     Dir = l, OutDir = l, OutI == I, !, 
     % format("Case 7~n", []), 
     add_item(grp(I, Number), Right, NewRight), 
     next_item(Mode, Left, NewItem, NewLeft),  
     NewDir = r,
     Leaps is Steps, 
     Moves is 0 - 1. 

updatetapemacro(Config, Mode, trans(State, I, Dir, _NewState, OutI, OutDir, Steps), NewLeft, NewItem, NewDir, NewRight, Leaps, Moves) :-
     Config = config(Left, State, Item, Dir, Right), 
     Item = grp(I, Number), ( (integer(Number), Number > 1) ; variable(Number)), 
     Dir = l, OutDir = l, OutI \== I, !, 
     % format("Case 8~n", []), 
     decrement_exponent(grp(I,Number), grp(I,N1)), 
     add_item(grp(I, N1), Right, NRight), 
     add_item(grp(OutI,1), NRight, NewRight), 
     next_item(Mode, Left, NewItem, NewLeft),  
     NewDir = r,
     Leaps is Steps, 
     Moves is 0 - 1. 

updatetapemacro(Config, _Mode, trans(State, I, Dir, NewState, OutI, OutDir, Steps), NewLeft, NewItem, NewDir, NewRight, Leaps, Moves) :-
     Config = config(Left, State, Item, Dir, Right), 
     Item = grp(I, Number), ( (integer(Number), Number > 1) ; variable(Number) ), 
     State \== NewState, Dir = l, OutDir = r, !, 
     % format("Case 9~n", []), 
     decrement_exponent(grp(I,Number), grp(I,N1)), 
     NewItem = grp(I, N1), 
     add_item(grp(OutI,1), Left, NewLeft), 
     NewDir = l, 
     NewRight = Right, 
     Leaps is Steps, 
     length(I, N), 
     Moves is N.

updatetapemacro(Config, _Mode, trans(State, I, Dir, NewState, OutI, OutDir, Steps), NewLeft, NewItem, NewDir, NewRight, Leaps, Moves) :-
     Config = config(Left, State, Item, Dir, Right), 
     Item = grp(I, Number), ( (integer(Number), Number > 1) ; variable(Number) ), 
     State \== NewState, Dir = r, OutDir = l, !, 
     % format("Case 10~n", []), 
     decrement_exponent(grp(I,Number), grp(I,N1)), 
     NewItem = grp(I, N1), 
     add_item(grp(OutI,1), Right, NewRight), 
     NewDir = r,
     NewLeft = Left, 
     Leaps is Steps, 
     length(I, N), 
     Moves is 0 - N.

add_item(grp(I,N), [], [grp(I,N)]). 
add_item(grp(I1,N1), [grp(I2,N2)|Rest], [grp(I1,N3)|Rest]) :-
    I1 == I2, integer(N1), integer(N2), N3 is N1 + N2. 
add_item(grp(I1,N1), [grp(I2,N2)|Rest], Result) :-
    \+ (I1 == I2, integer(N1), integer(N2)), 
    append([grp(I1,N1)], [grp(I2,N2)|Rest], Result). 
add_item(grp(I,N), [I2|Rest], Result) :-
    \+ (I2 == grp(_,_)), 
    append([grp(I,N)], [I2|Rest], Result). 


next_item(naive, [], Blank, []) :- blank_symbol(Blank).
next_item(naive, [I|Rest], I, Rest).
next_item(macro(K), [], grp(Blanks,1), []) :- blank_symbol(B), blanks(B, K, Blanks). 
next_item(macro(_K), [I|Rest], I, Rest) :- I = grp(Item, Num), integer(Num), Num > 0, Item \== []. 
next_item(macro(K), [I|Rest], NewItem, NewRest) :- I = grp(Item, Num), integer(Num), (Num is 0; (Num > 0, Item = [])), next_item(macro(K), Rest, NewItem, NewRest). 
next_item(macro(_K), [I|Rest], I, Rest) :- I = grp(_Item, Num), variable(Num). 

blanks(_, 0, []). 
blanks(Blank, N, [Blank|Result]) :- N > 0, N1 is N - 1, blanks(Blank, N1, Result). 

set_moves(l, r, N, N).
set_moves(r, l, N, M) :- M is 0 - N. 
set_moves(r, r, _N, 1). 
set_moves(l, l, _N, M) :- M is 0 - 1. 

splitlast(T, T1, T2) :- append(T1, T2, T), length(T2, 1). 

updateinputs(_Config, _Mode, Hops, Dir, Moves, Inputs, NewConfig, _NewHops, NewInputs) :-
     % format("Updating history~n", []), display(Inputs), 
     update_history(Hops, Inputs, NewConfig, Inputs1), 
     update_pos(Inputs1, Dir, Moves, Inputs2), 

     update_steps(Inputs2, Inputs3), 
     NewInputs = Inputs3, 
     % display(NewInputs), nl, 
     true.

update_history(Hops, Inputs, NewConfig, NewInputs) :-
     member(history(History), Inputs), 
     member(pos(Current), Inputs), 
     member(now(Now), Inputs), append([s(Hops,Now,Current)], History, NewHist), 
     trim_to_size(NewHist, NewHistory), 
     delete(Inputs, history(History), Inps),
     delete(Inps, now(Now), Is), 
     append(Is, [now(NewConfig),history(NewHistory)], NewInputs). 

update_history(_Hops, Inputs, _NewConfig, Inputs) :-
     \+ (member(history(_History), Inputs), member(now(_Now), Inputs)), 
     true. 

trim_to_size(History, History) :- 
     length(History, Len), 
     history_memory(Size), Len =< Size. 
     
trim_to_size(History, Trimmed) :- 
     length(History, Len), 
     history_memory(Size), Len > Size,
     append(Trimmed, _, History), length(Trimmed, Size). 

history_memory(150). 

display_history([]).
display_history([s(Hops, Config, Pos)|Rest]) :-
      format("~d: ", [Hops]), display_config(Config), format("Pos: ~d~n", [Pos]), 
      display_history(Rest).

update_steps(Inputs, NewInputs) :-
      member(steps(S), Inputs), 
      delete(Inputs, steps(S), Inputs1), 
      S1 is S + 1,
      append([steps(S1)], Inputs1, NewInputs). 
update_steps(Inputs, Inputs) :-
      \+ member(steps(_S), Inputs). 

update_otters(Leaps, Inputs, NewInputs) :-
     member(otters(Otters), Inputs), NewOtters is Otters + 1, 
     member(ottersteps(OtterSteps), Inputs), NewOtterSteps is OtterSteps + Leaps, 
     delete(Inputs, otters(Otters), Inputs1), 
     delete(Inputs1, ottersteps(OtterSteps), Inputs2), 
     append([otters(NewOtters),ottersteps(NewOtterSteps)], Inputs2, NewInputs).
update_otters(Inputs, Inputs) :-
     \+ member(otters(_Otters), Inputs). 
update_otters(Inputs, Inputs) :-
     \+ member(ottersteps(_OtterSteps), Inputs). 

update_pos(Inputs, _Dir, Num, Inputs1) :-
      member(pos(Current), Inputs), delete(Inputs, pos(Current), Is),
      NewCurrent is Current + Num, 
      append([pos(NewCurrent)], Is, Inputs1). 
update_pos(Inputs, _Dir, _Num, Inputs) :-
      \+ member(pos(_Current), Inputs).

set_new_current(Current, l, Leap, NewCurrent) :- NewCurrent is Current - Leap.
set_new_current(Current, r, Leap, NewCurrent) :- NewCurrent is Current + Leap.

output_status(Config, Mode, Hops, Inputs, NewInputs, Used) :-
      yes_to_output(Config, Mode, Hops, Inputs, NewInputs),
      output_config(Config, Mode, Hops, NewInputs, Used), nl, ttyflush, 
      true, !. 
	    
output_status(Config, Mode, Hops, Inputs, Inputs, _Used) :-
      \+ yes_to_output(Config, Mode, Hops, Inputs, _NewInputs),
      true, !. 

yes_to_output(_Config, _Mode, _Hops, Inputs, Inputs) :-
      member(trace, Inputs),!.
yes_to_output(Config, _Mode, _Hops, Inputs, Inputs) :-
      member(watch(State), Inputs),
      Config = config(_Left, State, _Item, _Dir, _Right), !. 
yes_to_output(Config, _Mode, _Hops, Inputs, Inputs) :-
      member(watch(State, Item), Inputs),
      Config = config(_Left, State, Item, _Dir, _Right), !. 
yes_to_output(_Config, _Mode, Hops, Inputs, Inputs) :-
      member(trace(Number), Inputs),
      Hops >= Number, !.
yes_to_output(_Config, _Mode, Hops, Inputs, NewInputs) :-
      member(over(Number), Inputs), 
      Hops >= Number, !,
      delete(Inputs, over(Number), Inputs1),
      member(every(Every), Inputs1), 
      NewOver is Number + Every,
      append([over(NewOver)], Inputs1, NewInputs).  
 
output_config(Config, Mode, Hops, Inputs, Used) :-
      count_ones(Config, Ones), 
      Config = config(Left, State, Item, Dir, Right), 
      member(pos(Pos), Inputs), 
      prettyprint(State, Left, Item, Dir, Pos, Mode, Right, Hops, Ones, Used). 

display_trans([]).
display_trans([Trans|Rest]) :-
      Trans = trans(State, In, InDir, NewState, Out, OutDir, Steps),
      format("Trans: ~k ", [State]),
      display_string(In), 
      format(" ~k -> ~k ", [InDir,NewState]),
      display_string(Out), format(" ~k ~d~n", [OutDir,Steps]),
      display_trans(Rest). 

display_otters([]).
display_otters([Otter|Rest]) :-
       copy_term(Otter, TempOtter), 
       TempOtter = oo(InitConfig, FinalConfig, Leaps), 
       format("From: ", []), display_config(InitConfig), 
       format(" To: ", []),  display_config(FinalConfig), 
       display_leaps(Leaps), nl, 
       display_otters(Rest). 

display_leaps(Leaps) :-
       integer(Leaps), format(" in ~d steps.~n", [Leaps]).  
display_leaps(Leaps) :-
       \+ integer(Leaps), format(" in ", []), display(Leaps), format(" steps.~n", []). 

display_string([]).
display_string([A|Rest]) :-
       display(A), display_string(Rest).

count_ones(config(Left, _State, Item, _Dir, Right), Ones) :-
      count_ones1(Left, C1), count_ones1([Item], C2), count_ones1(Right, C3),
      Ones is C1 + C2 + C3. 
count_ones1([], 0).
count_ones1([Item|Rest], Count) :-
      is_blank(Item), !, 
      count_ones1(Rest, Count).
count_ones1([Item|Rest], Count) :-
      is_input(Item), 
      count_ones1(Rest, Count1),
      Count is Count1 + 1. 
count_ones1([grp(I,N)|Rest], Count) :-
      count_ones1(I, C), 
      count_ones1(Rest, Count1),
      Count is Count1 + N*C. 

prettyprint(State, Left, Item, Dir, Pos, Mode, Right, Hops, Ones, Used) :-
	length(Left,L), setblanks(Pos, Mode, L, Blanks), 
	reverse(Left, PL), % invert(PL, PPL), 
	leftprint(Mode, Blanks, PL, State, Item, Dir, Right), 
	format(" ~k     Hops: ~d Ones: ~d Pos: ~d ", [Used,Hops,Ones,Pos]), 
	true.

setblanks(Pos, Type, L, Blanks) :-
	member(Type, [naive]), 
	padding(Pad), 
	Blanks is Pad + Pos - L.
setblanks(_Pos, Type, _L, 0) :-
	\+ member(Type, [naive]).

leftprint(_, Blanks, _PL, _State, _Item, _, Right) :-
	Blanks < 0, format("{too long on the left} ", []), pprint(Right).
leftprint(naive, Blanks, PL, State, Item, _, Right) :- 
	Blanks >= 0, nblanks(Blanks), pprint(PL), format("{~k}",[State]), pprint([Item]), pprint(Right).
leftprint(Mode, Blanks, PL, State, Item, Dir, Right) :- 
        member(Mode, [macro(_K), adapt]), 
	Blanks >= 0, nblanks(Blanks), pprint(PL), format("{~k(",[State]), pprint([Item]), format(")~k}", [Dir]), pprint(Right).

padding(0). 
nblanks(N) :- N < 0, !, format("No space! ", []).
nblanks(0) :- !.
nblanks(N) :- N > 0, !, format(" ", []), N1 is N - 1, nblanks(N1). 

pprint([]). 
pprint([0|Rest]) :- !, format("0", []), pprint(Rest).
pprint([1|Rest]) :- !, format("1", []), pprint(Rest).
pprint([2|Rest]) :- !, format("2", []), pprint(Rest).
pprint([3|Rest]) :- !, format("3", []), pprint(Rest).
pprint([4|Rest]) :- !, format("4", []), pprint(Rest).
pprint([5|Rest]) :- !, format("5", []), pprint(Rest).
pprint([6|Rest]) :- !, format("6", []), pprint(Rest).
pprint([7|Rest]) :- !, format("7", []), pprint(Rest).

pprint([grp(T,C)|Rest]) :-
 	var(C), 
 	T = [_|_], !,
 	format("{",[]), 
 	pchars(T),
	format("}^(",[]), ttyflush, display(C), format(")", []), 
	pprint(Rest). 
pprint([grp(T,C)|Rest]) :-
 	% var(C), 
        nonvar(C), 
 	C = v(_V,Min,_,_,_), integer(Min), 
 	T = [_|_], !,
 	format("{",[]), 
 	pchars(T), 
	% format("}^(~k)",[C]), 
	% format("}^(~k)",[V]), 
	format("}^_[~d]",[Min]), 
	pprint(Rest). 
pprint([grp(T,C)|Rest]) :-
	% var(C), 
        nonvar(C), 
	C = v(_V,Min,_,_,_), \+ integer(Min), % var(Min), 
	T = [_|_], !,
	format("{",[]), 
	pchars(T), 
	% format("}^(~k)",[C]), 
	% format("}^(~k)",[V]), 
	format("}^_[~k]",[Min]), 
	pprint(Rest). 
pprint([grp(T,C)|Rest]) :-
        \+ integer(C), \+ (C = v(_V,_,_,_,_)),
        format("{",[]), 
	pchars(T), 
	format("}^_[", []),
        display(C),
        format("]",[]),
        pprint(Rest). 
pprint([grp(T,C)|Rest]) :-
	T = [_|_], % T is a list
	integer(C), 
	C > 1, !, 
	format("{",[]), 
	pchars(T), ttyflush, 
	print_exponent(C), !, 
	pprint(Rest). 
pprint([grp(T,C)|Rest]) :-
	T = [_|_], % T is a list
	integer(C),  
	C = 1, !, 
	pchars(T), 
	pprint(Rest). 
pprint([grp(T,C)|Rest]) :-
	T = [_|_], % T is a list
	integer(C), 
	C = 0, !, 
	pprint(Rest). 

pprint([grp(T,C,Visit)|Rest]) :-
 	% var(C), 
 	C = v(_V,Min,_,_,_), integer(Min), 
 	T = [_|_], !,
 	format("{",[]), 
 	pchars(T), 
	% format("}^(~k)",[C]), 
	% format("}^(~k)",[V]), 
	format("}^_[~d]{~d}",[Min,Visit]), 
	pprint(Rest). 
pprint([grp(T,C,Visit)|Rest]) :-
	% var(C), 
	C = v(_V,Min,_,_,_), var(Min), 
	T = [_|_], !,
	format("{",[]), 
	pchars(T), 
	% format("}^(~k)",[C]), 
	% format("}^(~k)",[V]), 
	format("}^_[~k]{~d}",[Min,Visit]), 
	pprint(Rest). 
pprint([grp(T,C,Visit)|Rest]) :-
	T = [_|_], % T is a list
	integer(C), 
	C > 1, !, 
	format("{",[]), 
	pchars(T), 
	format("}^(~d){~d}",[C,Visit]), 
	pprint(Rest). 
pprint([grp(T,C,Visit)|Rest]) :-
	T = [_|_], % T is a list
	integer(C), 
	C = 1, !, 
	pchars(T), format("{~d}", [Visit]), 
	pprint(Rest). 
pprint([grp(T,C,_Visit)|Rest]) :-
	T = [_|_], % T is a list
	integer(C), 
	C = 0, !, 
	pprint(Rest). 

print_exponent(N) :- max_print(Max), N =< Max, format("}^(~d)",[N]).
print_exponent(N) :- max_print(Max), N > Max, big_number_of_digits(N,  Number), format("}^(digits(~d))",[Number]), !.

print_bignum(N) :- print_bignum1(external, N).

print_bignum1(_,N) :- max_print(Max), N =< Max, format("~d", [N]). 
print_bignum1(external, N) :- max_print(Max), N > Max, big_number_of_digits(N, Number), format("(~d digits)", [Number]), !.
print_bignum1(internal, N) :- max_print(Max), N > Max, big_number_of_digits(N, Number), format("digits(~d)", [Number]), !.

max_print(999999999).

big_number_of_digits(N, M) :-
      X is 10 ** 80000, N >= X, 
      Number1 is N // X,
      number_of_digits(Number1, 80001, M). 

big_number_of_digits(N, M) :-
      X is 10 ** 60000, N >= X, 
      Y is 10 ** 80000, N < Y, !, 
      Number1 is N // X,
      number_of_digits(Number1, 60001, M). 

big_number_of_digits(N, M) :-
      X is 10 ** 40000, N >= X, 
      Y is 10 ** 60000, N < Y, !, 
      Number1 is N // X,
      number_of_digits(Number1, 40001, M). 

big_number_of_digits(N, M) :-
      X is 10 ** 20000, N >= X, 
      Y is 10 ** 40000, N < Y, !, 
      Number1 is N // X,
      number_of_digits(Number1, 20001, M). 

big_number_of_digits(N, M) :-
      X is 10 ** 20000, N < X, !,
      number_of_digits(N, 1, M). 

number_of_digits(N, M, M) :-
      N >= 0, N < 10.
number_of_digits(N, M, Number) :-
      N >= 10, 
      NewN is N // 10, 
      NewM is M + 1, 
      number_of_digits(NewN, NewM, Number). 
number_of_digits(N, M, Number) :-
      N < 0, !, 
      NewN is 0 - N,
      number_of_digits(NewN, M, Number).

pchars([]). 
pchars([0|Rest]) :- !, format("0",[]), pchars(Rest). 
pchars([1|Rest]) :- !, format("1",[]), pchars(Rest). 
pchars([2|Rest]) :- !, format("2",[]), pchars(Rest). 
pchars([3|Rest]) :- !, format("3",[]), pchars(Rest). 
pchars([4|Rest]) :- !, format("4",[]), pchars(Rest). 
pchars([5|Rest]) :- !, format("5",[]), pchars(Rest). 

% Machine language predicates. 

blank_symbol(0). % default
halt_state(z). % default. % Changed from h to fit in with Marxen ... 
looping_state(loop). % default. 

% Book-keeping
decrement_exponent(grp(I,Number), grp(I,NewNumber)) :-
	integer(Number), Number > 1, NewNumber is Number - 1.
decrement_exponent(grp(I,Number), grp(I,NewNumber)) :-
	variable(Number), decrement_variable(Number, NewNumber).

variable(v(_,_,_,_,_)). % Internal representation of variables for compress adapt form.
value(v(_Name,Value,_,_,_), Value). 
decrement_variable(v(Name,Value,A3,A4,A5), v(Name,V1,A3,A4,A5)) :- V1 is Value - 1.
minimum_compress_size(6). 

input([grp(In,_)|_], In) :- !. 
input([In|_], In) :- is_input(In), !. 

is_state(a).
is_state(b).
is_state(c).
is_state(d).
is_state(e).
is_state(f).
is_state(g).
is_state(h).
is_input(0).
is_input(1).
is_input(2).
is_input(3).
is_input(4).
is_input(5).
is_input(6).
is_input(7).

is_tape([]).
is_tape([I|Rest]) :- is_input(I), is_tape(Rest). 

blanks(I, []) :- blank_symbol(I). 
blanks(K, [I|Rest]) :- blank_symbol(I), K > 0, K1 is K-1, blanks(K1, Rest).

number_of_states(machine(M, _, _), N) :-
       states(M, Ss), length(Ss, N). 
number_of_states(M, N) :-
       M \== machine(_,_,_),
       states(M, Ss), length(Ss, N). 

states(M, States) :- states1(M, [], States). 
states1([], States, States).
states1([t(S,_,_,_,_)|Rest], StatesSoFar, States) :-
	add(S, StatesSoFar, NewSoFar),
	states1(Rest, NewSoFar, States).
add(Item, List, List) :-
	member(Item, List). 
add(Item, List, NewList) :-
	\+ member(Item, List), append(List, [Item], NewList). 

number_of_symbols(machine(M, _, _), N) :-
       symbols(M, Ss), length(Ss, N). 
number_of_symbols(M, N) :-
       M \== machine(_,_,_),
       symbols(M, Ss), length(Ss, N). 

symbols(M, Symbols) :- symbols1(M, [], Symbols). 
symbols1([], Symbols, Symbols).
symbols1([t(_,I,O,_,_)|Rest], SymbolsSoFar, Symbols) :-
	add(I, SymbolsSoFar, NewSoFar1),
	add(O, NewSoFar1, NewSoFar2),
        symbols1(Rest, NewSoFar2, Symbols). 

number_of_dirs(machine(M, _, _), N) :-
       dirs(M, Ss), length(Ss, N). 
number_of_dirs(M, N) :-
       M \== machine(_,_,_),
       dirs(M, Ss), length(Ss, N). 
dirs(M, Symbols) :- dirs1(M, [], Symbols). 
dirs1([], Dirs, Dirs).
dirs1([t(_,_,_,Dir,_)|Rest], DirsSoFar, Dirs) :-
	add(Dir, DirsSoFar, NewSoFar),
        dirs1(Rest, NewSoFar, Dirs). 

bound_max(1000000). 
maxbound(B, B) :-  bound_max(Bound), B =< Bound, !. 
maxbound(B, Bound) :- bound_max(Bound), B > Bound, !. %% Do not run for more than this many steps looking for a transition! May need to change this value ... 

% Removes Item from List leaving NewList.
% Fails if Item is not in List. 
remove(Item, List, NewList) :- member(Item, List), delete(List, Item, NewList). 

% There is a path in machine M of length N from S to NS
path(S,NS,N,M,[t(S,I,O,D,NS)]) :-
      N is 1, remove(t(S,I,O,D,NS), M, _RestM). 
path(S,NS,N,M,[t(S,I,O,D,S1)|Path]) :-
      N > 1, remove(t(S,I,O,D,S1), M, RestM), 
      N1 is N - 1, 
      path(S1,NS,N1,RestM,Path).
      
% There is a simple path in machine M of length N from S to NS.
% This means that S does not appear in the interal part of the path. 
simple_path(S,NS,N,M,Path) :- simple_path1(S, S,NS,N,M,Path).
simple_path1(_Source, S,NS,N,M,[t(S,I,O,D,NS)]) :-
      N is 1, remove(t(S,I,O,D,NS), M, _RestM). 
simple_path1(Source, S,NS,N,M,[t(S,I,O,D,S1)|Path]) :-
      N > 1, remove(t(S,I,O,D,S1), M, RestM), 
      N1 is N - 1, 
      Source \== S1, 
      simple_path1(Source,S1,NS,N1,RestM,Path).

% There is a cycle in machine M of length N from S to S
cycle(S,N,M,Path) :- path(S,S,N,M,Path). 

% There is a path in machine M of length somewhere between 1 and size(M) from S to NS
some_path(S,NS,M,Path) :-
     dimension(M,Size), 
     between(1,Size,N),
     path(S,NS,N,M,Path).

% There is a simple path in machine M of length somewhere between 1 and size(M) from S to NS
some_simple_path(S,NS,M,Path) :-
     dimension(M,Size), 
     between(1,Size,N),
     simple_path(S,NS,N,M,Path).

% Size is the size of M. One day this will include direction as well. 
dimension(M, Size) :-
     number_of_states(M,States), 
     number_of_symbols(M,Symbols), 
     Size is States * Symbols, !. 
     
member_num(X, List, N) :- member_num0(X, 1, List, N).  
member_num0(X, NumSoFar, [X|_], NumSoFar). 
member_num0(X, NumSoFar, [Y|Rest], N) :-
      X \== Y,
      NewNumSoFar is NumSoFar + 1, 
      member_num0(X, NewNumSoFar, Rest, N). 


convert_machine(M, NewM) :-
      number_of_symbols(M, Symbols), 
      sortmachine(Symbols, a,0,[],M,SortM),
      convert_machine1(SortM, [], NewM). % NewM is a string representing the Marxen representation of the machine.
      
sortmachine(_, _State, _Input, Done, [], NewM) :- reverse(Done, NewM),!. 
sortmachine(Symbols, State, Input, Done, Todo, Result) :-
	length(Todo, T), T > 0,
        % format("Looking for ~k ~d ~n", [State, Input]), ttyflush, 
	member(t(State, Input, Output, Dir, NewState), Todo), !, 
        % format("Found ~k ~d ~n", [State, Input]), ttyflush, 
	delete(Todo, t(State, Input, Output, Dir, NewState), Rest),
	increment(Symbols, State, Input, NS, NI), !, 
	% format("Calling sortmachine with ", []), write(NS), write(NI), write(Rest), nl, ttyflush, 
	sortmachine(Symbols, NS, NI, [t(State, Input, Output, Dir, NewState)|Done], Rest, Result).

sortmachine(Symbols, State, Input, Done, Todo, Result) :-
	length(Todo, T), T > 0,
        % format("Looking for ~k ~d ~n", [State, Input]), 
	\+ member(t(State, Input, _Output, _Dir, _NewState), Todo), !, 
        % format("Not found ~n", []), 
	increment(Symbols, State, Input, NS, NI), !,
	sortmachine(Symbols, NS, NI, Done, Todo, Result).

% increment(_, _, S, 0, S, 1).
increment(Symbols, State, Input, State, NewInput) :-
      Input < Symbols - 1, NewInput is Input + 1. 
increment(Symbols, State, Input, NewState, 0) :-
      Input >=  Symbols - 1, next_state(State, NewState). 

next_state(a,b). 
next_state(b,c).
next_state(c,d).
next_state(d,e).
next_state(e,f).
next_state(f,g).
next_state(g,h).
next_state(h,i).

convert_machine1([], SoFarNewM, SoFarNewM).
convert_machine1([t(S,I,O,D,NS)|Rest], SoFarNewM, NewM) :-
	atom_codes(O, NewO), 
	atom_codes(D, NewD),
	atom_codes(NS, NewNS), 
	append(NewO,NewD,Temp), append(Temp, NewNS, NewTrans), 
	append(SoFarNewM, NewTrans, NewSoFarNewM),
	convert_machine1(Rest, NewSoFarNewM, NewM). 

output_row(Symbols) :-output_row1(0, Symbols).
output_row1(Current, Final) :- Current >= Final. 
output_row1(Current, Final) :- Current < Final, format("<td><a class=heading>~d</a>", [Current]), NewCurrent is Current + 1, output_row1(NewCurrent, Final). 
	
output_html_table(Symbols, M) :-
	sortmachine(Symbols, a, 0, [], M, SortM),
       format("<table border=1><tr><td>&nbsp;", []), 
	output_row(Symbols), 
	output_html_table1(SortM).

output_html_table1([]). 
output_html_table1([t(S,I,O,D,NS)|Rest]) :-
	format("<tr><td><a class=heading>~k</a>", [S]), 
	output_html_table2(S,[t(S,I,O,D,NS)|Rest]).
	
output_html_table2(S, [t(S,I,O,D,NS)|Rest]) :-
	format("<td><a>~k ~k ~k</a>", [O, D, NS]), 
	output_html_table2(S, Rest). 

output_html_table2(State, [t(S,I,O,D,NS)|Rest]) :-
	State \== S, % Finished this row
	format("~n"),
	output_html_table1([t(S,I,O,D,NS)|Rest]). 
	
output_html_table2(_State, []) :-
	% Finished last row
	format("~n</table>~n"). 


