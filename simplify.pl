modulus_exponents(Config, Mods) :- setof(Mod, contains_mod(Config, Mod), Mods). 

contains_mod(Term, SubTerm) :- sub_term(SubTerm, Term), nonvar(SubTerm), SubTerm = _Z mod N, integer(N), N > 0. 

find_mod(Mods, _Progs, _Regs, Exponents, Names, Mods) :-
    member(mod(X,_N,_V), Mods), sub_term(S, Exponents), var(S), term_string(S, String, [variable_names(Names)]), String == X, !. % There is a compatible constraint, ie some variable has an existing constraint on it. 
find_mod(Mods, Progs, Regs, Exponents, Names, NewMods) :-
    % No compatible constraint
    sub_term(S, Exponents), var(S), term_string(S, Var, [variable_names(Names)]), init_value(Var, Progs, Regs, V1), member(_Exp mod N, Exponents), V is V1 mod N,
    append(Mods, [mod(Var,N,V)], NewMods). 
    
init_value("X1", Progs, _, V1) :- Progs = [V1|_],!. % Ugh!
init_value("X2", Progs, _, V1) :- append(P1, P2, Progs), length(P1, 1), P2 = [V1|_], !.
init_value("X3", Progs, _, V1) :- append(P1, P2, Progs), length(P1, 2), P2 = [V1|_].
init_value("Y1", _, Regs, V1) :-  Regs = [V1|_], !. % Ugh!
init_value("Y2", _, Regs, V1) :-  append(R1, R2, Regs), length(R1, 1), R2 = [V1|_], !.
init_value("Y3", _, Regs, V1) :-  append(R1, R2, Regs), length(R1, 2), R2 = [V1|_].

extract_exponents([], SoFar, SoFar).
extract_exponents([Item|Rest], SoFar, Result) :- Item = grp(_I, N), ground(N), extract_exponents(Rest, SoFar, Result). 
extract_exponents([Item|Rest], SoFar, Result) :- Item = grp(_I, N), variable(N), extract_mods(N, Mods), append(SoFar, Mods, NewSoFar), extract_exponents(Rest, NewSoFar, Result). 
 
extract_mods(Exp, []) :- ground(Exp).
extract_mods(Exp, []) :- var(Exp).
extract_mods(Exp, [Z mod N]) :- \+ground(Exp), \+var(Exp), Exp = Z mod N.
extract_mods(Exp, [Z mod N]) :- \+ground(Exp), \+var(Exp), Exp = (Z mod N).
extract_mods(Exp, [Z mod N]) :- \+ground(Exp), \+var(Exp), Exp = X + _Y, extract_mods(X, [Z mod N]).
extract_mods(Exp, [Z mod N]) :- \+ground(Exp), \+var(Exp), Exp = _X + Y, extract_mods(Y, [Z mod N]). 
extract_mods(Exp, [Z mod N]) :- \+ground(Exp), \+var(Exp), Exp = X - _Y, extract_mods(X, [Z mod N]).
extract_mods(Exp, [Z mod N]) :- \+ground(Exp), \+var(Exp), Exp = _X - Y, extract_mods(Y, [Z mod N]). 
extract_mods(Exp, [Z mod N]) :- \+ground(Exp), \+var(Exp), Exp = _Y + Z mod N.
extract_mods(Exp, [Z mod N]) :- \+ground(Exp), \+var(Exp), Exp = _Y - Z mod N.
extract_mods(Exp, [Z mod N]) :- \+ground(Exp), \+var(Exp), Exp = _X - _Y + Z mod N.
extract_mods(Exp, [Z mod N]) :- \+ground(Exp), \+var(Exp), Exp = _X + _Y - Z mod N.
extract_mods(Exp, []) :- \+ground(Exp), \+var(Exp). % Otherwise we can't find any mods ... 

rewrite_modulus([], Config, Hops, Pos, Moves, _, Config, Hops, Pos, Moves).
rewrite_modulus(Mods, Config, Hops, Pos, Moves, _, Config, Hops, Pos, Moves) :-  Mods \== [], \+ compatible_moduli(Mods). 
rewrite_modulus(Mods, Config, Hops, Pos, Moves, Names, NewConfig, NewHops, NewPos, NewMoves) :-
    Mods \== [], compatible_moduli(Mods), % Either only one or all the same modulus.
    rewrite_modulus1(Mods, Config, Hops, Pos, Moves, Names, NewConfig, NewHops, NewPos, NewMoves).

rewrite_modulus1([], Config, Hops, Pos, Moves, _, Config, Hops, Pos, Moves).
rewrite_modulus1(Mods, Config, Hops, Pos, Moves, Names, NewConfig, NewHops, NewPos, NewMoves) :-
    Mods = [Mod|RestMods], 
    Config = config(Left, State, Item, Dir, Right),
    rewrite_expressions_tape(Mod, Names, Left, NewLeft1),
    rewrite_expressions_tape(Mod, Names, [Item], [NewItem1]),
    rewrite_expressions_tape(Mod, Names, Right, NewRight1),

    NConfig1 = config(NewLeft1, State, NewItem1, Dir, NewRight1), reconfigure(NConfig1, NewConfig1), % Need to potentially reconfigure here ... 
    rewrite_expression(Mod, Names, Hops, NewHops1),
    rewrite_expression(Mod, Names, Pos, NewPos1),
    rewrite_expression(Mod, Names, Moves, NewMoves1),

    rewrite_modulus1(RestMods, NewConfig1, NewHops1, NewPos1, NewMoves1, Names, NewConfig, NewHops, NewPos, NewMoves).

compatible_moduli(Mods) :- length(Mods, 1).
compatible_moduli(Mods) :- length(Mods, L), L > 1, Mods = [mod(_, N, _)|RestMods], compatible_moduli(RestMods, N).
compatible_moduli([], _).
compatible_moduli([mod(_,N,_)|Rest], N) :- compatible_moduli(Rest, N). 

rewrite_expressions_tape(_, _, [], []).
rewrite_expressions_tape(Mod, Names, [Item|Rest], [NewItem|Result]) :-
    Item = grp(I,Exp), rewrite_expression(Mod, Names, Exp, NewExp),
    NewItem = grp(I, NewExp), 
    rewrite_expressions_tape(Mod, Names, Rest, Result).

% rewrite Exp using Var is Value mod N
rewrite_expression(Mod, Names, Exp, NewExp) :-
    rewrite_expression1(Mod, Exp, Exp1),    % First re-write K*((Expression)//N) where K is a multiple of N as (K//N)*(Expression - Expression mod N )
    rewrite_expression2(Mod, Names, Exp1, Exp2), % Then replace all occurrences of Expression mod N with the value from Var mod N being Value
    simplify_exp2(Exp2, NewExp). % and simplify, just to be sure. 

rewrite_expression1(_, Exp, Exp) :- ground(Exp); var(Exp).
rewrite_expression1(Mod, Exp, NewExp) :- \+ ground(Exp), \+ var(Exp), Exp = X + Y, !, rewrite_expression1(Mod, X, NewX), rewrite_expression1(Mod, Y, NewY), NewExp = NewX + NewY.
rewrite_expression1(Mod, Exp, NewExp) :- \+ ground(Exp), \+ var(Exp), Exp = X - Y, !, rewrite_expression1(Mod, X, NewX), rewrite_expression1(Mod, Y, NewY), NewExp = NewX - NewY.
rewrite_expression1(mod(_Var,N,_Value), Exp, NewExp) :- \+ ground(Exp), \+ var(Exp), sub_term(T, Exp), nonvar(T), T = _//_, Exp = N*(SubExp // N), !, NewExp = SubExp - (SubExp mod N).
rewrite_expression1(mod(_Var,N,_Value), Exp, NewExp) :- \+ ground(Exp), \+ var(Exp), sub_term(T, Exp), nonvar(T), T = _//_, Exp = N*((SubExp // N) + M), ground(M), !, NewExp = SubExp + N*M - (SubExp mod N).
rewrite_expression1(mod(_Var,N,_Value), Exp, NewExp) :- \+ ground(Exp), \+ var(Exp), sub_term(T, Exp), nonvar(T), T = _//_, Exp = K*(SubExp // N), 0 is K mod N, !, K1 is K//N, NewExp = K1*(SubExp) - K1*(SubExp mod N).
rewrite_expression1(mod(_Var,N,_Value), Exp, NewExp) :- \+ ground(Exp), \+ var(Exp), sub_term(T, Exp), nonvar(T), T = _//_, Exp = K*((SubExp // N) + M), 0 is K mod N, ground(M), !, K1 is K//N, NewExp = K1*(SubExp) + K*M - K1*(SubExp mod N).
% rewrite_expression1(mod(_Var,N,_Value), Exp, NewExp) :- \+ ground(Exp), \+ var(Exp), sub_term(T, Exp), nonvar(T), T = _//_, Exp = K*(SubExp // N), K1 is K mod N, K1 \== 0, !, K2 = K/N, NewExp = K2*(SubExp) - K2*(SubExp mod N). To do this we need to be able to simplify things like 4*2/4. 
% rewrite_expression1(mod(_Var,N,_Value), Exp, NewExp) :- \+ ground(Exp), \+ var(Exp), sub_term(T, Exp), nonvar(T), T = _//_, Exp = K*((SubExp // N) + M), K1 is K mod N, ground(M), K1 \== 0, !, K2 = K/N, NewExp = K2*(SubExp) + K*M - K2*(SubExp mod N).
rewrite_expression1(mod(_Var,_N,_Value), Exp, NewExp) :- \+ ground(Exp), \+ var(Exp), !, NewExp = Exp. 

% rewrite_expression2(mod(_Var,N,_Value), _Names, Term, Term) :- \+ (sub_term(S, Term), \+var(S), S = _Exp mod M, 0 is N mod M). % S = Exp mod N, or S = Exp mod M where N is a multiple of M.
rewrite_expression2(mod(_Var,N,_Value), _Names, Term, Term) :- \+ contains_mod(Term, N, _), !. 
% rewrite_expression2(mod(Var,N,_Value), Names, Term, Term) :-  sub_term(S, Term), \+var(S), S = Exp mod M, 0 is N mod M, \+ contains_variable(Exp, Var, Names). % Check that Exp contains Var. If not, nothing to do.
rewrite_expression2(mod(Var,N,_Value), Names, Term, Term) :-  contains_mod(Term, N, Exp mod _M), \+ contains_variable(Exp, Var, Names), !. % Check that Exp contains Var. If not, nothing to do. 
rewrite_expression2(mod(Var,N,Value), Names, Term, NewTerm) :-
    % sub_term(S, Term), \+var(S), S = Exp mod M, 0 is N mod M, contains_variable(Exp, Var, Names), % We know we are good if we get to here ...
    contains_mod(Term, N, Exp mod M), % We know we are good if we get to here ...
    term_string(Exp mod M, ExpS, [variable_names(Names)]),
    number_string(Value, ValueS), 
    replace_string(Var, ValueS, ExpS, NewExpS), % Replace Var with ValueS in ExpS
    find_names(Names, NewExpS, [], SubNames1), % Find appropriate subset of Names (ie only those which occur in NewExpS). These names also have to be in the order they occur in the string (!!)
    term_string(NewExp, NewExpS, [variable_names(SubNames1)]), % Simplify new expression if possible. Will be ground due to assumption that Var is the only variable in Exp
    simplify_exp2(NewExp, NewSimpleExp),
    term_string(NewSimpleExp, NewExpString, [variable_names(Names)]),
    term_string(Term, TermString, [variable_names(Names)]), % Replace Exp mod N appropriately in Term
    replace_string(ExpS, NewExpString, TermString, NewTermS),
    find_names(Names, NewTermS, [], SubNames2),     % find appropriate subset of names (ie only those which occur in NewTermS)
    term_string(NewTerm, NewTermS, [variable_names(SubNames2)]).

% True if the term Term contains the variable named VarName. 
contains_variable(Term, VarName, Names) :- term_string(Term, TermString, [variable_names(Names)]), string_compose(_, VarName, _, TermString), variable_name(VarName).

contains_mod(Term, N, Exp mod M) :- sub_term(S, Term), \+var(S), S = Exp mod M, 0 is N mod M. % S = Exp mod N, or S = Exp mod M where N is a multiple of M. 

reasonable_config(config(Left, _State, Item, _Dir, Right)) :- append(Left, [Item], Temp), append(Temp, Right, Tapes), \+ unreasonable(Tapes).

unreasonable(Tapes) :- member(grp(_Item, N), Tapes), unreasonable_term(N), !. 
unreasonable_term(Term) :- \+ ground(Term), div_subterm(Term, _S), !. % sub_term(S, Term), \+ var(S), S = _ / _. % Can't handle terms like 5/3 in Monster 74. Need to check this and 76, 77, 79 out.
unreasonable_term(Term) :- \+ ground(Term), min_subterm(Term, _S), !. % sub_term(S, Term), \+ var(S), S = _ / _. % Can't handle terms like 5/3 in Monster 74. Need to check this and 76, 77, 79 out. 

simple_config(config(Left, _State, Item, _Dir, Right)) :- append(Left, [Item], Temp), append(Temp, Right, Tapes), \+ complex(Tapes).

complex(Tapes) :- member(grp(_Item, N), Tapes), complex_term(N), !.

complex_term(Term) :- \+ ground(Term), div_subterm(Term, _S), !.
complex_term(Term) :- \+ ground(Term), intdiv_subterm(Term, _S), !.
complex_term(Term) :- \+ ground(Term), min_subterm(Term, _S), !.
complex_term(Term) :- \+ ground(Term), mod_subterm(Term, _S), !. 

% Needs to recognise that N*(X+K)//N = X + K//N and that (N*X +K)//N = X + K//N. 
simplify_exp2(Exp, ExpSimple) :- ground(Exp), integer_safe(Exp), ExpSimple is Exp, !.
simplify_exp2(Exp, ExpSimple) :- 
	termify(Exp, Terms), classify_terms(Terms, Simple, Complex, Ground),
	reduce_simple(Simple, T1, none), 
	reduce_complex(Complex, 1, 1, T2, none),
	update_ground(T2, VarT2, GroundT2), append(Ground, GroundT2, NewGround), % To allow for ground outcomes from reduce_complex
	eval_ground(NewGround, Constant), % Constant may not be simply a number due to non-integer-safe operations (/).
	safe_eval(Constant, none, NewConstant), 
        compose_term(T1, VarT2, NewConstant, ExpSimple), !. 

compose_term(T1, [], C, C) :- integer(T1), T1 == 0. 
compose_term(T1, T2, C, Result) :- integer(T1), T1 == 0, length(T2, N), N > 0, T2 = [T|Rest], add_term(T, Rest, T3), add_nonzero(T3, C, Result).
compose_term(T1, T2, C, Result) :- \+integer(T1), add_term(T1, T2, T3), add_nonzero(T3, C, Result). 

termify(Exp, Terms) :- ground(Exp), integer_safe(Exp), !, Term is Exp, Terms = [Term].
termify(Exp, Terms) :- var(Exp), !, Terms = [Exp].
termify(Exp, Terms) :- Exp = X + Y, !, termify(X, Terms1), termify(Y, Terms2), append(Terms1, Terms2, Terms).
termify(Exp, Terms) :- Exp = -X, !, termify(X, Terms1), negify(Terms1, Terms). 
termify(Exp, Terms) :- Exp = X - Y, !, termify(X, Terms1), termify(Y, Terms2), negify(Terms2, Terms3), append(Terms1, Terms3, Terms). 
termify(Exp, Terms) :- Exp = X * Y, !, termify(X, Terms1), termify(Y, Terms2),     multiply_terms(Terms1, Terms2, [], Terms).
termify(Exp, Terms) :- Exp = X * (Y), !, termify(X, Terms1), termify(Y, Terms2),   multiply_terms(Terms1, Terms2, [], Terms).
termify(Exp, Terms) :- Exp = (X) * Y, !, termify(X, Terms1), termify(Y, Terms2),   multiply_terms(Terms1, Terms2, [], Terms).
termify(Exp, Terms) :- Exp = (X) * (Y), !, termify(X, Terms1), termify(Y, Terms2), multiply_terms(Terms1, Terms2, [], Terms).
termify(Exp, Terms) :- !, Terms = [Exp]. % Default

integer_safe(Exp) :- div_subterms(Exp, Terms), Terms == []. 

div_subterms(Exp, SubTerms) :- findall(S, div_subterm(Exp, S), SubTerms1), sort(SubTerms1, SubTerms). 

div_subterm(Exp, S) :- sub_term(S, Exp), \+ var(S), S = _ / _.
intdiv_subterm(Exp, S) :- sub_term(S, Exp), \+ var(S), S = _ // _.
min_subterm(Exp, S) :- sub_term(S, Exp), \+ var(S), S = min(_,_).
mod_subterm(Exp, S) :- sub_term(S, Exp), \+ var(S), S = _ mod _. 
    
negify([], []).
negify([T|Terms], [NT|Result]) :- NT = -T, negify(Terms, Result).

multiply_terms([], _, SoFar, SoFar).
multiply_terms([T1|Terms1], Terms2, SoFar, Result) :- multiply_all(T1, Terms2, [], Temp1), append(SoFar, Temp1, NewSoFar), multiply_terms(Terms1, Terms2, NewSoFar, Result).

multiply_all(_T, [], SoFar, SoFar).
multiply_all(T, [T1|Rest], SoFar, Result) :- ground(T), ground(T1), NewT is T*T1, append(SoFar, [NewT], NewSoFar), multiply_all(T, Rest, NewSoFar, Result).
multiply_all(T, [T1|Rest], SoFar, Result) :- ground(T), \+ ground(T1), T \== 1, T \== 0, !, append(SoFar, [T*T1], NewSoFar), multiply_all(T, Rest, NewSoFar, Result).
multiply_all(T, [T1|Rest], SoFar, Result) :- ground(T), \+ ground(T1), T == 1, append(SoFar, [T1], NewSoFar), multiply_all(T, Rest, NewSoFar, Result).
multiply_all(T, [T1|Rest], SoFar, Result) :- ground(T), \+ ground(T1), T == 0, append(SoFar, [0], NewSoFar), multiply_all(T, Rest, NewSoFar, Result). 
multiply_all(T, [T1|Rest], SoFar, Result) :- \+ ground(T), ground(T1), T1 \== 1, T1 \== 0, !, append(SoFar, [T1*T], NewSoFar), multiply_all(T, Rest, NewSoFar, Result).
multiply_all(T, [T1|Rest], SoFar, Result) :- \+ ground(T), ground(T1), T1 == 1, append(SoFar, [T], NewSoFar), multiply_all(T, Rest, NewSoFar, Result).
multiply_all(T, [T1|Rest], SoFar, Result) :- \+ ground(T), ground(T1), T1 == 0, append(SoFar, [0], NewSoFar), multiply_all(T, Rest, NewSoFar, Result).
multiply_all(T, [T1|Rest], SoFar, Result) :- \+ ground(T), \+ ground(T1), NewT = T*T1, append(SoFar, [NewT], NewSoFar), multiply_all(T, Rest, NewSoFar, Result). 

classify_terms([], [], [], []).
classify_terms([Term|Rest], Simple, Complex, [Term|Ground]) :- ground(Term), classify_terms(Rest, Simple, Complex, Ground). 
classify_terms([Term|Rest], [Term|Simple], Complex, Ground) :- \+ ground(Term), simple_term(Term), classify_terms(Rest, Simple, Complex, Ground). 
classify_terms([Term|Rest], Simple, [Term|Complex], Ground) :- \+ ground(Term), \+ simple_term(Term), classify_terms(Rest, Simple, Complex, Ground). 

update_ground([], [], []). 
update_ground([Term|Rest], Var, [Term|Ground]) :- ground(Term), update_ground(Rest, Var, Ground).
update_ground([Term|Rest], [Term|Var], Ground) :- \+ ground(Term), update_ground(Rest, Var, Ground). 

eval_ground(List, Term) :- divide_ground(List, Safe, Others), sum_list(Safe, Constant), maplist(simplify_ground, Others, NewList), gather_constants(NewList, Constant, Term).

divide_ground([], [], []).
divide_ground([Term|Rest], [Term|Safe], Others) :- integer_safe(Term), divide_ground(Rest, Safe, Others).
divide_ground([Term|Rest], Safe, [Term|Others]) :- \+ integer_safe(Term), divide_ground(Rest, Safe, Others).

simplify_ground_list([], []).
simplify_ground_list([Term|Rest], [Simple|Result]) :- simplify_ground(Term, Simple), simplify_ground_list(Rest, Result). 

gather_constants([], C, C).
gather_constants(List, C, NewTerm) :- length(List, 1), List = [Term], C == 0, NewTerm = Term. 
gather_constants(List, C, NewTerm) :- length(List, 1), List = [Term], C \== 0, NewTerm = Term + C. 
gather_constants(List, C, NewTerm) :- length(List, N), N > 1, C == 0, sum_terms(List, Sum), NewTerm = Sum.
gather_constants(List, C, NewTerm) :- length(List, N), N > 1, C \== 0, sum_terms(List, Sum), NewTerm = Sum + C. 

sum_terms(List, Sum) :- length(List, 1), List = [Term], Sum = Term.
sum_terms([Term|Rest], Sum) :- length([Term|Rest], N), N > 1, sum_terms(Rest, Sum1), Sum = Term + Sum1. 

simple_term(T) :- var(T), !.
simple_term(T) :- T = N*X, ground(N), simple_term(X), !.
simple_term(T) :- T = X*N, ground(N), simple_term(X), !.
simple_term(T) :- T =.. [-|S], S = [W], simple_term(W). % Could do more --- what about -(X+1)?? 

reduce_simple(S, T, N) :- term_variables(S, Vars), gather_variables(Vars, S, N, T).

% A complex term must contain mod, // or /. May also contain *. 
reduce_complex([], _, _, [], _).
reduce_complex([Term|Rest], Num, Denom, [NewTerm|Result], _) :-
    Term = Exp mod N, integer(N), N > 0,
    % termify(Exp, Terms), classify_terms(Terms, Simple, Complex, Ground),
    % reduce_simple(Simple, T1, N), reduce_complex(Complex, 1, T2, N), eval_ground(Ground, Constant),
    % % NewConstant is Constant mod N,
    % safe_eval(Constant, N, NewConstant), 
    % compose_term(T1, T2, NewConstant, T3),
    simplify_exp2(Exp, T3),
    % (T3 \== 0 -> NewTerm = T3 mod N; NewTerm = 0),
    build_term(Term, T3, N, Num, Denom, NewTerm),
    !, reduce_complex(Rest, 1, 1, Result, none), 
    true. 
%     reduce_complex(Rest, 1, 1, Result, none). 
reduce_complex([Term|Rest], Num, Denom, [NewTerm|Result], _) :-
    Term = -(Exp mod N), integer(N), N > 0, 
    % termify(Exp, Terms), classify_terms(Terms, Simple, Complex, Ground),
    % reduce_simple(Simple, T1, N), reduce_complex(Complex, T2, N), eval_ground(Ground, Constant),
    % % NewConstant is Constant mod N,
    % safe_eval(Constant, N, NewConstant), 
    % compose_term(T1, T2, NewConstant, T3),
    simplify_exp2(Exp, T3), 
    % ( \+ground(T3) -> NewTerm = -(T3 mod N); NewTerm is 0 -(T3 mod N)),
    build_term(Term, T3, N, Num, Denom, NewTerm), 
    !, reduce_complex(Rest, 1, 1, Result, none). 
% Want (2*X + 7)//2 to reduce to X + 7//2 = X + 3.
% Also want 2*(X+4)//2 to reduce to X+4 - (X+4) mod 2, and to X+4 when it is known that X+4 mod 2 = 0. Note that this needs to be interpreted as 2*((X+4)//2), as the Prolog version of 2*(X+4)//2 assumes that // is outermost. Check where exponents are multiplied ... 
reduce_complex([Term|Rest], Num, Denom, [NewTerm|Result], M) :-
    Term = (Exp)//N, 0 is Num mod N, !, NewNum is Num // N, Term1 = Exp - Exp mod N, reduce_complex([Term1|Rest], NewNum, Denom, [NewTerm|Result], M), 
    reduce_complex(Rest, 1, 1, Result, M).
reduce_complex([Term|Rest], Num, Denom, [NewTerm|Result], M) :-
    Term = (Exp)//N, !, termify(Exp, Ts), classify_terms(Ts, S, C, G), eval_ground(G, Constant), classify_ns(S, N, Num, S1, S2), compose_term_n(S1, S2, C, Constant, N, Num, Denom, NewTerm), % result is ((Num/Denom)*S1 + (Num/Denom)*(S2 + C + Constant)//N 
    reduce_complex(Rest, 1, 1, Result, M).
reduce_complex([Term|Rest],  Num,  Denom, [NewTerm|Result], M) :- ( Term = (Exp)/N; Term = Exp/N), integer(N), N \== 0, NewDenom is Denom * N, reduce_complex([Exp], Num, NewDenom, [NewTerm], none), !, reduce_complex(Rest, 1, 1, Result, M).
reduce_complex([Term|Rest], _Num, _Denom, [0|Result], none) :- Term = A * _B, ground(A), 0 is A, !, reduce_complex(Rest, 1, 1, Result, none). 
reduce_complex([Term|Rest], _Num, _Denom, [0|Result], none) :- Term = _A * B, ground(B), 0 is B, !, reduce_complex(Rest, 1, 1, Result, none). 
reduce_complex([Term|Rest],  Num,  Denom, [NewTerm|Result], none) :- Term = A * B, ground(A), NewNum is Num*A, reduce_complex([B], NewNum, Denom, [NewTerm], none), !, reduce_complex(Rest, 1, 1, Result, none). 
reduce_complex([Term|Rest],  Num,  Denom, [NewTerm|Result], none) :- Term = A * B, ground(B), NewNum is Num*B, reduce_complex([A], NewNum, Denom, [NewTerm], none), !, reduce_complex(Rest, 1, 1, Result, none).
reduce_complex([Term|Rest],  Num,  Denom, [NewTerm|Result], none) :- ( Term = (Exp)/N; Term = Exp/N), integer(N), N \== 0, NewDenom is Denom * N, reduce_complex([Exp], Num, NewDenom, [NewTerm], none), !, reduce_complex(Rest, 1, 1, Result, none).
reduce_complex([Term|Rest], _Num, _Denom, [0|Result], N) :- Term = A * _B, integer(N), N > 1, ground(A), 0 is A mod N, !, reduce_complex(Rest, 1, 1, Result, N). 
reduce_complex([Term|Rest], _Num, _Denom, [0|Result], N) :- Term = _A * B, integer(N), N > 1, ground(B), 0 is B mod N, !, reduce_complex(Rest, 1, 1, Result, N). 
reduce_complex([Term|Rest],  Num,  Denom, [NewTerm|Result], N) :- Term = A * B, integer(N), N > 1, ground(A), NewNum is Num*A, reduce_complex([B], NewNum, Denom, [NewTerm], none), !, reduce_complex(Rest, 1, 1, Result, N). 
reduce_complex([Term|Rest],  Num,  Denom, [NewTerm|Result], N) :- Term = A * B, integer(N), N > 1, ground(B), NewNum is Num*B, reduce_complex([A], NewNum, Denom, [NewTerm], none), !, reduce_complex(Rest, 1, 1, Result, N). 
reduce_complex([Term|Rest],  Num,  Denom, [NewTerm|Result], N) :- lowest_terms(Num, Denom, NewNum, NewDenom), assemble_term(Term, NewNum, NewDenom, NewTerm), reduce_complex(Rest, 1, 1, Result, N). % Otherwise ... 

% use assemble_term where possible?
build_term(Term, Exp, N, _Num, _Denom, NewTerm) :- Term = _ mod N, Exp == 0, !, NewTerm = 0.
build_term(Term, Exp, N,  Num, _Denom, NewTerm) :- Term = _ mod N, Exp \== 0, Num == 0, !, NewTerm = 0. 
build_term(Term, Exp, N,  Num,  Denom, NewTerm) :- Term = _ mod N, Exp \== 0, assemble_factor(Num, Denom, Factor), Factor  == 1, !, NewTerm = Exp mod N.
build_term(Term, Exp, N,  Num,  Denom, NewTerm) :- Term = _ mod N, Exp \== 0, assemble_factor(Num, Denom, Factor), Factor \== 1, !, NewTerm = Factor * (Exp mod N). 
build_term(Term, Exp, N,  Num,  Denom, NewTerm) :- Term = -(_ mod N), ground(Exp), !, assemble_factor(Num, Denom, Factor), NewTerm is 0 - Factor * (Exp mod N).
build_term(Term, Exp, N,  Num,  Denom, NewTerm) :- Term = -(_ mod N), \+ ground(Exp), assemble_factor(Num, Denom, Factor), Factor  == 1, !, NewTerm =  - (Exp mod N).
build_term(Term, Exp, N,  Num,  Denom, NewTerm) :- Term = -(_ mod N), \+ ground(Exp), assemble_factor(Num, Denom, Factor), Factor \== 1, !, NewTerm =  - Factor * (Exp mod N).
 
safe_eval(Expression, N, Value) :- ground(Expression), integer(N), integer_safe(Expression), !, Value is Expression mod N. 
safe_eval(Expression, N, Expression mod N) :- integer(N), !. 
safe_eval(Expression, none, Value) :- ground(Expression), integer_safe(Expression), !, Value is Expression. 
safe_eval(Expression, none, Expression).

lowest_terms(Num, Denom, NewNum, NewDenom) :- Common is gcd(Num, Denom), NewNum is Num/Common, NewDenom is Denom/Common.

assemble_term(_Term, Num, _Denom, NewTerm) :- Num == 0, !, NewTerm = 0. 
assemble_term( Term, Num,  Denom, NewTerm) :- assemble_factor(Num, Denom, Factor), Factor == 1, !, NewTerm = Term.
assemble_term( Term, Num,  Denom, NewTerm) :- assemble_factor(Num, Denom, Factor), !, NewTerm = Factor*Term. % Should we have a special case for Factor == -1?

assemble_factor(Num, _Denom, Factor) :- Num == 0, !, Factor = 0.
assemble_factor(Num,  Denom, Factor) :- lowest_terms(Num, Denom, NewNum, NewDenom), NewDenom == 1, !, Factor = NewNum.
assemble_factor(Num,  Denom, Factor) :- lowest_terms(Num, Denom, NewNum, NewDenom), NewDenom \== 1, !, lowest_terms(Num, Denom, NewNum, NewDenom), Factor = NewNum/NewDenom.

% Split Terms into multiples of N and rest
classify_ns([], _, _, [], []).
classify_ns([Term|Rest], N, Num, [T|TermsN], Others) :-     var(Term), 0 is Num mod N, !, set_kterm(Num, N, Term, T), classify_ns(Rest, N, Num, TermsN, Others).
classify_ns([Term|Rest], N, Num, TermsN, [Term|Others]) :-  var(Term), L is Num mod N, L \== 0, !, classify_ns(Rest, N, Num, TermsN, Others).
classify_ns([Term|Rest], N, Num, [T|TermsN], Others) :-  \+ var(Term), Term = K*X, integer(K), var(X), % Could be more general!!
							  0 is K mod N, !, set_kterm(K, N, X, T),  classify_ns(Rest, N, Num, TermsN, Others).
classify_ns([Term|Rest], N, Num, TermsN, [Term|Others]) :- classify_ns(Rest, N, Num, TermsN, Others).

set_kterm(K, N, T, Term) :-  1  is K // N, !, Term = T. % We know K mod N is 0, so ... 
set_kterm(K, N, T, Term) :-  K1 is K // N, !, Term = K1*T. 

compose_term_n_old([], S2, C, Constant, N, Num, Denom, Term) :-
    % No multiples found. So nothing to do except evaluate ground terms
    append(S2, C, Temp), append(Temp, [Constant], Terms1), lowest_terms(Num, Denom, NewNum, NewDenom), assemble_factor(NewNum, NewDenom, Factor), multiply_all(Factor, Terms1, [], Terms2), add_terms(Terms2, T1), Term = (T1)//N. 
compose_term_n_old(S1, [], [] , Constant, N, Num, Denom, Term) :-
    S1 \== [], % Some simplification happened in a simple case, so ...
    lowest_terms(Num, Denom, NewNum, NewDenom), assemble_factor(NewNum, NewDenom, Factor), 
    multiply_all(Factor, S1, [], NewS1), NewC is Constant // N, append(NewS1, [NewC], Terms), add_terms(Terms, Term).
compose_term_n_old(S1, S2, C, Constant, N, Num, Denom, Term) :-
    S1 \== [], \+ (S2 == [], C == []), % Some simplification happened in a not-so-simple case, so ... 
    % append(S2, C, Temp), append(Temp, [Constant], Terms1), multiply_all(Coeff, Terms1, [], Terms2), add_terms(Terms2, T), append(S1, [(T)//N], NewTerms), add_terms(NewTerms, Term).
    lowest_terms(Num, Denom, NewNum, NewDenom), assemble_factor(NewNum, NewDenom, Factor), 
    multiply_all(Factor, S1, [], NewS1), append(S2, C, Temp), append(Temp, [Constant], Terms1), multiply_all(Factor, Terms1, [], Terms2), add_terms(Terms2, T), append(NewS1, [(T)//N], NewTerms), add_terms(NewTerms, Term).

compose_term_n(S1, [], [], Constant, N, Num, Denom, Term) :-
    lowest_terms(Num, Denom, NewNum, NewDenom), assemble_factor(NewNum, NewDenom, Factor), multiply_all(Factor, S1, [], NewS1),
    Term2 is Constant // N, % Very simple case! 
    append(NewS1, [Term2], NewTerms),
    add_terms(NewTerms, Term). 
compose_term_n(S1, S2, C, Constant, N, Num, Denom, Term) :-
    \+ (S2 == [], C == []), 
    lowest_terms(Num, Denom, NewNum, NewDenom), assemble_factor(NewNum, NewDenom, Factor), multiply_all(Factor, S1, [], NewS1),
    % reduce_or_not(Num, Denom, N, S2, C, Constant, Term2),
    append(S2, C, Temp), append(Temp, [Constant], Terms1), add_terms(Terms1, Term2), 
    assemble_term(Term2 // N, Num, Denom, Term3),
    append(NewS1, [Term3], NewTerms),
    add_terms(NewTerms, Term). 

% reduce_or_not(Num, Denom, N, S, C, Constant, Term) :-
%    0 is Num mod N, !, % So we can eliminate //
%    K is Num // N, % Keep K an integer
%    append(S, C, Temp), append(Temp, [Constant], Terms1), add_terms(Terms1, Term2),
%    Term3 = Term2 - Term2 mod N, 
%    assemble_term(Term3, Num, Denom, Term).
%reduce_or_not(Num, Denom, N, S, C, Constant, Term) :-
%    K is Num mod N, K \== 0, !, % So we can't eliminate //
%    append(S, C, Temp), append(Temp, [Constant], Terms1), add_terms(Terms1, Term2), 
%    assemble_term(Term2 // N, Num, Denom, Term).

gather_variables([], _, _, 0).
gather_variables([V|Vars], S, N, Term) :-  length(Vars, 0), !,  gather_one(V, S, Vs, _Others), find_coefficient(Vs, N, C), set_term(C, V, Term).
gather_variables([V|Vars], S, N, Term) :-  gather_one(V, S, Vs, Others), find_coefficient(Vs, N, C), set_term(C, V, T1), gather_variables(Vars, Others, N, T2), Term = T1 + T2.  

gather_one(_V, [], [], []).
gather_one(V, [T|Vars], [T|Rest1], Rest2) :- sub_term(Var, T), var(Var), V == Var, !, gather_one(V, Vars, Rest1, Rest2).
% gather_one(V, [T|Vars], Rest1, [T|Rest2]) :- \+ (sub_term(Var, T), var(Var), V == Var), gather_one(V, Vars, Rest1, Rest2).
gather_one(V, [T|Vars], Rest1, [T|Rest2]) :- gather_one(V, Vars, Rest1, Rest2).

find_coefficient([], _, 0). 
find_coefficient([T|Rest], none, C) :- term_val(T, C1), find_coefficient(Rest, none, C2), sum_safe(C1, C2, C). 
find_coefficient([T|Rest], N, C) :-    integer(N), N > 1, term_val(T, C1), find_coefficient(Rest, N, C2), sum_safe_mod(C1, C2, N, C). 

sum_safe(C1, C2, C) :- integer_safe(C1+C2), C3 is C1, C4 is C2, C3 \== 0, C4 \== 0, !, C is C3+C4.
sum_safe(C1, C2, C) :- integer_safe(C1+C2), 0 is C1, C4 is C2, C4 \== 0, !, C is C4.
sum_safe(C1, C2, C) :- integer_safe(C1+C2), C3 is C1, 0 is C2, C3 \== 0, !, C is C3.
sum_safe(C1, C2, C) :- integer_safe(C1+C2), 0 is C1, 0 is C2, !, C = 0.
sum_safe(C1, C2, C) :- \+ integer_safe(C1+C2), integer_safe(C1), 0 is C1, !, C = C2.
sum_safe(C1, C2, C) :- \+ integer_safe(C1+C2), integer_safe(C1), C3 is C1, C3 \== 0, !, C = C2 + C3.
sum_safe(C1, C2, C) :- \+ integer_safe(C1+C2), integer_safe(C2), 0 is C2, !, C = C1.
sum_safe(C1, C2, C) :- \+ integer_safe(C1+C2), integer_safe(C2), C4 is C2, C4 \== 0, !, C = C1 + C4.

sum_safe_mod(C1, C2, N, C) :- integer_safe(C1+C2), C is (C1+C2) mod N.
sum_safe_mod(C1, C2, N, C) :- \+ integer_safe(C1+C2), C = (C1+C2) mod N. 

set_term(0, _, 0).
set_term(1, V, V).
set_term(-1, V, T) :- T = -V. 
set_term(N, V, T) :- \+ member(N, [-1,0,1]), T = N*V.

term_val(T, 1) :- var(T), !. 
term_val(T, -1) :- \+ var(T), T = -V, var(V), !.
term_val(T, -N2) :- \+ var(T), T = -V, \+ var(V), V = N*V1, term_val(V1, N1), integer(N), N \== 0, N \== 1, !, N2 is N*N1, !.
term_val(T,  N2) :- \+ var(T), T = -V, \+ var(V), V = N*_V1,                  integer(N), N == 0, !, N2 is 0, !.
term_val(T,  N2) :- \+ var(T), T = -V, \+ var(V), V = N*V1, term_val(V1, N1), integer(N), N == 1, !, N2 is N1, !.
term_val(T, -N2) :- \+ var(T), T = -V, \+ var(V), V = V1*N, term_val(V1, N1), integer(N), N \== 0, N \== 1, N2 is N*N1, !.
term_val(T,  N2) :- \+ var(T), T = -V, \+ var(V), V = _V1*N,                  integer(N), N == 0, N2 is 0, !.
term_val(T,  N2) :- \+ var(T), T = -V, \+ var(V), V = V1*N, term_val(V1, N1), integer(N), N == 1, N2 is N1, !.
term_val(T, -N2) :- \+ var(T), T = -V, \+ var(V), V = N*V1, term_val(V1, N1), \+ integer(N), N0 is N, integer(N0), N0 \== 0, N0 \== 1, N2 is N*N1.
term_val(T,  N2) :- \+ var(T), T = -V, \+ var(V), V = N*_V1,                  \+ integer(N), N0 is N, integer(N0), N0 == 0, N2 is 0.
term_val(T,  N2) :- \+ var(T), T = -V, \+ var(V), V = N*V1, term_val(V1, N1), \+ integer(N), N0 is N, integer(N0), N0 == 1, N2 is N1.
term_val(T, -N2) :- \+ var(T), T = -V, \+ var(V), V = N*V1, term_val(V1, N1), \+ integer(N), N0 is N, \+ integer(N0), N2 = N*N1.
term_val(T, -N2) :- \+ var(T), T = -V, \+ var(V), V = V1*N, term_val(V1, N1), \+ integer(N), N0 is N, integer(N0), N0 \== 0, N0 \== 1, N2 is N*N1.
term_val(T, -N2) :- \+ var(T), T = -V, \+ var(V), V = _V1*N,                  \+ integer(N), N0 is N, integer(N0), N0 == 0, N2 is 0.
term_val(T, -N2) :- \+ var(T), T = -V, \+ var(V), V = V1*N, term_val(V1, N1), \+ integer(N), N0 is N, integer(N0), N0 == 1, N2 is N1, !.
term_val(T, -N2) :- \+ var(T), T = -V, \+ var(V), V = V1*N, term_val(V1, N1), \+ integer(N), N0 is N, \+ integer(N0), N2 = N*N1, !.
term_val(T, N2) :-  \+ var(T), T = N*V, term_val(V, N1), integer(N), N0 is N1, integer(N0), N0 \== 0, N0 \== 1, N2 is N*N1, !.
term_val(T, N2) :-  \+ var(T), T = N*V, term_val(V, N1), integer(N), N0 is N1, integer(N0), N0 == 0, N2 is 0, !.
term_val(T, N2) :-  \+ var(T), T = N*V, term_val(V, N1), integer(N), N0 is N1, integer(N0), N0 == 1, N2 is N, !.
term_val(T, N2) :-  \+ var(T), T = N*V, term_val(V, N1), integer(N), N0 is N1, \+ integer(N0), N2 = N*N1, !.
term_val(T, N2) :-  \+ var(T), T = V*N, term_val(V, N1), integer(N), N0 is N1, integer(N0), N0 \== 0, N0 \== 1, N2 is N*N1, !.
term_val(T, N2) :-  \+ var(T), T = V*N, term_val(V, N1), integer(N), N0 is N1, integer(N0), N0 == 0, N2 is 0, !.
term_val(T, N2) :-  \+ var(T), T = V*N, term_val(V, N1), integer(N), N0 is N1, integer(N0), N0 == 1, N2 is N, !.
term_val(T, N2) :-  \+ var(T), T = V*N, term_val(V, N1), integer(N), N0 is N1, \+ integer(N0), N2 = N*N1, !.
term_val(T, N2) :-  \+ var(T), T = N*V, term_val(V, N1), \+ integer(N), N0 is N, integer(N0), N0 \== 0, N0 \== 1, N2 = N*N1, !.
term_val(T, N2) :-  \+ var(T), T = N*_V,                 \+ integer(N), N0 is N, integer(N0), N0 == 0, N2 is 0, !.
term_val(T, N2) :-  \+ var(T), T = N*V, term_val(V, N1), \+ integer(N), N0 is N, integer(N0), N0 == 1, N2 is N1, !.
term_val(T, N2) :-  \+ var(T), T = N*V, term_val(V, N1), \+ integer(N), N0 is N, \+ integer(N0), N1 \== 1, N2 = N*N1, !.
term_val(T, N2) :-  \+ var(T), T = N*V, term_val(V, N1), \+ integer(N), N0 is N, \+ integer(N0), N1 == 1, N2 = N, !.
term_val(T, N2) :-  \+ var(T), T = V*N, term_val(V, N1), \+ integer(N), N0 is N, integer(N0), N0 \== 0, N0 \== 1, N2 = N*N1, !.
term_val(T, N2) :-  \+ var(T), T = _V*N,                 \+ integer(N), N0 is N, integer(N0), N0 == 0, N2 is 0, !.
term_val(T, N2) :-  \+ var(T), T = V*N, term_val(V, N1), \+ integer(N), N0 is N, integer(N0), N0 == 1, N2 is N1, !.
term_val(T, N2) :-  \+ var(T), T = V*N, term_val(V, N1), \+ integer(N), N0 is N, \+ integer(N0), N1 \== 1, N2 = N*N1, !.
term_val(T, N2) :-  \+ var(T), T = V*N, term_val(V, N1), \+ integer(N), N0 is N, \+ integer(N0), N1 == 1, N2 = N, !.

add_terms([], 0).
add_terms([T|Rest], Result) :- add_term(T, Rest, Result). 
add_term(Term, [], Result) :- Result = Term. 
add_term(Term, [T|Rest], Result) :- T \== 0, Temp = Term + T,  add_term(Temp, Rest, NewTerm), Result = NewTerm.
add_term(Term, [T|Rest], Result) :- T == 0,  add_term(Term, Rest, NewTerm), Result = NewTerm. 

add_nonzero(Term, Constant, ExpSimple) :- Constant == 0, ExpSimple = Term.
add_nonzero(Term, Constant, ExpSimple) :- integer_safe(Constant), C is Constant, C > 0, ExpSimple = Term + C.
add_nonzero(Term, Constant, ExpSimple) :- integer_safe(Constant), C is Constant, C < 0, Neg is 0-C, ExpSimple = Term - Neg. 
add_nonzero(Term, Constant, ExpSimple) :- \+ integer_safe(Constant), ExpSimple = Term + Constant.

% Assumes Exp is ground. 
simplify_ground(Exp, NewExp) :- integer_safe(Exp), NewExp is Exp, !.
simplify_ground(Exp, NewExp) :- Exp = -(Exp1), simplify_ground(Exp1, Exp2), NewExp = -(Exp2). 
simplify_ground(Exp, NewExp) :- (Exp = A + B; Exp = (A+B)), integer_safe(B), 0 is B, !,    simplify_ground(A, NewExp).                     % So A must not be integer safe
simplify_ground(Exp, NewExp) :- (Exp = A + B; Exp = (A+B)), integer_safe(B), NewB is B, !, simplify_ground(A, NewA), NewExp = NewA + NewB. % So A must not be integer safe
simplify_ground(Exp, NewExp) :- (Exp = A + B; Exp = (A+B)), integer_safe(A), 0 is A, !,    simplify_ground(B, NewExp).                     % So B must not be integer safe
simplify_ground(Exp, NewExp) :- (Exp = A + B; Exp = (A+B)), integer_safe(A), NewA is A, !, simplify_ground(B, NewB), NewExp = NewB + NewA. % So B must not be integer safe
simplify_ground(Exp, NewExp) :- (Exp = A + B; Exp = (A+B)), simplify_ground(A, NewA),      simplify_ground(B, NewB), NewExp = NewA + NewB. % Neither A nor B integer safe
simplify_ground(Exp, NewExp) :- (Exp = A - B; Exp = (A-B)), integer_safe(B), 0 is B, !,    simplify_ground(A, NewExp).                     % So A must not be integer safe
simplify_ground(Exp, NewExp) :- (Exp = A - B; Exp = (A-B)), integer_safe(B), NewB is B, !, simplify_ground(A, NewA), NewExp = NewA - NewB. % So A must not be integer safe
simplify_ground(Exp, NewExp) :- (Exp = A - B; Exp = (A-B)), integer_safe(A), 0 is A, !,    simplify_ground(B, NewB), NewExp = - NewB.      % So B must not be integer safe
simplify_ground(Exp, NewExp) :- (Exp = A - B; Exp = (A-B)), integer_safe(A), NewA is A, !, simplify_ground(B, NewB), NewExp = NewA - NewB. % So B must not be integer safe
simplify_ground(Exp, NewExp) :- (Exp = A - B; Exp = (A-B)), simplify_ground(A, NewA),      simplify_ground(B, NewB), NewExp = NewA - NewB. % Neither A nor B integer safe
simplify_ground(Exp, Exp) :-    (Exp = _A/_B; Exp = (_A/_B)). % This is definitely not integer safe, so ... 
simplify_ground(Exp, NewExp) :- Exp = A mod B, integer_safe(B), N is B, simplify_ground(A, NewA), simplify_mod(NewA, N, NewExp).
simplify_ground(Exp, Exp) :-    Exp = _A mod B, \+ integer_safe(B). 
simplify_ground(Exp, 0) :-      (Exp = _A*B; Exp = (_A*B)), integer_safe(B), 0 is B, !.
simplify_ground(Exp, NewExp) :- (Exp = A*B; Exp = (A*B)), integer_safe(B), 1 is B,       simplify_ground(A, NewExp), !.                    % So A must not be integer 
simplify_ground(Exp, NewExp) :- (Exp = A*B; Exp = (A*B)), integer_safe(B), !, NewB is B, simplify_ground(A, NewA), NewExp = NewB*NewA, !.  % So A must not be integer safe. Should -1 be a special case? 
simplify_ground(Exp, 0) :-      (Exp = A*_B; Exp = (A*_B)), integer_safe(A), 0 is A, !. 
simplify_ground(Exp, NewExp) :- (Exp = A*B; Exp = (A*B)), integer_safe(A), 1 is A,       simplify_ground(B, NewExp), !.                    % So B must not be integer safe
simplify_ground(Exp, NewExp) :- (Exp = A*B; Exp = (A*B)), integer_safe(A), !, NewA is A, simplify_ground(B, NewB), NewExp = NewA*NewB, !.  % So B must not be integer safe. Should -1 be a special case? 
simplify_ground(Exp, NewExp) :- (Exp = A*B; Exp = (A*B)), simplify_ground(A, NewA),       simplify_ground(B, NewB), NewExp = NewA*NewB, !. % Neither A nor B integer safe
simplify_ground(Exp, Exp). 

% simplify Exp mod N, so Term + X is Term if X is a multiple of N etc.
simplify_mod(Exp, N, NewExp) :- integer_safe(Exp), NewExp is Exp mod N, !. 
simplify_mod(Exp, N, NewExp) :- Exp = A + B, integer_safe(A), 0 is A mod N, !, simplify_mod(B, N, Exp1), NewExp = Exp1 mod N.
simplify_mod(Exp, N, NewExp) :- Exp = A + B, integer_safe(A), K is A mod N,    simplify_mod(B, N, Exp1), NewExp = (Exp1 + K) mod N. 
simplify_mod(Exp, N, NewExp) :- Exp = A + B, integer_safe(B), 0 is B mod N, !, simplify_mod(A, N, Exp1), NewExp = Exp1 mod N.
simplify_mod(Exp, N, NewExp) :- Exp = A + B, integer_safe(B), K is B mod N,    simplify_mod(A, N, Exp1), NewExp = (Exp1 + K) mod N. 
simplify_mod(Exp, N, NewExp) :- Exp = _A + _B, NewExp = Exp mod N.
simplify_mod(Exp, N, NewExp) :- Exp = A - B, integer_safe(A), 0 is A mod N, !, simplify_mod(B, N, Exp1), NewExp = -(Exp1) mod N.
simplify_mod(Exp, N, NewExp) :- Exp = A - B, integer_safe(A), K is A mod N,    simplify_mod(B, N, Exp1), NewExp = (K- Exp1) mod N. 
simplify_mod(Exp, N, NewExp) :- Exp = A - B, integer_safe(B), 0 is B mod N, !, simplify_mod(A, N, Exp1), NewExp = Exp1 mod N.
simplify_mod(Exp, N, NewExp) :- Exp = A - B, integer_safe(B), K is B mod N,    simplify_mod(A, N, Exp1), NewExp = (Exp1 - K) mod N. 
simplify_mod(Exp, N, NewExp) :- Exp = _A - _B, NewExp = Exp mod N.
simplify_mod(Exp, N, NewExp) :- Exp = A*B, integer_safe(B), 1 is B mod N, !,   simplify_mod(A, N, NewA), NewExp = NewA mod N. 
simplify_mod(Exp, N, NewExp) :- Exp = A*B, integer_safe(B), NewB is B mod N,   simplify_mod(A, N, NewA), NewExp = NewB*NewA mod N . 
simplify_mod(Exp, N, NewExp) :- Exp = A*B, integer_safe(A), 1 is A, !,         simplify_mod(B, N, NewB), NewExp = NewB mod N. 
simplify_mod(Exp, N, NewExp) :- Exp = A*B, integer_safe(A), NewA is A,         simplify_mod(B, N, NewB), NewExp = NewA*NewB mod N.  
simplify_mod(Exp, N, NewExp) :- Exp = A*B, simplify_mod(A, N, NewA),           simplify_mod(B, N, NewB), NewExp = NewA*NewB mod N.
simplify_mod(Exp, N, Exp mod N). 

find_names(_Names, String, SoFar, SoFar) :-
    \+ first_var(_Var, String, _). % No first variable name in this string, so there is no variable name in the string ... 
find_names(Names, String, SoFar, SubNames) :-
    first_var(S, String, Rest), 
    atom_string(NS1, S), member(NS1=Var, Names), % So we have found a variable name in the string which is in Names
    add_to_list(NS1=Var, SoFar, NewSoFar),
    find_names(Names, Rest, NewSoFar, SubNames). 
find_names(Names, String, SoFar, SubNames) :- 
    first_var(S, String, Rest), 
    atom_string(NS1, S), \+ member(NS1=_Var, Names), % So we have found a variable name in the string which is in not Names ... probably shouldn't happen!
    find_names(Names, Rest, SoFar, SubNames). 

contains_var(S) :- string_compose(_, Var, _, S), variable_name(Var). 

% Var is the first variable name in String
first_var(Var, String, Rest) :- string_compose(S1, Var, Rest, String), variable_name(Var), \+ contains_var(S1). 

check_constraints(_, _, _, []).
check_constraints(InitConfig, FinalConfig, Names, Mods) :-
    Mods \== [],
    extract_vars(InitConfig, Mods, Names, InitVarList), extract_vars(FinalConfig, Mods, Names, FinalVarList), 
    \+ \+ compatible(Mods, InitVarList, FinalVarList, Names).

%extract_vars(Config, Mods, Names, List).
% Lists is the list of terms var(Var, Exp) where Exp needs to be compatible for variable Var. 
extract_vars(_, [], _, []).
extract_vars(Config, [mod(Var,_N,_Value)|Rest], Names, [var(Var, Expressions)|Result]) :- 
    expressions_from_config(Var, Config, Names, Expressions),
    extract_vars(Config, Rest, Names, Result). 

expressions_from_config(Var, Config,  Names, ExpList) :-
    Config = config(Left, _State, Item, _Dir, Right),
    expressions_from_tape(Var, Left, Names, ExpLeft),
    expressions_from_tape(Var, [Item], Names, ExpItem),
    expressions_from_tape(Var, Right, Names, ExpRight),
    append(ExpLeft, ExpItem, Temp), append(Temp, ExpRight, ExpList).

expressions_from_tape(_, [], _, []).
expressions_from_tape(Var, [Item|Rest], Names, [Exp|Result]) :-
    Item = grp(_, Exp), term_string(Exp, String, [variable_names(Names)]),
    string_compose(_, Var, _, String),
    expressions_from_tape(Var, Rest, Names, Result). 
expressions_from_tape(Var, [Item|Rest], Names, Result) :-
    Item = grp(_, Exp), term_string(Exp, String, [variable_names(Names)]),
    \+ string_compose(_, Var, _, String),
    expressions_from_tape(Var, Rest, Names, Result). 

compatible([], _, _,_). 
compatible([mod(Var,N,Value)|Rest], [var(Var,Expressions1)|InitVarList], [var(Var,Expressions2)|FinalVarList], Names) :-
    assignment(Var, N, Value, Expressions1, Names, Result1),
    assignment(Var, N, Value, Expressions2, Names, Result2),
    Result1 == Result2,
    compatible(Rest, InitVarList, FinalVarList, Names). 

% Expression has value Result when V is Value mod N
assignment(Var, N, Value, Expression, Names, Result) :-
    copy_term(f(Expression, Names), f(Exp1, Names1)),
    term_string(Exp1, String1, [variable_names(Names1)]), 
    replace_string(Var, Value, String1, NewString),
    find_names(Names1, NewString, [], SubNames),
    term_string(Term1, NewString, [variable_names(SubNames)]),
    check_results(Term1, N, Result). 

check_results(List, N, Result) :- length(List,1), List = [Exp], Result is Exp mod N.
check_results(List, N, Result) :- length(List,N), N > 1, List = [Exp|Rest], Result is Exp mod N, check_results(Rest, N, Result). 
    


