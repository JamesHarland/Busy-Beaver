:- op(1000, fx, timex).
timex G :- time(G).

% No need to worry about named machines for this case, as this is only used in search mode. 
find_trans(machine(M,_,_), M).
find_trans(M, M) :- M \= machine(_,_,_). 

machine(_, M, M) :- M = machine(_,_,_), !.
% In the cases below M is usually a list of transitions, but it can be the name of a special machine, ie riemann, zermelo, goldbach. 
machine(naive, M, machine(M, [], [])).
machine(macro(_K), M, machine(M, [], [])).
machine(flex, M, machine(M, [], [])).
machine(search, M, machine(M, [], [])).

blank_tape(config(Left, _State, Item, _Dir, Right)) :- is_blank(Item), all_blank(Left), all_blank(Right).

is_blank(I) :- blank_symbol(I).
is_blank(grp(I,N)) :- ground(N), all_blank(I). % Don't delete terms with variables in them ... 

check_blank(Tape, Tape) :- \+ all_blank(Tape). 
check_blank(Tape, []) :- all_blank(Tape). 

all_blank([]).
all_blank([I|Rest]) :- is_blank(I), all_blank(Rest). 

make_list(0, _, []).
make_list(N, Item, [Item|Rest]) :- N > 0, N1 is N-1, make_list(N1, Item, Rest).

expanded(I, L, SoFar, NewI) :- append(SoFar, I, NewI), length(NewI, N), N =< L. 
expanded(I, L, SoFar, NewI) :- append(SoFar, I, NewSoFar), length(NewSoFar, N), N =< L, expanded(I, L, NewSoFar, NewI). 

flatten_tape(_, [], []).
flatten_tape(right, [grp(I,N)|Rest], NewResult) :-
    flatten_tape_grp(I, N, Flat), flatten_tape(right, Rest, Result), append(Flat, Result, NewResult). 
flatten_tape(left, [grp(I,N)|Rest], NewResult) :-
    flatten_tape_grp(I, N, Flat), flatten_tape(left, Rest, Result), reverse(Flat, RevFlat), append(RevFlat, Result, NewResult). 
flatten_tape(Direction, [I|Rest], [I|Result]) :- is_input(I), flatten_tape(Direction, Rest, Result).
    
flatten_tape_grp(_I, N, []) :- integer(N), N == 0. 
flatten_tape_grp(I, N, I)   :- integer(N), N == 1. 
flatten_tape_grp(I, N, Result) :- integer(N), N > 1, N1 is N-1, flatten_tape_grp(I, N1, Rest), append(I, Rest, Result). 
flatten_tape_grp(I, N, [grp(I,N)]) :- variable(N).

list_values(0, _, []).
list_values(L, N, [N|List]) :- L > 0, L1 is L-1, list_values(L1, N, List). 

config_size(config(Left, _State, Item, _Dir, Right), Size) :-
    tape_size(Left, Size1), tape_size([Item], Size2), tape_size(Right, Size3),
    Size #= Size1 + Size2 + Size3. 

% 4-way version of string_concat, ie S = S1S2S3 
string_compose(S1, S2, S3, S) :- var(S), string_concat(S1, S2, T), string_concat(T, S3, S).
string_compose(S1, S2, S3, S) :- ground(S), string_concat(T, S3, S), string_concat(S1, S2, T).

% Replace all occurrences of S1 with S2 in S3 giving S4 (all strings)
replace_string(S1, _S2, S3, S3) :- \+ string_compose(_T1, S1, _T2, S3). 
replace_string(S1, S2, S3, S4) :- string_compose(T1, S1, T2, S3), string_compose(T1, S2, T2, Temp), replace_string(S1, S2, Temp, S4). 

opposite_direction(l,r). 
opposite_direction(r,l). 

symbols_list(Symbols, List, SymbolsList) :-
	symbols_list1(Symbols, List1),
	legal_list(List, List1, SymbolsList).

symbols_list1(2, [0,1]). 
symbols_list1(3, [0,1,2]). 
symbols_list1(4, [0,1,2,3]). 
symbols_list1(5, [0,1,2,3,4]). 
symbols_list1(6, [0,1,2,3,4,5]). 
symbols_list1(7, [0,1,2,3,4,5,6]). 
symbols_list1(8, [0,1,2,3,4,5,6,7]). 

symbol(N,X) :- symbols_list1(N, List), member(X, List). 

states_list(1, [a]). 
states_list(2, [a,b]). 
states_list(3, [a,b,c]). 
states_list(4, [a,b,c,d]).
states_list(5, [a,b,c,d,e]). 
states_list(6, [a,b,c,d,e,f]). 
states_list(7, [a,b,c,d,e,f,g]). 
states_list(8, [a,b,c,d,e,f,g,h]). 

state(N,X) :- states_list(N, List), member(X, List). 

next_state(a,b).
next_state(b,c).
next_state(c,d).
next_state(d,e).
next_state(e,f).
next_state(f,g).
next_state(g,h).
next_state(h,i).

legal_list(_List, [0,1],           [0,1]).
legal_list(_List, [0,1,2],         [0,1,2]).
legal_list(List,  [0,1,2,3],       [0,1,2,3]) :- once( member(t(_,_,2,_,_), List) ). 
legal_list(List,  [0,1,2,3],       [0,1,2]) :- \+ member(t(_,_,2,_,_), List). 
legal_list(List,  [0,1,2,3,4],     [0,1,2,3,4]) :- once( member(t(_,_,3,_,_), List) ). 
legal_list(List,  [0,1,2,3,4],     [0,1,2,3]) :- \+ member(t(_,_,3,_,_), List), once( member(t(_,_,2,_,_), List) ). 
legal_list(List,  [0,1,2,3,4],     [0,1,2]) :- \+ member(t(_,_,2,_,_), List).
legal_list(List,  [0,1,2,3,4,5],   [0,1,2,3,4,5]) :- once( member(t(_,_,4,_,_), List) ). 
legal_list(List,  [0,1,2,3,4,5],   [0,1,2,3,4]) :- \+ member(t(_,_,4,_,_), List), once( member(t(_,_,3,_,_), List) ). 
legal_list(List,  [0,1,2,3,4,5],   [0,1,2,3]) :- \+ member(t(_,_,3,_,_), List), once( member(t(_,_,2,_,_), List) ). 
legal_list(List,  [0,1,2,3,4,5],   [0,1,2]) :- \+ member(t(_,_,2,_,_), List).
legal_list(List,  [0,1,2,3,4,5,6], [0,1,2,3,4,5,6]) :- once( member(t(_,_,5,_,_), List) ). 
legal_list(List,  [0,1,2,3,4,5,6], [0,1,2,3,4,5]) :- \+ member(t(_,_,5,_,_), List), once( member(t(_,_,4,_,_), List) ). 
legal_list(List,  [0,1,2,3,4,5,6], [0,1,2,3,4]) :- \+ member(t(_,_,4,_,_), List), once( member(t(_,_,3,_,_), List) ). 
legal_list(List,  [0,1,2,3,4,5,6], [0,1,2,3]) :-	\+ member(t(_,_,3,_,_), List), once( member(t(_,_,2,_,_), List) ). 
legal_list(List,  [0,1,2,3,4,5,6], [0,1,2]) :- \+ member(t(_,_,2,_,_), List).

legitimate_list(2, _,    [a,b]) :-         !. 
legitimate_list(3, _,    [a,b,c]) :-       !. 
legitimate_list(4, List, [a,b,c,d]) :-     once(  member(t(_,_,_,_,c), List) ). 
legitimate_list(4, List, [a,b,c]) :-       \+ member(t(_,_,_,_,c), List) . 
legitimate_list(5, List, [a,b,c,d,e]) :-   once(  member(t(_,_,_,_,d), List) ). 
legitimate_list(5, List, [a,b,c,d]) :-     \+ member(t(_,_,_,_,d), List) , once(  member(t(_,_,_,_,c), List) ). 
legitimate_list(5, List, [a,b,c]) :-       \+ member(t(_,_,_,_,d), List) , \+  member(t(_,_,_,_,c), List) . 
legitimate_list(6, List, [a,b,c,d,e,f]) :- once(  member(t(_,_,_,_,e), List) ). 
legitimate_list(6, List, [a,b,c,d,e]) :-   \+ member(t(_,_,_,_,e), List) , once(  member(t(_,_,_,_,d), List) ). 
legitimate_list(6, List, [a,b,c,d]) :-     \+ member(t(_,_,_,_,e), List) , \+ member(t(_,_,_,_,d), List) , once(  member(t(_,_,_,_,c), List) ). 
legitimate_list(6, List, [a,b,c]) :-       \+ member(t(_,_,_,_,e), List) ,\+ member(t(_,_,_,_,d), List) , \+  member(t(_,_,_,_,c), List) . 

collect(Left, Item, Right, OutItem) :- append(Left, [Item], Temp), append(Temp, Right, OutItem). 

% add_item(grp(I,N), List, NewList)
add_item(grp(I,_N), List, List) :- I == [].
add_item(grp(I,N), List, List) :- I \== [], ground(N), 0 is N.
add_item(grp(I,N), [], [grp(I,N)]) :- I \== [], ground(N), N1 is N, N1 > 0.
add_item(grp(I,N), [], [grp(I,N)]) :- I \== [], nonground(N,_).
add_item(grp(I1,N1), [grp(I2,N2)|Rest], [grp(I1,N3)|Rest]) :-  I1 == I2, eager_sum(N1, N2, N3).
add_item(grp(I1,N1), [grp(I2,N2)|Rest], Result) :- \+ (I1 == I2), append([grp(I1,N1)], [grp(I2,N2)|Rest], Result). 
add_item(grp(I,N), [I2|Rest], Result) :- \+ (I2 = grp(_,_)), append([grp(I,N)], [I2|Rest], Result). 

next_item(naive, [], Blank, []) :- blank_symbol(Blank).
next_item(naive, [I|Rest], I, Rest).
next_item(search, [], Blank, []) :- blank_symbol(Blank).
next_item(search, [I|Rest], I, Rest).

next_item(macro(K), [], grp(Blanks,1), []) :- blank_symbol(B), blanks(B, K, Blanks). 
next_item(macro(_K), [I|Rest], I, Rest) :- I = grp(_Item, Num), Num > 0, I \== []. 
next_item(macro(K), [I|Rest], NewItem, NewRest) :- I = grp(Item, Num), N is Num, (N == 0; (N > 0, Item = [])), next_item(macro(K), Rest, NewItem, NewRest). 
next_item(flex, [], grp(Blanks,1), []) :- blank_symbol(B), blanks(B, 1, Blanks). 
next_item(flex, [I|Rest], I, Rest) :- I = grp(_Item, Num), ground(Num), N is Num, N > 0, I \== []. 
next_item(flex, [I|Rest], NewItem, NewRest) :- I = grp(Item, Num), ground(Num), N is Num, (N == 0; (N > 0, Item = [])), next_item(flex, Rest, NewItem, NewRest). 
next_item(flex, [I|Rest], I, Rest) :- I = grp(_Item, Num), nonground(Num,_).

blanks(_, 0, []). 
blanks(Blank, N, [Blank|Result]) :- N > 0, N1 is N - 1, blanks(Blank, N1, Result). 

set_moves(l, r, N, N).
set_moves(r, l, N, M) :- M = -N. 
set_moves(r, r, _N, 1). 
set_moves(l, l, _N, M) :- M = -1. 

set_leaps_moves(l,r,Number, N, Steps, Leaps, Moves) :- ground(Number), Leaps is Steps * Number, Moves is N * Number.
set_leaps_moves(l,r,Number, N ,Steps, Leaps, Moves) :- \+ ground(Number), N = 1, Steps = 1, Leaps = Number, Moves = Number.
set_leaps_moves(l,r,Number, N ,Steps, Leaps, Moves) :- \+ ground(Number), N = 1, Steps > 1, Leaps = Steps * Number, Moves = Number. 
set_leaps_moves(l,r,Number, N, Steps, Leaps, Moves) :- \+ ground(Number), N > 1, Steps = 1, Leaps = Number, Moves = N * Number.
set_leaps_moves(l,r,Number, N, Steps, Leaps, Moves) :- \+ ground(Number), N > 1, Steps > 1, Leaps = Steps * Number, Moves = N * Number.
set_leaps_moves(r,l,Number, N, Steps, Leaps, Moves) :- ground(Number), Leaps is Steps * Number, Moves is 0-N * Number.
set_leaps_moves(r,l,Number, N, Steps, Leaps, Moves) :- \+ ground(Number), N = 1, Steps = 1, Leaps = Number, Moves = 0-Number.
set_leaps_moves(r,l,Number, N, Steps, Leaps, Moves) :- \+ ground(Number), N = 1, Steps > 1, Leaps = Steps * Number, Moves = 0-Number. 
set_leaps_moves(r,l,Number, N, Steps, Leaps, Moves) :- \+ ground(Number), N > 1, Steps = 1, Leaps = Number, Moves = 0-N * Number.
set_leaps_moves(r,l,Number, N, Steps, Leaps, Moves) :- \+ ground(Number), N > 1, Steps > 1, Leaps = Steps * Number, Moves = 0-N * Number.

% Leaps is (Number//K)*Steps
set_leaps_acc(Number, K, Steps, Leaps) :- ground(Number), Leaps is Steps * (Number//K).
set_leaps_acc(Number, K, Steps, Leaps) :- \+ ground(Number), K == 1, Steps == 1, Leaps = Number.
set_leaps_acc(Number, K, Steps, Leaps) :- \+ ground(Number), K > 1,  Steps == 1, Leaps = Number//K.
set_leaps_acc(Number, K, Steps, Leaps) :- \+ ground(Number), K == 1, Steps > 1, Leaps = Steps * Number.
set_leaps_acc(Number, K, Steps, Leaps) :- \+ ground(Number), K > 1,  Steps > 1, K == Steps, Leaps = Number - (Number mod K). % Is this overkill? 
set_leaps_acc(Number, K, Steps, Leaps) :- \+ ground(Number), K > 1,  Steps > 1, K \== Steps, Leaps = Steps * (Number//K). % Could we do more?

% Moves is (Number//K)*L
set_moves_acc(l,r,Number, K, L, Moves) :- ground(Number), Moves is L * (Number//K).
set_moves_acc(l,r,Number, K, L, Moves) :- \+ ground(Number), K == 1, L == 1, Moves = Number.
set_moves_acc(l,r,Number, K, L, Moves) :- \+ ground(Number), K > 1,  L == 1, Moves = Number//K. 
set_moves_acc(l,r,Number, K, L, Moves) :- \+ ground(Number), K == 1, L > 1,  Moves = L * Number.
set_moves_acc(l,r,Number, K, L, Moves) :- \+ ground(Number), K > 1,  L > 1,  Moves = L * (Number//K). 
set_moves_acc(r,l,Number, K, L, Moves) :- ground(Number), Moves is 0-L * (Number//K).
set_moves_acc(r,l,Number, K, L, Moves) :- \+ ground(Number), K == 1, L == 1, Moves = 0-Number.
set_moves_acc(r,l,Number, K, L, Moves) :- \+ ground(Number), K > 1,  L == 1, Moves = 0-(Number//K). 
set_moves_acc(r,l,Number, K, L, Moves) :- \+ ground(Number), K == 1, L > 1,  Moves = (0-L) * Number.
set_moves_acc(r,l,Number, K, L, Moves) :- \+ ground(Number), K > 1,  L > 1,  Moves = (0-L) * (Number//K).

% set_exponent and set_modulus are for the new extended acceleration transitions
% Exp is N*(Number // K) 
set_exponent(N, Number, K, Exp) :- ground(Number), K is 1, N1 is Number, Exp is N*N1. % Is this and the next three lines too fussy?
set_exponent(N, Number, K, Exp) :- ground(Number), K > 1, N is 1, N1 is Number, Exp is N1//K. 
set_exponent(N, Number, K, Exp) :- ground(Number), K > 1, N > 1, N == K, N1 is Number, Exp is N1 - (N1 mod K).
set_exponent(N, Number, K, Exp) :- ground(Number), K > 1, N > 1, N \== K, N1 is Number, Exp is N*(N1//K). 
set_exponent(N, Number, K, Exp) :- \+ ground(Number), K is 1, N is 1, Exp = Number.
set_exponent(N, Number, K, Exp) :- \+ ground(Number), K is 1, N > 1,  Exp = N*Number.
set_exponent(N, Number, K, Exp) :- \+ ground(Number), K > 1, N is 1,  Exp = Number//K. 
set_exponent(N, Number, K, Exp) :- \+ ground(Number), K > 1, N > 1, N == K, Exp = Number - (Number mod K).
set_exponent(N, Number, K, Exp) :- \+ ground(Number), K > 1, N > 1, N \== K, Exp = N*(Number//K). % Perhaps could do more with this when N is a multiple of K? 

% Mod is N*(Number mod K)
set_modulus(_Number, K, Mod) :- K is 1, Mod is 0.
set_modulus(Number, K, Mod) :- ground(Number), K > 1, Mod is Number mod K.
set_modulus(Number, K, Mod) :- \+ ground(Number), K > 1, Mod = Number mod K.

splitlast(T, T1, T2) :- append(T1, T2, T), length(T2, 1). 

history_memory(list, 300). 
history_memory(list(N), N). % New mode to allow per case list. 
history_memory(trie(N), N). % Experiment with this value - was originally 10. 
history_memory(newtrie(N,_), N). % Experiment with this value - was originally 10. 

output_blanks(0).
output_blanks(N) :- N > 0, format("   ", []), N1 is N-1, output_blanks(N1). 

prettyprint(Args, State, Left, Item, Dir, Pos, Mode, Right, Hops, Ones, Used, Time) :-
	length(Left,L), setblanks(Pos, Mode, L, Blanks), 
	reverse(Left, PL), 
	leftprint(Args, Mode, Blanks, PL, State, Item, Dir, Right), 
	format(" Hops: ", []), 
	write_term(Hops, [numbervars(true),ignoreops(true),variable_names(Args)]),
	format(" Ones: ~k Pos: ", [Ones]), % NPos #= Pos,
	write_term(Pos, [numbervars(true),ignoreops(true),variable_names(Args)]), 
	format(" Transition: ~k ", Used),
	( Time > 0 -> (get_time(Now), format(" Time so far: ~2f ", [Now-Time])) ; true ),
	true.

setblanks(Pos, Type, L, Blanks) :-
	member(Type, [naive]), 
	padding(Pad), 
	Blanks is Pad + Pos - L.
setblanks(_Pos, Type, _L, 0) :-
	\+ member(Type, [naive]).

leftprint(_, _, Blanks, _PL, _State, _Item, _, Right) :-
	Blanks < 0, format("{too long on the left} ", []), pprint([], Right).
leftprint(Args, naive, Blanks, PL, State, Item, _, Right) :- 
	Blanks >= 0, nblanks(Blanks), pprint(Args, PL), format("{~k}",[State]), pprint(Args, [Item]), pprint(Args, Right).
leftprint(Args, Mode, Blanks, PL, State, Item, Dir, Right) :- 
        member(Mode, [macro(_K), flex, search]), 
	Blanks >= 0, nblanks(Blanks), pprint(Args, PL), format("{~k}",[State]), pprint(Args, [Item]), format("{~k}", [Dir]), pprint(Args, Right).

padding(10). 
nblanks(N) :- N < 0, !, format("No space! ", []).
nblanks(0) :- !.
nblanks(N) :- N > 0, !, format(" ", []), N1 is N - 1, nblanks(N1). 

pprint(_, []). 
pprint(Args, [0|Rest]) :- !, format("0", []), pprint(Args, Rest).
pprint(Args, [1|Rest]) :- !, format("1", []), pprint(Args, Rest).
pprint(Args, [2|Rest]) :- !, format("2", []), pprint(Args, Rest).
pprint(Args, [3|Rest]) :- !, format("3", []), pprint(Args, Rest).
pprint(Args, [4|Rest]) :- !, format("4", []), pprint(Args, Rest).
pprint(Args, [5|Rest]) :- !, format("5", []), pprint(Args, Rest).
pprint(Args, [6|Rest]) :- !, format("6", []), pprint(Args, Rest).
pprint(Args, [7|Rest]) :- !, format("7", []), pprint(Args, Rest).
pprint(Args, [grp(T,C)|Rest]) :-
        nonground(C,_),
 	T = [_|_], !,
 	format("(",[]), 
 	pchars(T), 
	format(")^{",[]), ttyflush, 
	write_term(C, [numbervars(true),variable_names(Args)]), 
	format("}", []),
	pprint(Args, Rest). 
pprint(Args, [grp(T,C)|Rest]) :-
        nonvar(C), 
 	C = *, 
 	T = [_|_], !,
 	format("(",[]), 
 	pchars(T), 
	format(")*",[]), 
	pprint(Args, Rest). 
pprint(Args, [grp(T,C)|Rest]) :-
    \+ ground(C), \+ (C = v(_V,_,_,_,_)),
        format("(",[]), 
	pchars(T), 
	format(")^_[", []),
        write_term(C, [numbervars(true),variable_names(Args)]),
        format("]",[]),
        pprint(Args, Rest). 
pprint(Args, [grp(T,C)|Rest]) :-
	T = [_|_], % T is a list
	ground(C), 
	C #> 1, !,
	NC #= C, 
	format("(",[]), 
	pchars(T), ttyflush, 
	print_exponent(NC), !, 
	pprint(Args, Rest). 
pprint(Args, [grp(T,C)|Rest]) :-
	T = [_|_], % T is a list
	ground(C),  
	C #= 1, !,
	pchars(T), 
	pprint(Args, Rest). 
pprint(Args, [grp(_T,C)|Rest]) :-
	ground(C), 
	C #= 0, !,
	pprint(Args, Rest). 
pprint(Args, [grp(T,C,Visit)|Rest]) :-
	T = [_|_], % T is a list
	ground(C), 
	C #> 1, !, NC #= C,
	format("(",[]), 
	pchars(T), 
	format(")^{~d}{~d}",[NC,Visit]), 
	pprint(Args, Rest). 
pprint(Args, [grp(T,C,Visit)|Rest]) :-
	T = [_|_], % T is a list
	ground(C), 
	C #= 1, !,
	pchars(T), format("{~d}", [Visit]), 
	pprint(Args, Rest). 
pprint(Args, [grp(T,C,_Visit)|Rest]) :-
	T = [_|_], % T is a list
	ground(C), 
	C #= 0, !,
	pprint(Args, Rest). 

print_exponent(N) :- max_print(Max), N =< Max, format(")^{~d}",[N]).
print_exponent(N) :- max_print(Max), N > Max, big_number_of_digits(N,  Number), format(")^{digits(~d)}",[Number]), !.

print_bignum(N) :- print_bignum1(external, N).

print_bignum1(_,N) :- max_print(Max), N =< Max, format("~d", [N]), !. 
print_bignum1(external, N) :- max_print(Max), N > Max, !, big_number_of_digits(N, Number), format("{~d digits}", [Number]).
print_bignum1(internal, N) :- max_print(Max), N > Max, !, big_number_of_digits(N, Number), format("digits(~d)", [Number]).

print_bignum(S, N) :- print_bignum1(S, external, N).

print_bignum1(S,_,N) :- max_print(Max), N =< Max, format(S, "~d", [N]), !. 
print_bignum1(S, external, N) :- max_print(Max), N > Max, !, big_number_of_digits(N, Number), format(S, "{~d digits}", [Number]).
print_bignum1(S, internal, N) :- max_print(Max), N > Max, !, big_number_of_digits(N, Number), format(S, "digits(~d)", [Number]).

max_print(999999999999999).

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
      NewN is N // 10, !, 
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
pchars([6|Rest]) :- !, format("6",[]), pchars(Rest). 
pchars([7|Rest]) :- !, format("7",[]), pchars(Rest). 

% Machine language predicates. 

blank_symbol(0). % default
initial_state(a). % default
halt_state(z). % default. 
looping_state(loop). % default. 

% minimum_compress_size(6). 

input([grp(In,_)|_], In) :- !. 
input([In|_], In) :- is_input(In), !. 

is_input(S) :- member(S, [0,1,2,3,4,5,6,7]). 

is_tape([]).
is_tape([I|Rest]) :- is_input(I), is_tape(Rest). 

blanks(I, []) :- blank_symbol(I). 
blanks(K, [I|Rest]) :- blank_symbol(I), K > 0, K1 is K-1, blanks(K1, Rest).

number_of_states(machine(M, _, _), N) :-  states(M, Ss), length(Ss, N). 
number_of_states(M, N) :- M \== machine(_,_,_), states(M, Ss), length(Ss, N). 

states(M, States) :- \+ atomic(M), states1(M, [], States).
states(M, States) :- atomic(M), states1(M, States).

states1(riemann, 1200).
states1(zermelo, 1200).
states1(goldbach,1200).

states1([], States, States).
states1([t(S,_,_,_,_)|Rest], StatesSoFar, States) :-
	add_to_list(S, StatesSoFar, NewSoFar),
	states1(Rest, NewSoFar, States).

number_of_states_used(machine(M, _, _), N) :- states_used(M, Ss), length(Ss, N). 
number_of_states_used(M, N) :- M \== machine(_,_,_), states_used(M, Ss), length(Ss, N). 

number_of_symbols_used(machine(M, _, _), N) :- symbols_used(M, Ss), length(Ss, N). 
number_of_symbols_used(M, N) :- M \== machine(_,_,_), symbols_used(M, Ss), length(Ss, N). 

states_used(M, States) :- \+ atomic(M), states_used1(M, [], States).
states_used(M, States) :- atomic(M), states_used1(M, States).

states_used1(M, States) :- states1(M, States). 

states_used1([], States, States).
states_used1([t(S,_,_,_,NS)|Rest], StatesSoFar, States) :-
	add_to_list(S, StatesSoFar, NewSoFar1),
	add_to_list(NS, NewSoFar1, NewSoFar2),
	states_used1(Rest, NewSoFar2, States).

symbols_used(M, Symbols) :- \+ atomic(M), symbols_used1(M, [], Symbols).
symbols_used(M, Symbols) :- atomic(M), Symbols is 2.

symbols_used1([], Symbols, Symbols).
symbols_used1([t(_,S1,S2,_,_)|Rest], SymbolsSoFar, Symbols) :-
	add_to_list(S1, SymbolsSoFar, NewSoFar1),
	add_to_list(S2, NewSoFar1, NewSoFar2),
	symbols_used1(Rest, NewSoFar2, Symbols).

add_to_list(Item, List, List) :- member(Item, List). 
add_to_list(Item, List, NewList) :- \+ member(Item, List), append(List, [Item], NewList). 

number_of_symbols(machine(M, _, _), N) :- symbols(M, Ss), length(Ss, N). 
number_of_symbols(M, N) :- M \== machine(_,_,_), symbols(M, Ss), length(Ss, N). 

symbols(M, Symbols) :- \+ atomic(M), symbols1(M, [], Symbols).
symbols(M, Symbols) :- atomic(M), Symbols is 2.

symbols1([], Symbols, Symbols).
symbols1([t(_,I,O,_,_)|Rest], SymbolsSoFar, Symbols) :-
	add_to_list(I, SymbolsSoFar, NewSoFar1),
	add_to_list(O, NewSoFar1, NewSoFar2),
        symbols1(Rest, NewSoFar2, Symbols). 

number_of_dirs(machine(M,_,_),N) :- number_of_dirs1(M,N).
number_of_dirs(M,N) :- M \== machine(_,_,_), number_of_dirs1(M,N). 

number_of_dirs1(M,2) :- \+ atomic(M), up_dir(UpDir), down_dir(DownDir), \+ member(t(_,_,_,UpDir,_), M), \+ member(t(_,_,_,DownDir,_), M),!.
number_of_dirs1(M,3) :- \+ atomic(M), up_dir(UpDir), down_dir(DownDir), member(t(_,_,_,UpDir,_), M), \+ member(t(_,_,_,DownDir,_), M),!.
number_of_dirs1(M,4) :- \+ atomic(M), down_dir(DownDir), member(t(_,_,_,DownDir,_), M),!.
number_of_dirs1(M,2) :- atomic(M).

bound_max(1000000). 
maxbound(B, B) :-  bound_max(Bound), B =< Bound, !. 
maxbound(B, Bound) :- bound_max(Bound), B > Bound, !. %% Do not run for more than this many steps looking for a transition! May need to change this value ... 

remove(Item, List, NewList) :- member(Item, List), delete(List, Item, NewList). 

% delete_list(List1, List2, List3). List 3 is the result of deleting all elements of List2 from List1. 
delete_list(List1, [], List1). 
delete_list(List1, [Item|Rest], List3) :- member(Item, List1), delete(List1, Item, Temp), delete_list(Temp, Rest, List3). 
delete_list(List1, [Item|Rest], List3) :- \+ member(Item, List1), delete_list(List1, Rest, List3). 

% Debugging tool ... 
show_me([]).
show_me([I|Rest]) :-
      I = grp(I1,_Num), show_me_special(I1), show_me(Rest).
show_me([I|Rest]) :-
      I \= grp(_,_), show_me(Rest).

show_me_special([]).
show_me_special([1|Rest]) :- format("One", []), show_me_special(Rest).
show_me_special([l|Rest]) :- format("Left", []), show_me_special(Rest).
show_me_special([X|Rest]) :- X \== 1, X \== l, write(X), show_me_special(Rest).

variable(X) :- term_variables(X, List), List \== []. % Really means 'is an expression containing a variable'.

% % Use this to test whether a given varilable is in a list. This avoids unintended unifications. 
% var_member(X, [Y|_Rest]):- X == Y.
% var_member(X, [Y|Rest]):- X \== Y, var_member(X, Rest). 

% % List3 is the list of variables occurring in both List1 and List2.
% overlapping_vars([], _, _, []).
% overlapping_vars(_, [], _, []).
% overlapping_vars(List1, List2, Names2, List3) :-
%     List1 \== [], List2 \== [], 
%     List1 = [X|_], var(X), List2 = [Y|_], string(Y), !, % Names is the names of the variables in List2
%     stringify(List1, SL1, Names1), intersection(SL1, List2, L3), append(Names1, Names2, Ns), sort(Ns, Names), unstringify(L3, Names, List3).
% 
% intersection([], _List2, []).
% intersection([X|Rest1], List2, [X|Result]) :- member(X, List2), intersection(Rest1, List2,  Result).
% intersection([X|Rest1], List2, Result) :- \+ member(X, List2), intersection(Rest1, List2,  Result). 
 
stringify([], [], []).
stringify([V|Vars], [VString|Rest], [Atom=V|Names]) :- var(V), term_string(V, VString), atom_string(Atom, VString), stringify(Vars, Rest, Names). 

% unstringify([], _, []).
% unstringify([VString|Strings], Names, [Var|Rest]) :- atom_string(A, VString), member(A=Var, Names), term_string(Var, VString, [variable_names([A=Var])]), unstringify(Strings, Names, Rest). 

separator(",").

% clean_status(unknown,unknown).
% clean_status(going,going). 
% clean_status(going(_),going). 
% clean_status(halts,halts). 
% clean_status(meander,meander). 
% clean_status(blank,blank). 
% clean_status(loops(cycle(Config,H1,H2)),cycle(Config,H1,H2)). 
% clean_status(loops,loops). 
% clean_status(loops(road_runner),road_runner). 
% clean_status(loops(induction(_,_,hops(H1,H2,H3,H4))),induction(H1,H2,H3,H4)). 
% clean_status(normal,normal). 
% clean_status(pseudo,pseudo). 
% clean_status(pseudo(_,_),pseudo). 
% clean_status(abort,abort). 
% clean_status(abort(_),abort). 

% Status can be unknown, going, going(_), halts, meander, blank, loops(cycle), loops, loops(cycle(_)), loops(road_runner), loops(induction(_)), normal, pseudo, pseudo(_,_), 

% Utilities
append(W,X,Y,Z) :- append(W,X,T), append(T,Y,Z). 

bbinternalprint(Pred, N, M, O, H, Status) :-
	format("~k(~d,[", [Pred,N]),!, 
	printint(M), !, 
	format("],~d,~d,", [O,H]),!, 
	write(Status), !,  
        format("). ", []), !.

bbstringprint(Pred, N, M, O, H, Status) :-
    format("~k(~d,""", [Pred,N]),!,
    format("~s", [M]), !,
    format(""",~d,~d,", [O,H]),!, 
    write(Status), !,  
    format(").~n", []), !.
    
% sort_records([], []). 
% sort_records(List, List) :- length(List, 1). 
% sort_records(List, SortedList) :-
%       length(List, N), N >= 2,
%       divide(List, First, Second),
%       sort_records(First, Fs),
%       sort_records(Second, Ss),
%       merge_records(Fs, Ss, SortedList).
% 
% merge_records([], Ys, Ys).
% merge_records(Ys, [], Ys).
% merge_records([S1|Fs], [S2|Ss], [S1|Ys]) :- 
%       less_equal(S1, S2), merge_records(Fs, [S2|Ss], Ys).
% merge_records([S1|Fs], [S2|Ss], [S2|Ys]) :- 
%       \+ less_equal(S1, S2), merge_records([S1|Fs], Ss, Ys).
% 
% divide(List, First, Second) :-
%       length(List, N), Half is N // 2, 
%       length(First, Half), 
%       append(First, Second, List).

% less_equal(max(O1,_,_,_), min(O2,_,_,_)) :- O1 < O2. 
% less_equal(min(O1,_,_,_), max(O2,_,_,_)) :- O1 < O2. 
% less_equal(min(O1,_,_,_), min(O2,_,_,_)) :- O1 < O2. 
% less_equal(max(O1,_,_,_), max(O2,_,_,_)) :- O1 < O2. 
% less_equal(min(O1,_,_,_), max(O2,_,_,_)) :- O1 == O2. 

% output_records([]) :- !.
% output_records([max(O, N, M, H)|Rest]) :- 
% 	!, format("% ", []), bbinternalprint(maximum, N, M, O, H, halts),
% 	output_records(Rest).
% output_records([min(O, N, M, H)|Rest]) :- 
% 	!, format("% ", []), bbinternalprint(minimum, N, M, O, H, halts),
% 	output_records(Rest).

printint([]) :- !. 
printint([A]) :- format("~k", [A]), !. 
printint(Arg) :- Arg = [A1|Temp], Temp = [A2|Rest], !, format("~k,", [A1]), !, printint([A2|Rest]).

sortmachine(_N, _States, _Symbols, _State, _Input, Done, [], M) :- reverse(Done, M),!. 
sortmachine(N, States, Symbols, State, Input, Done, Todo, Result) :-
	length(Todo, T), T > 0,
	member(t(State, Input, Output, Dir, NewState), Todo), !, 
	delete(Todo, t(State, Input, Output, Dir, NewState), Rest),
	(Rest \== [] -> increment(States, Symbols, State, Input, NS, NI); (NS = State, NI = Input) ), !, 
	sortmachine(N, States, Symbols, NS, NI, [t(State, Input, Output, Dir, NewState)|Done], Rest, Result).

sortmachine(N, States, Symbols, State, Input, Done, Todo, Result) :-
	length(Todo, T), T > 0,
	\+ member(t(State, Input, _Output, _Dir, _NewState), Todo), !, 
	(Todo \== [] -> increment(States, Symbols, State, Input, NS, NI); (NS = State, NI = Input) ), !,
	sortmachine(N, States, Symbols, NS, NI, Done, Todo, Result).

padmachine(States, Symbols, _Dirs, M, PadM) :-
        length(M, L), L is States * Symbols, !, PadM = M. 
padmachine(States, Symbols, Dirs, M, PadM) :-
        length(M, L), L < States * Symbols, !, 
	padmachine1(States, Symbols, Dirs, M, s(a,0), [], PadM). 

padmachine1(_States, _Symbols, _Dirs, [], _Case, SoFar, SoFar).
padmachine1(States, Symbols, Dirs, [T|Rest], Case, SoFar, Result) :-
    T = t(S, I, _O, _D, _NS), Case = s(S,I), % No padding needed
    append(SoFar, [T], NewSoFar),
    next_case(Symbols, Case, NextCase),
    padmachine1(States, Symbols, Dirs, Rest, NextCase, NewSoFar, Result).
padmachine1(States, Symbols, Dirs, [T|Rest], Case, SoFar, Result) :-
    T = t(S, I, _O, _D, _NS), Case = s(S1,I1), s(S,I) \== s(S1,I1), % Padding needed due to Case missing
    append(SoFar, [t(S1,I1,-,-,-)], NewSoFar),
    next_case(Symbols, Case, NextCase),
    padmachine1(States, Symbols, Dirs, [T|Rest], NextCase, NewSoFar, Result).

next_case(Symbols, Case, NextCase) :-
    Case = s(S,C), C+1 < Symbols,
    NextC is C+1,
    NextCase = s(S,NextC).
next_case(Symbols, Case, NextCase) :-
    Case = s(S,C), Symbols is C+1, % Time to move to next state
    next_state(S, NextS), 
    NextCase = s(NextS,0).

convert_machine(Format, _States, _Symbols, _Dirs, _M, NewM) :- \+ member(Format, [string,list]), NewM = "".
convert_machine(string, States, Symbols, Dirs, machine(List, _, _), NewM) :- convert_machine0(States, Symbols, Dirs, List, NewM).
convert_machine(string, States, Symbols, Dirs, M, NewM) :- M \== machine(_,_,_), convert_machine0(States, Symbols, Dirs, M, NewM).
convert_machine(list, States, Symbols, Dirs, machine(List, _, _), NewM) :- convert_machine00(States, Symbols, Dirs, List, NewM).
convert_machine(list, States, Symbols, Dirs, M, NewM) :- M \== machine(_,_,_), convert_machine00(States, Symbols, Dirs, M, NewM).

% converts Machine in my format to Marxen format. 
convert_machine0(States, Symbols, Dirs, M, NewM) :-
    prepare(States, Symbols, Dirs, M, ReadyM),
    convert_machine1(ReadyM, "", NewM). % NewM is a string representing the Marxen representation of the machine.

prepare(States, Symbols, Dirs, M, ReadyM) :-
    sort(M, SortM),
    padmachine(States, Symbols, Dirs, SortM, ReadyM).

convert_machine1([], SoFarNewM, SoFarNewM).
convert_machine1([T|Rest], SoFarNewM, NewM) :-
    T == t(a,0,1,r,b), % Ignore a,0 transition as it is always the same 
    convert_machine1(Rest, SoFarNewM, NewM). 
convert_machine1([T|Rest], SoFarNewM, NewM) :-
    T \== t(a,0,1,r,b),
    convert_transition(T, NewTrans), 
    string_concat(SoFarNewM, NewTrans, NewSoFarNewM),
    convert_machine1(Rest, NewSoFarNewM, NewM). 

convert_transition(t(_,_,O,D,NS), NewTrans) :-
    atom_string(O, NewO), atom_string(D, NewD), atom_string(NS, NewNS), 
    string_concat(NewO,NewD,Temp), string_concat(Temp, NewNS, NewTrans).
        
% converts Machine in Marxen format to my format. 
convert_machine00(States, Symbols, _Dirs, M, NewM) :-
    convert_machine001(States, Symbols, s(a,0), M, [], NewM). 

convert_machine001(_States, _Symbols, _Case, "", SoFar, SoFar).
convert_machine001(States, Symbols, Case, M, SoFar, NewM) :-
    M \== "",
    Case = s(a,0), % This case is always the same, and so is not explicitly stored in the string
    NewTrans = t(a,0,1,r,b), 
    append(SoFar, [NewTrans], NewSoFar),
    next_case(Symbols, Case, NextCase), 
    convert_machine001(States, Symbols, NextCase, M, NewSoFar, NewM).

convert_machine001(States, Symbols, Case, M, SoFar, NewM) :-
    M \== "",
    Case = s(ThisState, ThisSymbol), s(ThisState, ThisSymbol) \== s(a,0),
    string_concat(Trans, RestM, M), string_length(Trans, 3),
    split_transition(Trans, O, D, NS), 
    NewTrans = t(ThisState, ThisSymbol, O, D, NS),
    append(SoFar, [NewTrans], NewSoFar),
    next_case(Symbols, Case, NextCase), 
    convert_machine001(States, Symbols, NextCase, RestM, NewSoFar, NewM).

split_transition(Trans, NewO, NewD, NewNS) :-
    Trans \== "---", 
    string_concat(O, RestT, Trans), string_concat(D, NS, RestT), string_length(O,1), string_length(D,1), string_length(NS,1),
    number_string(NewO,O),
    atom_string(NewD,D),
    atom_string(NewNS,NS).

split_transition(Trans, -, -, -) :- Trans == "---".

dir_to_string(l, "L").
dir_to_string(r, "R").

filter_transitions([], []).
filter_transitions([Trans|Rest], [Trans|Result]) :- real_transition(Trans), filter_transitions(Rest, Result). 
filter_transitions([Trans|Rest], Result) :- \+ real_transition(Trans), filter_transitions(Rest, Result). 

real_transition(t(_S,_I,O,D,NS)) :- \+ (O == -, D == -, NS == -). 

% --- Development utilities

arity(1, [X]).
arity(N, Vars) :- N > 1, N1 is N-1, arity(N1, V1), Vars = [X|V1].

depends_trans(Pred1, Pred2) :- depends_on(Pred1, Pred2). 
depends_trans(Pred1, Pred2) :- depends_on(Pred1, Pred3), \+ ignore(Pred3), Pred3 \== Pred2, depends_trans(Pred3, Pred2).
 
dependents(List, SortedDependents) :- findall(Pred2, (member(Pred1, List), depends_on(Pred1, Pred2)), Dependents), sort(Dependents, SortedDependents).

new_dependents(List, SortedDependents) :- findall(Pred2, (member(Pred1, List), depends_on(Pred1, Pred2), \+ member(Pred2, List)), Dependents), sort(Dependents, SortedDependents). 

all_dependents(List, Result) :- dependents(List, FirstDependents), append(List, FirstDependents, AllSoFar), find_all_dependents(AllSoFar, FirstDependents, Result).

find_all_dependents(AllSoFar, Frontier, AllSoFar) :- new_dependents(Frontier, New), extra_list(AllSoFar, New, [], []).
find_all_dependents(AllSoFar, Frontier, Result) :- new_dependents(Frontier, New), extra_list(AllSoFar, New, [], RealNew), RealNew \== [], append(AllSoFar, RealNew, NewSoFar), sort(NewSoFar, RealNewSoFar), find_all_dependents(RealNewSoFar, RealNew, Result). 

% find_all_dependents(List, Result) :- new_dependents(List, []), Result = List.
% find_all_dependents(List, Result) :- new_dependents(List, NewList), extra_list(NewList, List, []), Result = List.
% find_all_dependents(List, Result) :- new_dependents(List, NewList), extra_list(NewList, List, RealNewList), RealNewList \== [], append(List, RealNewList, Temp), sort(Temp, SortedTemp), find_all_dependents(SortedTemp, Temp2), sort(Temp2, Temp3), Result = Temp3. 

depends_on(Pred1, Pred2) :-
    current_predicate(Pred1/N), arity(N, Vars), Pred =.. [Pred1|Vars], clause(Pred, Body), body_term_calls(Body, Goal), Goal =.. [Pred2|Args], \+ ignore(Pred2), Pred2 \== Pred1.

ignore(',').
ignore(';').
ignore(':').
ignore('->').
ignore(member).
ignore(append).
ignore(delete). 
ignore(reverse).
ignore(length).
ignore(!). 
ignore(Pred) :- built_in_predicate(Pred/_).
ignore(is_blank).  % Not strictly but makes things simpler
ignore(all_blank). % Not strictly but makes things simpler

built_in_predicate(Name/Arity) :-
    predicate_property(system:Head, built_in),
    functor(Head, Name, Arity),
    \+ sub_atom(Name, 0, _, _, $). % From SWI Prolog documentation

% List3 is all elements of List2 which are not in List1. 

extra_list(_, [], SoFar, SoFar).
extra_list(List1, [Item|List2], SoFar, Result) :- member(Item, List1), !, extra_list(List1, List2, SoFar, Result). 
extra_list(List1, [Item|List2], SoFar, Result) :- \+ member(Item, List1), !, append(SoFar, [Item], NewSoFar), extra_list(List1, List2, NewSoFar, Result). 

% dual([], []).
% dual([Transition|Rest], [NewTransition|NewRest]) :- dual_transition(Transition, NewTransition), dual(Rest, NewRest). 
% 
% dual_transition(t(S,I,O,D,NS), t(NewI, NewS, O, D, z)) :- NS == z,  dual_state_symbol(S, NewS), dual_state_symbol(NewI, I).
% dual_transition(t(S,I,O,D,NS), t(NewI, NewS, NewNS, D, NewO)) :- NS \== z, dual_state_symbol(S, NewS), dual_state_symbol(NewI, I), dual_state_symbol(NS, NewNS), dual_state_symbol(NewO, O). 
% 
% dual_state_symbol(a,0).
% dual_state_symbol(b,1).
% dual_state_symbol(c,2).
% dual_state_symbol(d,3).
% dual_state_symbol(e,4).
% dual_state_symbol(f,5).
% dual_state_symbol(g,6).
% dual_state_symbol(h,7).

% % [abort,abort(_),unknown,going,going(_),halts,meander,blank,loops,loops(cycle),loops(cycle(_)),loops(road_runner),loops(induction),pseudo,pseudo(_)]), 

% check_going(Going, N, Machine, Ones, Hops, going) :-	!, write(Going, bb(N, Machine, Ones, Hops, going)),    put_char(Going, '.'), nl(Going), !. 
% check_going(Going, N, Machine, Ones, Hops, going(S)) :- !, write(Going, bb(N, Machine, Ones, Hops, going(S))), put_char(Going, '.'), nl(Going), !. 
% check_going(Going, N, Machine, Ones, Hops, unknown) :-  !, write(Going, bb(N, Machine, Ones, Hops, unknown)),  put_char(Going, '.'), nl(Going), !. 
% check_going(_Going, _N,_Machine, _Ones, _Hops, Status) :- member(Status, [abort,abort(_),halts,meander,blank,loops,loops(_),pseudo,pseudo(_)]), !.

% typecode(_, free, "free").
% typecode(_, all, "all").
% typecode(_, tnf, "tnf").
% 
% modecode(output, raw, "raw").
% modecode(output, analyse, "").
% modecode(output, refine, "").
% modecode(output, plot, "").
% modecode(output, pseudouni, "").
% modecode(output, stats, "").
% modecode(output, status, "").
% modecode(output, abort, "").
% modecode(output, convert(F,N), Name) :- atom_codes(F, FName), atom_codes(N, NName), string_concat(FName, NName, Temp), string_concat(Temp, "raw", Name). 
% modecode(input, raw, "raw").
% modecode(input, analyse, "raw").
% modecode(input, refine, "").
% modecode(input, plot, "").
% modecode(input, status, "").
% modecode(input, pseudouni, "raw").
% modecode(input, stats, "raw").
% modecode(input, number, "raw").
% modecode(input, abort, "raw").
% modecode(input, convert(_,_), "raw").
% modecode(generate, _, "raw").

% stylecode(2, abs, ""). 
% stylecode(2, rel, "rel"). 
% stylecode(Dirs, abs, "abs") :- Dirs > 2. 
% stylecode(Dirs, rel, "rel") :- Dirs > 2. 
% 
% extracode(input, M, "") :- \+ member(M, [refine, plot, status]).
% extracode(input, refine, "-going").
% extracode(input, status, "-out").
% extracode(input, plot, "-out").
% extracode(output, M, "") :- \+ member(M, [abort, refine, status]).
% extracode(output, refine, "-going-out").
% extracode(output, status, "-out").
% extracode(output, abort, "-out"). % ~~~??
% extracode(generate, _, ""). 
% 
% mode_status(status, output). 
% mode_status(Mode, input) :- Mode \= status. 
    
% precode(_, input, "").
% precode(_, output, "").
% precode(_, number, "").
% precode(_, analyse, "").
% precode(_, abort, "").
% precode(_, refine, "").
% precode(_, plot, "").
% precode(_, stats, "").
% precode(_, status, "").
% precode(_, convert(_,_), "").
% precode(_, pseudoni, "").
% precode(_, raw, "temp-"). 

% partcode(Mode, S4, Code) :- Mode \== abort, string_concat("-part", S4, Code). 
% partcode(abort, _, "abort"). 

% summary_name(analyse, "analyse").
% summary_name(abort, "abort").
% summary_name(refine, "refine").
% summary_name(plot, "").
% summary_name(raw, "").
% summary_name(number, "").
% summary_name(pseudouni, "pseudouni").
% summary_name(stats, "stats").
% summary_name(status, "status").
% summary_name(convert(_,_), "convert").

% Parameters. These need to be rationalised!
variable_bound(50). % How long to search for observant otter proof ... max found so far is 41 for monster 29
% induction_bound(50). % How long to search for induction proof ... need some data on this one
% minimum_for_loop(12). % minimum number of copies of item in original
% maximum_for_loop(6). % maximum number of extra copies to try. 
% compress_length(10). % Was 5, but then found long(4,1,M) and then example(3,M). 

beaver(input,  "G:/busybeaver/Data/Raw3/"). % Directory for input for analysis (ie where generated list of unanalysed machines gets put).  
beaver(output, "G:/busybeaver/Data/Results4/"). % Directory where results get written. 

which_dir(Mode,States,Symbols,Dirs,Style,Type,Directory) :- member(Mode,[plot,refine,status]), directory(output,States,Symbols,Dirs,Style,Type,Directory).
which_dir(Mode,States,Symbols,Dirs,Style,Type,Directory) :- \+ member(Mode, [plot,refine, status]), directory(input,States,Symbols,Dirs,Style,Type,Directory).

% directory(output, States, Symbols, Style, Dirs, tnf, Base), 
directory(Mode, States, Symbols, Dirs, _Style, Type, Directory) :-
        Dimension is States * Symbols * Dirs, 
	((Dimension < 16); (Dimension == 16, Type \== all)), 
	beaver(Mode, Base), Directory = Base. 
directory(Mode, States, Symbols, Dirs, Style, Type, Directory) :-
        Dimension is States * Symbols * Dirs, 
	((Dimension > 16);(Dimension == 16, Type == all)), % Use subdirectory for larger cases. 16 is file unless Type is all
	beaver(Mode, Base), 
	number_string(States,S1), number_string(Symbols,S2), number_string(Dirs,S3),
	(Style = rel -> string_concat(S3,"rel", S4); S4 = S3), 
	string_concat(S1,S2,Temp0), string_concat(Temp0, S4, Temp1),
	string_concat("busybeaver", Temp1, Temp2),
	string_concat(Temp2, "/", Temp3),
	string_concat(Base, Temp3, Directory). 

ensure_exists(Directory) :- atom_codes(Real, Directory), exists_directory(Real). 
ensure_exists(Directory) :- atom_codes(Real, Directory), \+ exists_directory(Real), make_directory(Real). 

% filelinemax(1000000) :- !. % maximum number of lines to write to a file before starting a new one. 

% Parameters and utilities.
% There is a path in machine M of length N from S to NS
path(S,NS,N,M,[t(S,I,O,D,NS)]) :- N is 1, remove(t(S,I,O,D,NS), M, _RestM). 
path(S,NS,N,M,[t(S,I,O,D,S1)|Path]) :- N > 1, remove(t(S,I,O,D,S1), M, RestM), N1 is N - 1, path(S1,NS,N1,RestM,Path).
      
% There is a simple path in machine M of length N from S to NS.
% This means that S does not appear in the internal part of the path. 
simple_path(S,NS,N,M,Path) :- simple_path1(S, S,NS,N,M,Path).
simple_path1(_Source, S,NS,N,M,[t(S,I,O,D,NS)]) :-
      N is 1, remove(t(S,I,O,D,NS), M, _RestM). 
simple_path1(Source, S,NS,N,M,[t(S,I,O,D,S1)|Path]) :-
      N > 1, remove(t(S,I,O,D,S1), M, RestM), 
      N1 is N - 1, Source \== S1, 
      simple_path1(Source,S1,NS,N1,RestM,Path).

% There is a cycle in machine M of length N from S to S
cycle(S,N,M,Path) :- path(S,S,N,M,Path). 

% There is a path in machine M of length somewhere between 1 and size(M) from S to NS
some_path(S,NS,M,Path) :- dimension(M,Size), between(1,Size,N), path(S,NS,N,M,Path).

% There is a simple path in machine M of length somewhere between 1 and size(M) from S to NS
some_simple_path(S,NS,M,Path) :- dimension(M,Size), between(1,Size,N), simple_path(S,NS,N,M,Path).

% Size is the size of M. One day this will include direction as well. 
dimension(M, Size) :- number_of_states(M,States), number_of_symbols(M,Symbols), Size is States * Symbols, !. 
     
% member_num(X, List, N)
% X is the Nth member of List. Fails if X is not a member of List. 
member_num(X, List, N) :- member_num0(X, 1, List, N).  

member_num0(X, NumSoFar, [X|_], NumSoFar). 
member_num0(X, NumSoFar, [Y|Rest], N) :- X \== Y, NewNumSoFar is NumSoFar + 1, member_num0(X, NewNumSoFar, Rest, N). 

% set_status(S, S) :- \+ member(S, [going, going(_), unknown]). 
% set_status(unknown,going).
% set_status(going,going).
% set_status(going(N),going(N)).

% bound(M, Type, Bound, Steps) :- number_of_states(M, States), number_of_symbols(M, Symbols), number_of_dirs(M, Dirs), bound(States, Symbols, Dirs, Type, Bound, Steps).

% bound(2,2,2,search,  50,    50).
% bound(2,2,2,analyse, 100,   100).
% bound(2,2,2,abort,   100,   100).
% bound(2,2,2,monster, 1000,  1000).
% bound(3,2,2,search,  50,    100).
% bound(3,2,2,analyse, 200,   100).
% bound(3,2,2,abort,   200,   100).
% bound(3,2,2,monster, 1000,  1000).
% bound(2,3,2,analyse, 10000, 1000).
% bound(2,3,2,abort,   1000,  1000).
% bound(2,3,2,monster, 100000, 2000).
% bound(2,3,2,search,  50,     100).
% bound(4,2,2,monster, 150000, 2000).% Some 4,2 cases need history 1,000 and hops 150,000 ... 
% bound(4,2,2,analyse, 50000,  10000).
% bound(4,2,2,search,  100,    100).
% bound(4,2,2,abort,   100,    100).
% bound(2,4,2,analyse, 500000, 20000).
% bound(2,4,2,abort,   1000,   1000).
% bound(2,4,2,monster, 5000000, 200000).
% bound(2,4,2,search,  100,    100).
% bound(3,3,2,analyse, 20000,  1000).
% bound(3,3,2,monster, 10000000000, 20000000).
% bound(3,3,2,search,  100,    100).
% bound(3,3,2,abort,   1000,   1000).
% bound(5,2,2,analyse, 20000,  1000).
% bound(5,2,2,monster, 50000000, 50000).
% bound(5,2,2,search,  300,    300).
% bound(5,2,2,abort,   1000,   1000).
% bound(2,5,2,analyse, 10000,  1000).
% bound(2,5,2,monster, 100000000, 5000).
% bound(2,5,2,search,  200,    200).
% bound(2,5,2,abort,   1000,   1000).
% 
% bound(6,2,2,search,100,100).
% bound(4,3,2,search,100,100).
% bound(3,4,2,search,100,100).
% bound(2,6,2,search,100,100).

% meander(1,[t(a,0,4,l,b),t(a,1,1,r,z),t(a,2,2,r,a),t(a,3,0,l,b),t(a,4,3,l,b),t(b,0,2,r,a),t(b,1,3,l,b),t(b,2,3,r,b),t(b,3,2,l,b),t(b,4,1,l,b)]).  % Not a meanderer!
% meander(2,[t(a,0,1,r,b),t(a,1,1,r,z),t(a,2,2,r,a),t(a,3,0,l,b),t(a,4,3,l,b),t(b,0,2,r,a),t(b,1,3,l,b),t(b,2,3,r,b),t(b,3,2,l,b),t(b,4,1,l,b)]).  % Not a meanderer!
% meander(3,[t(a,0,1,r,b),t(a,1,2,l,a),t(a,2,1,r,a),t(b,0,1,l,c),t(b,1,1,l,a),t(b,2,2,r,c),t(c,0,1,l,z),t(c,1,1,l,a),t(c,2,2,r,b)]). % Not a meanderer!
% meander(4,[t(a,0,1,r,b),t(a,1,1,l,a),t(b,0,0,l,a),t(b,1,1,l,c),t(c,0,1,r,z),t(c,1,0,l,d),t(d,0,1,r,d),t(d,1,0,l,a)]). % Meanderer from CATS07 paper. 

backwards_bound(10). % Experiment with this value.

% output_row(Symbols) :- output_row1(0, Symbols).
% output_row1(Current, Final) :- Current >= Final. 
% output_row1(Current, Final) :- Current < Final, format("<td><a class=heading>~d</a>", [Current]), NewCurrent is Current + 1, output_row1(NewCurrent, Final). 
% 	
% output_html_table(Symbols, M) :-
% 	sortmachine(Symbols, a, 0, [], M, SortM),
%         format("<table border=1><tr><td>&nbsp;", []), 
% 	output_row(Symbols), 
% 	output_html_table1(SortM).
% 
% output_html_table1([]). 
% output_html_table1([t(S,I,O,D,NS)|Rest]) :-
% 	format("<tr><td><a class=heading>~k</a>", [S]), 
% 	output_html_table2(S,[t(S,I,O,D,NS)|Rest]).
% 	
% output_html_table2(S, [t(S,_I,O,D,NS)|Rest]) :-
% 	format("<td><a>~k ~k ~k</a>", [O, D, NS]), 
% 	output_html_table2(S, Rest). 
% 
% output_html_table2(State, [t(S,I,O,D,NS)|Rest]) :-
% 	State \== S, % Finished this row
% 	format("~n"),
% 	output_html_table1([t(S,I,O,D,NS)|Rest]). 
% 	
% output_html_table2(_State, []) :-
% 	format("~n</table>~n"). 
% 
% machine_to_dot(M, FileName) :-
%     tell(FileName),
%     convert_to_dot(M), 
%     told.
% 
% convert_to_dot(M) :-
%     format("digraph example1 {~nrankdir=LR;~nnode [shape = circle];~n", []),
%     output_machine_to_dot(M), 
%     format("}~n", []),
%     true. 
% 
% output_machine_to_dot([]).
% output_machine_to_dot([t(S, I, O, D, N)|Rest]) :-
%     capital(D, CD), 
%     format("~k -> ~k [label = ""~d~d~s"" ];~n", [S,N,I,O,CD]), 
%     output_machine_to_dot(Rest).
% 
% capital(l,"L").
% capital(r,"R").
% 
% dots :-
%     Dir = "C:\\Users\\User\\OneDrive - RMIT University\\Research\\Papers\\busybeaver\\Diagrams\\Monsters\\",
%     monsters_to_dot(Dir).
% 
% monsters_to_dot(Dir) :-
%     between(1,100,N), 
%     monster(N, M),
%     string_concat(Dir, "monster", Temp),
%     number_string(N, Num), string_concat(Temp, Num, Temp2), string_concat(Temp2, ".dot", Name),
%     atom_string(FileName, Name), 
%     tell(FileName),
%     convert_to_dot(M),
%     told,
%     fail.
% monsters_to_dot(_).
% 
% monsters_to_csv :-
%     Dir = "C:\\Users\\User\\OneDrive - RMIT University\\Software\\busybeaver\\Code\\Refactor\\",
%     % Dir = "C:\\Users\\e24991\\Documents\\Refactor\\",
%     string_concat(Dir, "monsters.csv", Name), atom_string(FileName, Name),
%     tell(FileName),
%     monsters_to_csv_each, 
%     told. 
% monsters_to_csv_each :-
%     between(1,100,N),
%     monster(N,M),
%     number_of_states(M, States), number_of_symbols(M, Symbols),
%     M = [t(a,0,1,r,b)|RestM],
%     format("~d,~d,~d,", [N, States, Symbols]),
%     output_to_sheet(RestM), 
%     fail.
% monsters_to_csv_each.
% 
% output_to_sheet([]) :- nl. % Do we need to pad here?
% output_to_sheet([T|Rest]) :- convert_transition(T, S), format("~s,", [S]), output_to_sheet(Rest). 

% % POwershell:
% % cd C:\Users\User\'OneDrive - RMIT University'\Research\Papers\busybeaver\Diagrams\Monsters
% % $nums = 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100
% foreach ($n in $nums) { C:\'Program Files'\Graphviz\bin\dot -Tpng -o monster$n.png monster$n.dot }

