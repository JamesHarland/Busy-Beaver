:- include(emulate).
:- include(machines). 

test(Number, States, Symbols, Macro) :- 

    monster(Number,M), 

    % write out each machine's results in detail to a separate file
    
    number_codes(Number, Num), append("machine", Num, Temp), append(Temp, ".html", Name), atom_codes(FileName, Name), 
    tell(FileName),

    % Set up file
    format("<html>~n<head>~n  <title>Machine ~d  Results</title>~n<style type=""text/css"">@import ""http://www.cs.rmit.edu.au/~~jah/harland.css""; </style>~n</head>~n<body>~n~n<h2>Machine ~d Results</h2>~n~n", [Number, Number]), 

    get_time(T1), format("<a>", []), 
    emulate_bb(M,tens(40000),macro(Macro), [otter], OtterOnes, OtterHops, OtterStatus, OtterOutputs), format("</a><br>~n", []),  
    get_time(T2), OtterTime is T2-T1, 
    
    get_time(T3), format("<a>", []), 
    emulate_bb(M,tens(40000),macro(Macro), [maxsteps(5000000)], OtterLessOnes, OtterLessHops, OtterLessStatus, OtterLessOutputs), format("</a><br>~n", []),  
    get_time(T4), OtterLessTime is T4-T3, 

    (OtterLessStatus \== halts -> (get_time(T5), format("<a>", []), 
				  emulate_bb(M,OtterLessHops,macro(Macro), [otter], OtterExtraOnes, OtterExtraHops, OtterExtraStatus, OtterExtraOutputs), format("</a><br>~n", []),   
				  get_time(T6), OtterExtraTime is T6-T5) ; true), 
    member(steps(OSteps), OtterOutputs), 
    member(otters(Otters), OtterOutputs), 
    member(ottersteps(OtterSteps), OtterOutputs), 
    member(config(Left, State, Item, Dir, Right), OtterOutputs),
    member(machine(_,MMM,_), OtterOutputs), 

    origin(Number, URL, Comments), 

      % Output machine details

    format("<a>Machine ~d: ", [Number]), write(M), format("</a><br>~n~n" , []), 
    convert_machine(M, MM), 
    format("<a>String format: ~d~d~s ", [States,Symbols,MM]), format("</a><br>~n~n" , []), 
    format("<a>Origin: <a href=""~s"">~s</a>&nbsp;<a> ~s</a><br>~n", [URL, URL, Comments]), 
    format("<a class = ""heading"">Table: ~n", []), 

	output_html_table(Symbols, M), 
    format("<a>Status: ", []), write(OtterStatus), format("</a><br>~n" , []), 

    format("<a>Final Configuration: ", []), display_config(config(Left, State, Item, Dir, Right)), format("</a><br>~n", []), 

    format("<a>Ones: ~d </a><br>~n", [OtterOnes]), 
    format("<a>Hops: ~d </a><br>~n", [OtterHops]),
    format("<a>Steps: ~d </a><br>~n", [OSteps]), 
    format("<a>Otters: ~d </a><br>~n", [Otters]), 
    format("<a>OtterSteps: ~d </a><br>~n", [OtterSteps]), 
%    format("<a>OtterSteps percentage: ~1f </a><br>~n", [OtterSteps*100/OtterHops]), 
    format("<a>Otter Time: ~f<a><br>~n", [OtterTime]),  

    format("<a>OtterLess Hops: ~d </a><br>~n", [OtterLessHops]), 
    format("<a>OtterLess Time: ~f<a><br>~n", [OtterLessTime]),  

    ( OtterLessStatus \== halts -> 
	( format("<a>OtterExtra Hops: ~d </a><br>~n", [OtterExtraHops]), format("<a>OtterExtra Time: ~f<a><br>~n", [OtterExtraTime])) 
        ; true ), 

    format("<a>Macro Size: ~d</a><br>~n", [Macro]), !, ttyflush, 
    format("<a>Macro Machine: ", []), write(MMM), format("</a><br>~n", []), !, ttyflush, 

    % Finish up file
    format("</body>~n</html>~n", []), 
    
    told, 
    % Now output information for summary file

    format("result(~d,~d,~d,", [Number,States,Symbols]), 
    print_bignum1(internal, OtterOnes), format(",", []), 
    print_bignum1(internal, OtterHops), format(",", []), 
    print_bignum1(internal, OSteps), format(",", []), 
    print_bignum1(internal, Otters), format(",", []),
    print_bignum1(internal, OtterSteps), format(",", []),  
    format("~f", OtterTime),  format(",", []),  
    print_bignum1(internal, OtterLessHops), format(",", []),  
    format("~f", OtterLessTime),  format(",", []),  

   ( OtterLessStatus \== halts -> 
 	( print_bignum1(internal, OtterExtraHops), format(","), format("~f", OtterExtraTime), format(").~n", []))
         ; format("none,none).~n", []) ), 
   true. 

allotter :-

% Set up internal results file
    tell('Otter-results.pl'), 

    get_time(T1), 

        test(1,2,4,2), !, 
	test(2,2,4,2), !, 
	test(3,2,4,2), !, 
	test(4,2,4,2), !, 
 	test(5,3,3,3), !, 
 	test(6,3,3,2), !, 
  	test(7,3,3,2), !, 
  	test(8,3,3,2), !, 
  	test(9,3,3,2), !, 
  	test(10,3,3,2), !, 
  	test(11,3,3,2), !, 
  	test(12,3,3,2), !, 
  	test(13,3,3,2), !, 
 	test(14,3,3,2), !, 
  	test(15,3,3,2), !, 
  	test(16,3,3,2), !, 
  	test(17,3,3,2), !, 
  	test(18,5,2,3),  !, 
  	test(19,5,2,3),  !, 
  	test(20,5,2,3),  !, 
  	test(21,5,2,3),  !, 
  	test(22,5,2,3),  !, 
  	test(23,5,2,3),  !, 
  	test(24,5,2,4),  !, 
  	test(25,5,2,4),  !, 
  	test(26,5,2,4),  !, 
  	test(27,2,5,2),  !, 
  	test(28,2,5,2),  !, 
  	test(29,2,5,2),  !, 
  	test(30,2,5,2),  !, 
  	test(31,2,5,2),  !, 
  	test(32,2,5,2),  !, 
  	test(33,2,5,1),  !, 
  	test(34,2,5,1),  !, 
  	test(35,2,5,1),  !, 
  % 	test(36,2,5,3),  !, % Do this one at the end .... 
  	test(37,2,5,2),  !, 
  	test(38,2,5,2),  !, 
  	test(39,2,5,2),  !, 
  	test(40,2,5,2),  !, 
  	test(41,2,5,1),  !, 
  	test(42,2,5,1),  !, 
  	test(43,2,5,1),  !, 
  	test(44,2,5,1),  !, 
  	test(45,2,5,1),  !, 
  	test(46,2,5,1),  !, 
  	test(47,2,5,2),  !, 
  	test(48,2,5,2),  !, 
  	test(49,2,5,2),  !, 
  	test(50,2,5,2),  !, 
  	test(51,6,2,3),  !, 
  	test(52,6,2,4),  !, 
  	test(53,6,2,3),  !, 
  	test(54,6,2,4),  !, 
  	test(55,6,2,4),  !, 
  	test(56,6,2,2),  !, 
  	test(57,6,2,4),  !, 
  	test(58,6,2,4),  !, 
  	test(59,6,2,4),  !, 
  	test(60,6,2,4),  !, 
  	test(61,6,2,4),  !, 
  	test(62,6,2,4),  !, 
  	test(63,6,2,4),  !, 
  	test(64,6,2,4),  !, 
  	test(65,6,2,4),  !, 
  	test(66,6,2,4),  !, 
  	test(67,6,2,3),  !, 
  	test(68,6,2,2),  !, 
  	test(69,6,2,6),  !, 
  	test(70,6,2,4),  !, 
  	test(71,6,2,2),  !, 
  	test(72,6,2,6),  !, 
  	test(73,3,4,6),  !, 
  	test(74,3,4,1),  !, 
  	test(75,3,4,2),  !, 
  	test(76,3,4,2),  !, 
  	test(77,3,4,2),  !, 
  	test(78,3,4,2),  !, 
  	test(79,3,4,2),  !, 
  	test(80,3,4,2),  !, 
  	test(81,3,4,2),  !, 
  	test(82,4,3,2),  !, 
  	test(83,4,3,2),  !, 
  	test(84,4,3,2),  !, 
  	test(85,4,3,2),  !, 
  	test(86,4,3,2),  !, 
  	test(87,4,3,2),  !, 
  	test(88,4,3,2),  !, 
  	test(89,4,3,2),  !, 
  	test(90,4,3,2),  !, 
  	test(91,4,3,2),  !, 
  	test(92,2,6,1),  !, 
  	test(93,2,6,1),  !, 
  	test(94,2,6,2),  !, 
  	test(95,2,6,1),  !, 
  	test(96,2,6,2),  !, 
  	test(97,2,6,2),  !, 
  	test(98,2,6,2),  !, 
 	test(99,7,2,1),  !, 
 	test(100,8,2,1),  !, 
  % 	test2(37,2,5,12),  !, % If all the others are done, then ...     

    % Finish up summary file
    told,

    get_time(T2), Time is T2-T1, 
    tell('Otter-time.txt'),  output_time(Time), told, 
    true. 

otterhtml :- 
	% Convert Otter-results.pl to Otter-results.html
	see('Otter-results.pl'),tell('Otter-results.html'), 
	format("<html>~n<head>~n  <title>Otter Results</title>~n<style type=""text/css"">@import ""http://www.cs.rmit.edu.au/~~jah/harland.css""; </style>~n</head>~n<body>~n~n<h2>Otter Results</h2>~n~n", []), 
	
    format("<p>~n<table border = 1>~n<tr><td> <a class=""heading"">Number</a>~n<td> <a class=""heading"">Dimension</a>~n<td> <a class=""heading"">Ones</a>~n<td> <a class=""heading"">Hops</a>~n<td> <a class=""heading"">Steps</a>~n<td> <a class=""heading"">Otters</a>~n<td> <a class=""heading"">Otter Steps</a>~n<td> <a class=""heading"">Time</a>~n<td> <a class=""heading"">OtterLess Hops</a>~n<td> <a class=""heading"">OtterLess Time</a>~n<td> <a class=""heading"">OtterExtra Hops</a>~n<td> <a class=""heading"">OtterExtra Time</a>~n~n", []),

	read(Term),
	process_html_result(Term).

process_html_result(end_of_file) :- seen, format("</table>~n</body>~n</html>~n", []), told. 
process_html_result(Term) :-
	Term \== end_of_file, !, %  writeln(Term), 
	Term = result(Number, States, Symbols, OtterOnes, OtterHops, OSteps, Otters, OtterSteps, OtterTime, OtterLessHops, OtterLessTime, OtterExtraHops, OtterExtraTime), 
	% format("Here!~n", []), ttyflush, 
	number_codes(Number, Num), append("machine", Num, Temp), append(Temp, ".html", Name),
	format("<tr><td><a href=""~s""> ~d </a><td><a> ~dx~d </a><td><a> ", [Name,Number,States,Symbols]), ttyflush, 
	print_digits(OtterOnes),  format(" </a><td><a> ", []), ttyflush, 
	print_digits(OtterHops),  format(" </a><td><a> ", []), ttyflush, 
	print_digits(OSteps),     format(" </a><td><a> ", []), ttyflush, 
	print_digits(Otters),     format(" </a><td><a> ", []), ttyflush, 
	print_digits(OtterSteps), format(" </a><td><a> ", []), ttyflush, 
	format("~2f", OtterTime),       format(" </a><td><a> ", []), ttyflush, 
	print_digits(OtterLessHops), format(" </a><td><a> ", []), ttyflush, 
	format("~2f", OtterLessTime),       format(" </a><td><a> ", []), ttyflush, 
 
	( OtterExtraHops \== none -> 
 	( print_digits(OtterExtraHops), format(" </a><td><a> ", []), format("~2f", OtterExtraTime), format(" </a>~n", []))
         ; format(" -- </a><td><a> -- </a>~n", []) ), 

	 read(NewTerm),
	 process_html_result(NewTerm). 

otterlatex :- 
	% Convert Otter-results.pl to a latex form that can be put in the paper. This only does the two tabular environment, and doesn't split them into two parts.  
	tell('results.tex'), 
	table1, 
	table2,
	told, 
	true. 

table1 :- 
	see('Otter-results.pl'), 
        format("\\begin{tabular}{lllllllll}~n\\textbf{No.} & \\textbf{Dim.} & \\textbf{Ones} & \\textbf{Hops} & \\textbf{Steps} & \\textbf{Otters} & \\textbf{Otter Steps} & \\textbf{Otter \\%} & \\textbf{Time}\\\\~n", []), 

	read(Term),
	process_table1(Term).

table2 :- 
	see('Otter-results.pl'), 
	format("\\begin{tabular}{llllllll}~n\\textbf{No.} & \\textbf{Dim.} & \\textbf{Hops1} &  \\textbf{Time1} & \\textbf{Hops2} & \\textbf{Time2} & \\textbf{Hops3} & \\textbf{Time3}\\\\~n", []), 

	read(Term),
	process_table2(Term).

process_table1(end_of_file) :- seen, format("\\end{tabular}~n", []). 
process_table1(Term) :-
	Term \== end_of_file, !, 
	Term = result(Number, States, Symbols, OtterOnes, OtterHops, OSteps, Otters, OtterSteps, OtterTime, _, _, _, _), 
	
	format("~d & ~dx~d & ", [Number,States,Symbols]),
	print_digits(OtterOnes),  format(" & ", []), 
	print_digits(OtterHops),  format(" & ", []), 
	print_digits(OSteps),     format(" & ", []), 
	print_digits(Otters),     format(" & ", []), 
	print_digits(OtterSteps), format(" & ", []), 
	print_otter_percentage(OtterHops, OtterSteps), format(" & ", []), 
	format("~2f", OtterTime),  format(" \\\\~n", []), 
	
	 read(NewTerm),
	 process_table1(NewTerm). 

process_table2(end_of_file) :- seen, format("\\end{tabular}~n", []). 
process_table2(Term) :-
	Term \== end_of_file, !, 
	Term = result(Number, States, Symbols, _, OtterHops, _, _, _, OtterTime, OtterLessHops, OtterLessTime, OtterExtraHops, OtterExtraTime), 
	
	format("~d & ~dx~d & ", [Number,States,Symbols]),
	print_digits(OtterHops),  format(" & ", []), 
	format("~2f", OtterTime),  format(" & ", []), 
	print_digits(OtterLessHops), format(" & ", []), 
	format("~2f", OtterLessTime),       format(" & ", []), 
 
	( OtterExtraHops \== none -> 
 	( print_digits(OtterExtraHops), format(" & ", []), format("~2f", OtterExtraTime), format("\\\\~n", []))
         ; format(" -- &  -- \\\\~n", []) ), 

	 read(NewTerm),
	 process_table2(NewTerm). 

print_digits(Num) :- integer(Num), format("~d",[Num]).
print_digits(digits(Num)) :- integer(Num), format("(~d digits)",[Num]).

print_otter_percentage(OtterHops, OtterSteps) :-
	integer(OtterHops), integer(OtterSteps), X is 100*OtterSteps/OtterHops, format("~2f", [X]).
print_otter_percentage(OtterHops, OtterSteps) :-
	\+ (integer(OtterHops), integer(OtterSteps)), format("$>$99", []).

output_time(Time) :-  
    H is Time / 3600, Hours is floor(H), 
    M is float_fractional_part(H) * 60, Minutes is floor(M), 
    Seconds is floor(float_fractional_part(M)*60), 
    format("Total time taken is ~d hours ~d minutes ~d seconds~n", [Hours, Minutes, Seconds]), 
    true. 
