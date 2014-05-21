%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
					
					%%%%%%%%%%%%%%%%%%%%%%%%%%%
					%       INTERPRETER       %
					%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-use_module(library(sgml)).
:-use_module(library(clpfd)).
:-use_module(library(sgml_write)).


:- include('VariablesTable.pl').
:- include('AuxiliaryFunctions.pl').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%%%%%%%%%%%%%%%
					% interpreter %
					%%%%%%%%%%%%%%%

% First you have to keep this file in a folder called "PFC"
% Then open swi Prolog and write "interpreter('salida4.xml','output4.xml')." to test it

% Funcion principal, se le puede meter el fichero de entrada y salida, o incluirle también
% las variables inf, sup y maxDepth, si no se incluyen se ponen por defecto a: -3, 3 y 10

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interpreter(EntryFile, OutFile, FunctionName):- 
	interpreter(EntryFile, OutFile, -3, 3, 10, FunctionName). % Defaults

interpreter(EntryFile, OutFile, Inf, Sup, MaxDepth, FunctionName):- 
	assert(inf(0)),
	assert(sup(0)),
	assert(maxDepth(0)),
	assert(program(0)),

	retractall(inf(_)),
	assert(inf(Inf)),
	retractall(sup(_)),
	assert(sup(Sup)),
	retractall(maxDepth(_)),
	assert(maxDepth(MaxDepth)),

	findall((N,L),interpreterAux(EntryFile,N,L, FunctionName),V),
	%interpreterAux(EntryFile, N,L),
	%write(V), write('\n'),
	open(OutFile, write, Stream, []),

    writeList(Stream,V),
   	%writeList(Stream,(N,L)),
    close(Stream).

interpreterAux(EntryFile,LabelTableNames, LabelTableValues, FunctionName):-
	load_xml_file(EntryFile, Program),

	removeEmpty(Program,GoodProgram),
	retractall(program(_)),
	assert(program(GoodProgram)),
	write(GoodProgram),write('\n'),
	write(FunctionName),write('\n'),
	lookForFunction(GoodProgram,FunctionName,Function),
	
	
	write(Function),

	state(InitS,[],[],[],[]),
	%execute([],Function,ExitTable),
	execute(InitS,Function,EndS),
	state(EndS,ExitTable,Cinput,Coutput,Trace),

	labelList(ExitTable,LabelTableNames,LabelTableValues),
	once(label(LabelTableValues)),
	write(Cinput),
	write(Coutput),
	write(Trace).
	%label(LabelTableValues).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%%%%%%%%%%%
					% execute %
					%%%%%%%%%%%

% Recorre la lista de instrucciones una a una para ir ejecutándolas

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

execute(Entry,[],Entry):-!.

execute(Entry,[('while',Data,[C,('body',_,B)])|RestInstructios],Out) :-!,
	maxDepth(N),
	step(Entry,('while',Data,[C,('body',_,B)]),N,Out1),
	execute(Out1,RestInstructios,Out).

execute(Entry,[('for',Data,[V,C,A,('body',_,B)])|RestInstructios],Out) :-!,
	maxDepth(N),
	step(Entry,('for',Data,[V,C,A,('body',_,B)]),N,Out1),
	execute(Out1,RestInstructios,Out).

%execute(Entry,[('function',Data,BodyFunction)|_],Out) :-!,
%	step(Entry,('function',Data,BodyFunction),Out). %para que solo haga el main

execute(Entry,[Instruction|RestInstructios],Out) :-
	step(Entry,Instruction,Out1),
	execute(Out1,RestInstructios,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%%%%%%%%
					% STEP %
					%%%%%%%%

% Ejecuta una instrucción en concreto

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

step(EntryS,('function',[_,_=void,_=Line],FunctionBody),OutS) :- !,
	state(EntryS,Table,Cin,Cout,Trace),
		apila(Table,Table1),
		append(Trace,[Line],Trace1),
	state(EntryS1,Table1,Cin,Cout,Trace1),
	write(f),
	execute(EntryS1,FunctionBody,OutS).

step(EntryS,('function',[_,_=ExitValue,_=Line],FuncionBody),OutS) :- !,
	state(EntryS,Table,Cin,Cout,Trace),
		apila(Table,Table1),
		getTuple(ExitValue,Tuple),
		add(Table1,Tuple,Table2),
		append(Trace,[Line],Trace1),
	state(EntryS1,Table2,Cin,Cout,Trace1),

	execute(EntryS1,FuncionBody,OutS).

step(EntryS,('param',[_=int,_=ParamName],ParamBody),OutS) :- !,
	inf(X), sup(Y),
	Value in X..Y,

	state(EntryS,Table,Cin,Cout,Trace),
		add(Table,(int,ParamName,Value),Table1),
	state(EntryS1,Table1,Cin,Cout,Trace),

	execute(EntryS1,ParamBody,OutS).

step(EntryS,('param',[_=ParamType,_=ParamName],ParamBody),OutS) :- !,
	state(EntryS,Table,Cin,Cout,Trace),
		add(Table,(ParamType,ParamName,_),Table1),
	state(EntryS1,Table1,Cin,Cout,Trace),

	execute(EntryS1,ParamBody,OutS).

step(EntryS,('body',_,Body),OutS) :- !,
	state(EntryS,Table,Cin,Cout,Trace),
		apila(Table, Table1),
	state(EntryS1,Table1,Cin,Cout,Trace),

	execute(EntryS1,Body,EntryS2),

	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		desapila(Table2, Table3),
	state(OutS,Table3,Cin2,Cout2,Trace2).

step(EntryS,('declarations',_,Body),OutS) :- !,
	execute(EntryS,Body,OutS).

step(EntryS,('declaration',[_=int,_=Name,_=Line],[(const,[value=Value],_)]),OutS):- !,
	atom_number(Value,Value1),

	state(EntryS,Table,Cin,Cout,Trace),
		add(Table,(int,Name,Value1),Table1),
		append(Trace,[Line],Trace1),
	state(OutS,Table1,Cin,Cout,Trace1).

step(EntryS,('declaration',[_=int,_=Name,_=Line],DecBody),OutS):- !,
	inf(X), sup(Y),
	Value in X..Y,

	state(EntryS,Table,Cin,Cout,Trace),
		add(Table,(int,Name,Value),Table1),
		append(Trace,[Line],Trace1),
	state(EntryS1,Table1,Cin,Cout,Trace1),

	execute(EntryS1,DecBody,OutS).

step(EntryS,('declaration',[_=Type,_=Name,_=Line],DecBody),OutS):- !,
	state(EntryS,Table,Cin,Cout,Trace),
		add(Table,(Type,Name,_),Table1),
		append(Trace,[Line],Trace1),
	state(EntryS1,Table1,Cin,Cout,Trace1),

	execute(EntryS1,DecBody,OutS).

step(EntryS,('assignment',[_=Name,_=Line],[AssigBody]),OutS) :- !,
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),

	resolveExpression(EntryS1,AssigBody,Value,EntryS2),

	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		update(Table2,(Name,Value),Table3),
	state(OutS,Table3,Cin2,Cout2,Trace2).

step(EntryS,('assigmentOperator',[_=Name,_,_=Operator,_=Line],[AssigBody]),OutS) :- !,
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),

	resolveExpression(EntryS1,
		('binaryOperator',[Operator],
			[('variable',[_=Name],[]),AssigBody])
		,Value,EntryS2),

	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		update(Table2,(Name,Value),Table3),
	state(OutS,Table3,Cin2,Cout2,Trace2).

step(EntryS,('unaryOperator',[_=Name,_=Operator,_=Line],[]),OutS):-!,
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),

	resolveExpression(EntryS1,
		('binaryOperator',[Operator],
			[('variable',[_=Name],[]),
				('constValue',1,[])])
		,Value,EntryS2),

	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		update(Table2,(Name,Value),Table3),
	state(OutS,Table3,Cin2,Cout2,Trace2).

step(EntryS,('consoleOut',[_=Line],Expr),OutS):-!,
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),

	resolveExpression(EntryS1,Expr,Value,EntryS2),

	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		append(Cout2,[Value],Cout3),
	state(OutS,Table2,Cin2,Cout3,Trace2).

% IF -> THEN
step(EntryS,('if',[_=Line],[Condition,('then',_,Then),_]),OutS):-
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),

	resolveExpression(EntryS1,Condition,1,EntryS2),

	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		apila(Table2,Table3),
	state(EntryS3,Table3,Cin2,Cout2,Trace2),

	execute(EntryS3,Then,EntryS4),

	state(EntryS4,Table4,Cin4,Cout4,Trace4),
		desapila(Table4, Table5),
	state(OutS,Table5,Cin4,Cout4,Trace4).

% IF -> ELSE
step(EntryS,('if',[_=Line],[_,_,('else',_,Else)]),OutS):- !,
	state(EntryS,Table,Cin,Cout,Trace),
		apila(Table,Table1),
		append(Trace,[Line],Trace1),
	state(EntryS1,Table1,Cin,Cout,Trace1),

	execute(EntryS1,Else,EntryS2),

	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		desapila(Table2, Table3),
	state(OutS,Table3,Cin2,Cout2,Trace2).

step(EntryS,('if',[_=Line],_),OutS):- !,
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[Line],Trace1),
	state(OutS,Table,Cin,Cout,Trace1).

step(EntryS,('return',[_=Line],[Body]),OutS):-!,
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),

	resolveExpression(EntryS1,Body,Result,EntryS2),

	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		getTuple(Tuple),
		add(Table2,Tuple,Table3),
		update(Table3,(ret,Result),Table4),
		updateReturnValue(Table4,Table5),
	state(OutS,Table5,Cin2,Cout2,Trace2).
	
% FOR
step(EntryS,('for',[_=Line],_),0,OutS):-!,
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[Line],Trace1),
	state(OutS,Table,Cin,Cout,Trace1).

step(EntryS,('for',[_=Line],[Variable,Condition,Advance,('body',_,ForBody)]),N,OutS):-
	state(EntryS,Table,Cin,Cout,Trace),
		variableAdvance(Table,Variable,VariableName,Table1),
		append(Trace,[Line],Trace1),
	state(EntryS1,Table1,Cin,Cout,Trace1),

	resolveExpression(EntryS1,Condition,1,EntryS2),
	execute(EntryS2,ForBody,EntryS3),
	execute(EntryS3,[Advance],EntryS4),

	N1 is N - 1,
	step(EntryS4,('for',[_=Line],[VariableName,Condition,Advance,('body',_,ForBody)]),N1,OutS).

step(EntryS,('for',[_=Line],_),_,OutS):-!,
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[Line],Trace1),
	state(OutS,Table,Cin,Cout,Trace1).

% WHILE
step(EntryS,('while',[_=Line],_),0,OutS):-!,
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[Line],Trace1),
	state(OutS,Table,Cin,Cout,Trace1).

step(EntryS,('while',[_=Line],[Condition,('body',_,WhileBody)]),N,OutS):-
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),

	resolveExpression(EntryS1,Condition,1,EntryS2),
	execute(EntryS2,WhileBody,EntryS3),

	N1 is N - 1,
	step(EntryS3,('while',[_=Line],[Condition,('body',_,WhileBody)]),N1,OutS).

step(EntryS,('while',[_=Line],_),_,OutS):-!,
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[Line],Trace1),
	state(OutS,Table,Cin,Cout,Trace1).

step(EntryS,_,_,EntryS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

						%%%%%%%%%%%%%%%%%%%
						%   EXPRESSIONS   %
						%%%%%%%%%%%%%%%%%%%

% Resuelve la expresión que se le pasa, puede ser operación:
% 'binaria', 'unaria', 'llamada', 'buleana' ó 'aritmética' ...

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resolveExpression(EntryS,('notOperator',_,Expr),NotResult,OutS):-
	resolveExpression(EntryS,Expr,Result,OutS),
	not(Result,NotResult).

resolveExpression(EntryS,('signOperator',[type='-'],Expr),InvResult,OutS):-
	resolveExpression(EntryS,Expr,Result,OutS),
	work('*',-1,Result,InvResult).

resolveExpression(EntryS,('signOperator',[type='+'],Expr),Result,OutS):-
	write(signOp),
	resolveExpression(EntryS,Expr,Result,OutS).


resolveExpression(EntryS,('binaryOperator',Operator,[X,Y]),Result,OutS):-
	getContent(Operator,Op),
	resolveExpression(EntryS,X, Operand1,EntryS1),
	resolveExpression(EntryS1,Y, Operand2,OutS),
	work(Op, Operand1, Operand2,Result).

resolveExpression(EntryS,('variable',[_=OperandName],_),OperandValue,EntryS):-
	state(EntryS,Table,_,_,_),
	getValue(Table,OperandName,OperandValue).

resolveExpression(EntryS,('constValue',Value,_),Value,EntryS). % DEPRECATED?

resolveExpression(EntryS,('const',[_=Value],_),Result,EntryS):-
	atom_number(Value,Result).

resolveExpression(EntryS,('consoleIn',[_=int],_),Value,OutS):-
	state(EntryS,Table,Cin,Cout,Trace),
	inf(X), sup(Y),
	Value in X..Y,
	append(Cin,[Value],Cin1),
	state(OutS,Table,Cin1,Cout,Trace).

resolveExpression(Entry,('callFunction',[name=Name, type=Type],Params),ValueReturned):-!,
	apila(Entry,Entry1),
	addListParams(Entry1,Params,Out1),
	program(Program),
	lookForFunction(Program,Name,Type,Function),
	createListParams(Function,Body,ListParams),
	updateNames(Out1,ListParams,Out2),
	execute(Out2,Body,Out3),
	returnesValue(Out3,ValueReturned).


						%%%%%%%%%%%%
						%   BOOL   %
						%%%%%%%%%%%%

not(Value,1):-Value#=0.
not(Value,0):-Value#=1.

work('<', Op1,Op2,1):- Op1 #< Op2.
work('<', _,_,0).

work('<=', Op1,Op2, 1):- Op1 #=< Op2.
work('<=', _,_,0).

work('>=', Op1,Op2,1):- Op1 #>= Op2.
work('>=', _,_,0).

work('>', Op1,Op2,1):- Op1 #> Op2.
work('>', _,_,0).

work('==', Op1,Op2,1):- Op1 #= Op2.
work('==', _,_,0).

work('!=', Op1,Op2,1):- Op1 #\= Op2.
work('!=', _,_,0).

work('&&', Op1,Op2,1):- Op1 #/\ Op2.
work('&&', _,_,0).

work('||', Op1,Op2,1):- Op1 #\/ Op2.
work('||', _,_,0).



						%%%%%%%%%%%%%%%%%%
						%   Arithmetic   %
						%%%%%%%%%%%%%%%%%%

work('+', Op1,Op2,Z):- !, Z #= Op1 + Op2.
work('-', Op1,Op2,Z):- !, Z #= Op1 - Op2.
work('*', Op1,Op2,Z):- !, Z #= Op1 * Op2.
work('/', _,0,_):- !, fail.
work('/', Op1,Op2,Z):- !, Z #= Op1 / Op2.


