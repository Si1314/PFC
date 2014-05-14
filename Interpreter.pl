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

interpreter(EntryFile, OutFile):- 
	interpreter(EntryFile, OutFile, -3, 3, 10). % Defaults

interpreter(EntryFile, OutFile, Inf, Sup, MaxDepth):- 
	retractall(inf(_)),
	assert(inf(Inf)),
	retractall(sup(_)),
	assert(sup(Sup)),
	retractall(maxDepth(_)),
	assert(maxDepth(MaxDepth)),

	findall((N,L),interpreterAux(EntryFile,N,L),V),
	%interpreterAux(EntryFile, N,L),
	%write(V), write('\n'),
	open(OutFile, write, Stream, []),

    writeList(Stream,V),
   	%writeList(Stream,(N,L)),
    close(Stream).

interpreterAux(EntryFile,LabelTableNames, LabelTableValues):-
	load_xml_file(EntryFile, Program),

	removeEmpty(Program,GoodProgram),
	retractall(program(_)),
	assert(program(GoodProgram)),
	execute([],GoodProgram,ExitTable),

	labelList(ExitTable,LabelTableNames,LabelTableValues),
	once(label(LabelTableValues)).
	%label(LabelTableValues).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%%%%%%%%%%%
					% execute %
					%%%%%%%%%%%

% Recorre la lista de instrucciones una a una para ir ejecutándolas

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

execute(Entry,[],Entry):-!.

execute(Entry,[('while',_,[C,('body',_,B)])|RestInstructios],Out) :-!,
	maxDepth(N),
	step(Entry,('while',_,[C,('body',_,B)]),N,Out1),
	execute(Out1,RestInstructios,Out).

execute(Entry,[('for',_,[V,C,A,('body',_,B)])|RestInstructios],Out) :-!,
	maxDepth(N),
	step(Entry,('for',_,[V,C,A,('body',_,B)]),N,Out1),
	execute(Out1,RestInstructios,Out).

execute(Entry,[('function',Data,BodyFunction)|_],Out) :-!,
	step(Entry,('function',Data,BodyFunction),Out). %para que solo haga el main

execute(Entry,[Instruction|RestInstructios],Out) :-
	step(Entry,Instruction,Out1),
	execute(Out1,RestInstructios,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%%%%%%%%
					% STEP %
					%%%%%%%%

% Ejecuta una instrucción en concreto

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

step(Entry,('function',[_,_=void],FuncionBody),Out) :- !,
	apila(Entry,Entry1),
	execute(Entry1,FuncionBody,Out).

step(Entry,('function',[_,_=ExitValue],FuncionBody),Out) :- !,
	apila(Entry,Entry1),
	getTuple(ExitValue,Tuple),
	add(Entry1,Tuple,Out1),
	execute(Out1,FuncionBody,Out).

step(Entry,('param',[_=int,_=ParamName],ParamBody),Out) :- !,
	inf(X), sup(Y),
	Value in X..Y,
	add(Entry,(int,ParamName,Value),Out1),
	execute(Out1,ParamBody,Out).

step(Entry,('param',[_=ParamType,_=ParamName],ParamBody),Out) :- !,
	add(Entry,(ParamType,ParamName,_),Out1),
	execute(Out1,ParamBody,Out).

step(Entry,('body',_,Body),Out) :- !,
	apila(Entry, Out1),
	execute(Out1,Body,Out2),
	desapila(Out2, Out).

step(Entry,('declarations',_,Body),Out) :- !,
	execute(Entry,Body,Out).

step(Entry,('declaration',[_=int,_=Name],[(const,[value=Value],_)]),Out):- !,
	atom_number(Value,Value1),
	add(Entry,(int,Name,Value1),Out).

step(Entry,('declaration',[_=int,_=Name],DecBody),Out):- !,
	inf(X), sup(Y),
	Value in X..Y,
	add(Entry,(int,Name,Value),Out1),
	execute(Out1,DecBody,Out).

step(Entry,('declaration',[_=Type,_=Name],DecBody),Out):- !,
	add(Entry,(Type,Name,_),Entry1),
	execute(Entry1,DecBody,Out).

step(Entry,('assignment',[_=Name],[AssigBody]),Out) :- !,
	resolveExpression(Entry,AssigBody,Value),
	update(Entry,(Name,Value),Out).

step(Entry,('assigmentOperator',[_=Name],[AssigBody]),Out) :- !,
	resolveExpression(Entry,AssigBody,Value),
	update(Entry,(Name,Value),Out).

% IF -> THEN
step(Entry,('if',_,[Condition,('then',_,Then),_]),Out):-
	resolveExpression(Entry,Condition,'true'),
	apila(Entry,Out1),
	execute(Out1,Then,Out2),
	desapila(Out2, Out).

% IF -> ELSE
step(Entry,('if',_,[_,_,('else',_,Else)]),Out):- !,
	apila(Entry,Out1),
	execute(Out1,Else,Out2),
	desapila(Out2, Out).

step(Entry,('if',_,_),Entry):- !.

step(Entry,('return',_,[Body]),Out):-!,
	resolveExpression(Entry,Body,Result),
	getTuple(Tuple),
	add(Entry,Tuple,Out1),
	update(Out1,(ret,Result),Out2),
	updateReturnValue(Out2,Out).
	
% FOR
step(Entry,('for',_,_),0,Entry):-!.

step(Entry,('for',_,[Variable,Condition,Advance,('body',_,ForBody)]),N,Out):-
	variableAdvance(Entry,Variable,VariableName,Entry1),
	resolveExpression(Entry1,Condition,true),
	execute(Entry1,ForBody,Out1),
	execute(Out1,[Advance],Out2),
	N1 is N - 1,
	step(Out2,('for',_,[VariableName,Condition,Advance,('body',_,ForBody)]),N1,Out).

step(Entry,('for',_,_),_,Entry):-!.

% WHILE
step(Entry,('while',_,_),0,Entry):-!.

step(Entry,('while',_,[Condition,('body',_,WhileBody)]),N,Out):-
	resolveExpression(Entry,Condition,true),
	execute(Entry,WhileBody,Out1),
	N1 is N - 1,
	step(Out1,('while',_,[Condition,('body',_,WhileBody)]),N1,Out).

step(Entry,('while',_,_),_,Entry):-!.

step(Entry,_,_,Entry).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

						%%%%%%%%%%%%%%%%%%%
						%   EXPRESSIONS   %
						%%%%%%%%%%%%%%%%%%%

% Resuelve la expresión que se le pasa, puede ser operación:
% 'binaria', 'unaria', 'llamada', 'buleana' ó 'aritmética' ...

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resolveExpression(Entry,('binaryOperator',Operator,[X,Y]),Result):-
	getContent(Operator,Op),
	resolveExpression(Entry,X, Operand1),
	resolveExpression(Entry,Y, Operand2),
	work(Op, Operand1, Operand2,Result).

resolveExpression(Entry,('unaryOperator',[name=Name,Operator],[]),Result):-!,
	resolveExpression(Entry,('binaryOperator',[Operator],[('variable',[_=Name],[]),('constValue',1,[])]),Result).

resolveExpression(Entry,('unaryOperator',[name=Name,Operator],[Y]),Result):-!,
	resolveExpression(Entry,('binaryOperator',[Operator],[('variable',[_=Name],[]),Y]),Result).

resolveExpression(Entry,('variable',[_=OperandName],_),OperandValue):-
	getValue(Entry,OperandName,OperandValue).

resolveExpression(_,('constValue',Value,_),Value).

resolveExpression(_,('const',[_=Value],_),Result):-
	atom_number(Value,Result).

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


work('<', Op1,Op2,true):- Op1 #< Op2.
work('<', _,_,false).

work('<=', Op1,Op2, true):- Op1 #=< Op2.
work('<=', _,_,false).

work('>=', Op1,Op2,true):- Op1 #>= Op2.
work('>=', _,_,false).

work('>', Op1,Op2,true):- Op1 #> Op2.
work('>', _,_,false).

work('==', Op1,Op2,true):- Op1 #= Op2.
work('==', _,_,false).

work('!=', Op1,Op2,true):- Op1 #\= Op2.
work('!=', _,_,false).

work('&&', Op1,Op2,true):- Op1 #/\ Op2.
work('&&', _,_,false).


						%%%%%%%%%%%%%%%%%%
						%   Arithmetic   %
						%%%%%%%%%%%%%%%%%%

work('+', Op1,Op2,Z):- !, Z #= Op1 + Op2.
work('-', Op1,Op2,Z):- !, Z #= Op1 - Op2.
work('*', Op1,Op2,Z):- !, Z #= Op1 * Op2.
work('/', _,0,_):- !, fail.
work('/', Op1,Op2,Z):- !, Z #= Op1 / Op2.