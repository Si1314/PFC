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
% Then open swi Prolog and write "interpreter('funcionesBasicas.xml','output4.xml',potencia)." to test it

% Funcion principal, se le puede meter el fichero de entrada y salida, o incluirle también
% las variables inf, sup y maxDepth, si no se incluyen se ponen por defecto a: -3, 3 y 10

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interpreter(EntryFile, OutFile, FunctionName):- 
	interpreter(EntryFile, OutFile, -5, 15, 5, FunctionName). % Defaults

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

	findall((N,L,T,Cin,Cout),interpreterAux(EntryFile,N,L,FunctionName,T,Cin,Cout),V),
	%interpreterAux(EntryFile, N,L),
	%write(V), write('\n'),
	open(OutFile, write, Stream, []),

    writeList(Stream,V),
   	%writeList(Stream,(N,L)),
    close(Stream).

interpreterAux(EntryFile,LabelTableNames, LabelTableValues,FunctionName,Trace,Cinput,Coutput):-
	load_xml_file(EntryFile, Program),

	removeEmpty(Program,GoodProgram),
	retractall(program(_)),
	assert(program(GoodProgram)),
	
	write(FunctionName),write('\n'),
	lookForFunction(GoodProgram,FunctionName,Function),
	
	state(InitS,[],[],[],[]),
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
	write('ffffff'),write('\n'),
	maxDepth(N),
	step(Entry,[('for',Data,[V,C,A,[('body',_,B)]])],N,Out1),
	execute(Out1,RestInstructios,Out).

%execute(Entry,[('function',Data,BodyFunction)|_],Out) :-!,
%	step(Entry,('function',Data,BodyFunction),Out). %para que solo haga el main

execute(Entry,[('return',D1,D2)|_],Out) :-!,
	write('estoy haciendo return'),write('\n'),
	step(Entry,('return',D1,D2),Out),!.

execute(Entry,[Instruction|RestInstructios],Out) :-
	write(Instruction),write('\n'),
	step(Entry,Instruction,Out1),
	execute(Out1,RestInstructios,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%%%%%%%%
					% STEP %
					%%%%%%%%

% Ejecuta una instrucción en concreto

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

step(EntryS,('function',[_,_=void,_=Line],FunctionBody),OutS) :- !,
	write('Metodo'),write('\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		apila(Table,Table1),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table1,Cin,Cout,Trace1),

	execute(EntryS1,FunctionBody,OutS).

step(EntryS,('function',[_,_=ExitValue,_=Line],FunctionBody),OutS) :- !,
	write('Funcion'),write('\n'),
	state(EntryS,Table,Cin,Cout,Trace),
	write('esta es la tabla nada más comenzandoA: '), write(Table),write('\n'),
		apila(Table,Table1),
		write('apilamos: '), write(Table1),write('\n'),
		getTuple(ExitValue,Tuple),
		add(Table1,Tuple,Table2),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table2,Cin,Cout,Trace1),
	write('esta es la tabla nada más comenzandoB: '), write(Table2),write('\n'),
	execute(EntryS1,FunctionBody,OutS).

step(EntryS,('params',_,Params),OutS) :- !,
	write('estoy en Params: '), write(Params),write('\n'),
	execute(EntryS,Params,OutS).

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
	write('declara1 '),write('\n'),
	atom_number(Value,Value1),
	state(EntryS,Table,Cin,Cout,Trace),
		%write(Value1),write('\n'),
		add(Table,(int,Name,Value1),Table1),
		%write(hechoAdd),write('\n'),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(OutS,Table1,Cin,Cout,Trace1).

step(EntryS,('declaration',[_=int,_=Name,_=Line],DecBody),OutS):- !,
	inf(X), sup(Y),
	Value in X..Y,
	write('declara2 '),write(Line),write('\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),

	resolveExpression(EntryS1,DecBody,Value,EntryS2),
	write('declara2 '),write(Line),write('\n'),

	state(EntryS2,Table2,Cin2,Cout2,Trace2),
	add(Table2,(int,Name,Value),Table3),
	state(OutS,Table3,Cin2,Cout2,Trace2).

step(EntryS,('declaration',[_=Type,_=Name,_=Line],DecBody),OutS):- !,
	write('declara3'),write(Line),write('\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		add(Table,(Type,Name,_),Table1),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table1,Cin,Cout,Trace1),

	execute(EntryS1,DecBody,OutS).

step(EntryS,('assignment',[_=Name,_=Line],AssigBody),OutS) :- !,
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),

	write('\n---\n'),
	resolveExpression(EntryS1,AssigBody,Value,EntryS2),
	write('\n???\n'),
	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		update(Table2,(Name,Value),Table3),
	state(OutS,Table3,Cin2,Cout2,Trace2).

step(EntryS,('assignmentOperator',[_=Name,_,_=Operator,_=Line],[AssigBody]),OutS) :- !,
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),

	resolveExpression(EntryS1,
		('binaryOperator',[Operator],
			[('variable',[_=Name],[]),AssigBody])
		,Value,EntryS2),

	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		update(Table2,(Name,Value),Table3),
	state(OutS,Table3,Cin2,Cout2,Trace2).

step(EntryS,('unaryOperator',[_=Name,_=Operator,_=Line],[]),OutS):-!,
	write('unary'),write(Line),write('\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
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
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),

	resolveExpression(EntryS1,Expr,Value,EntryS2),

	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		append(Cout2,[Value],Cout3),
	state(OutS,Table2,Cin2,Cout3,Trace2).

% IF -> THEN

step(EntryS,('if',[_=Line],[Condition,('then',_,Then)]),OutS):-
	step(EntryS,('if',[_=Line],[Condition,('then',_,Then),('else',_,[])]),OutS).

step(EntryS,('if',[_=Line],[Condition,('then',_,Then),_]),OutS):-
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),

	resolveExpression(EntryS1,Condition,1,EntryS2),
	write('\nTRUE IF\n\n'),
	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		apila(Table2,Table3),
	state(EntryS3,Table3,Cin2,Cout2,Trace2),

	execute(EntryS3,Then,EntryS4),

	state(EntryS4,Table4,Cin4,Cout4,Trace4),
		desapila(Table4, Table5),
	state(OutS,Table5,Cin4,Cout4,Trace4).

% IF -> ELSE
step(EntryS,('if',[_=Line],[_,_,('else',_,Else)]),OutS):- !,
	write('\n\nFALSE IF\n\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		apila(Table,Table1),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table1,Cin,Cout,Trace1),

	execute(EntryS1,Else,EntryS2),

	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		desapila(Table2, Table3),
	state(OutS,Table3,Cin2,Cout2,Trace2).

%step(EntryS,('if',[_=_],_),EntryS):- !.
%	state(EntryS,Table,Cin,Cout,Trace),
%		append(Trace,[' '],Space),
%		append(Space,[Line],Trace1),
%	state(OutS,Table,Cin,Cout,Trace1).

step(EntryS,('return',[_=Line],[Body]),OutS):-!,
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),

	write('\nTablaaaaaaaa:\n'), write(Table), write('\n'),

%	resolveExpression(EntryS1,Body,Result,EntryS2),
%	write('result: '),write(Result),write('\n'),
%	state(EntryS2,Table2,Cin2,Cout2,Trace2),
%		%getTuple(Tuple),
%		%add(Table2,Tuple,Table3),
%		update(Table2,(ret,Result),Table4),
%		updateReturnValue(Table4,Table5),
%	state(OutS,Table5,Cin2,Cout2,Trace2).

	resolveExpression(EntryS1,Body,Value,EntryS2),
	write('result: '),write(Value),write('<-\n'),
	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		update(Table2,(ret,Value),Table3),
	state(OutS,Table3,Cin2,Cout2,Trace2).
	
% FOR
step(EntryS,[('for',[_=Line],_)],0,OutS):-!,
	write('entraFor1'),
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(OutS,Table,Cin,Cout,Trace1).

step(EntryS,[('for',[_=Line],[Variable,Condition,Advance,[('body',_,ForBody)]])],N,OutS):-
	write('entraFor2A'),write('\n'),
	write(Variable),write('\n'),
	variableAdvance(EntryS,Variable,VariableName,EntryS1),
	%state(EntryS1,Table1,Cin1,Cout1,Trace1),
	%append(Trace1,[' '],Space),
	%append(Space,[Line],Trace2),
	%state(EntryS2,Table1,Cin1,Cout1,Trace2),
	write('entraFor2B'),write('\n'),
	resolveExpression(EntryS1,Condition,1,EntryS3),
	write('entraFor2C'),write('\n'),
	execute(EntryS3,ForBody,EntryS4),
	write('entraFor2D'),write('\n'),
	execute(EntryS4,[Advance],EntryS5),
	write('entraFor2E'),write('\n'),
	N1 is N - 1,
	write(N1),write('\n'),
	step(EntryS5,[('for',[_=Line],[VariableName,Condition,Advance,[('body',_,ForBody)]])],N1,OutS).

%step(EntryS,[('for',[_=Line],_)],_,OutS):-!,
%	write('entraFor3'),write('\n'),
%	state(EntryS,Table,Cin,Cout,Trace),
%		append(Trace,[' '],Space),
%		append(Space,[Line],Trace1),
%	state(OutS,Table,Cin,Cout,Trace1).

% WHILE
step(EntryS,('while',[_=Line],_),0,OutS):-!,
	write('*entraWhileUltimaVuelta*'),write('\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(OutS,Table,Cin,Cout,Trace1).

step(EntryS,('while',[_=Line],[Condition,('body',_,WhileBody)]),N,OutS):-
	write('*entraWhile*'),write('\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),

	resolveExpression(EntryS1,Condition,1,EntryS2),
	execute(EntryS2,WhileBody,EntryS3),

	N1 is N - 1,
	step(EntryS3,('while',[_=Line],[Condition,('body',_,WhileBody)]),N1,OutS).

step(EntryS,('while',[_=Line],_),_,OutS):-!,	% PARA QUE ESTA ESTO?????
	write('*entraWhileNoCondicion*'),write('\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(OutS,Table,Cin,Cout,Trace1).

step(EntryS,_,_,EntryS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

						%%%%%%%%%%%%%%%%%%%
						%   EXPRESSIONS   %
						%%%%%%%%%%%%%%%%%%%

% Resuelve la expresión que se le pasa, puede ser operación:
% 'binaria', 'unaria', 'llamada', 'buleana' ó 'aritmética' ...

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resolveExpression(Entry,[],_,Entry):-!.

resolveExpression(EntryS,('notOperator',_,[Expr]),NotResult,OutS):-
	resolveExpression(EntryS,Expr,Result,OutS),
	not(Result,NotResult).

resolveExpression(EntryS,('signOperator',[type='-'],[Expr]),InvResult,OutS):-
	write(Expr),write('\n'),
	resolveExpression(EntryS,Expr,Result,OutS),
	work('*',-1,Result,InvResult),write(InvResult),write('\n').

resolveExpression(EntryS,('signOperator',[type='+'],[Expr]),Result,OutS):-
	write('signOp'),write('\n'),
	resolveExpression(EntryS,Expr,Result,OutS),write(Result),write('\n').


resolveExpression(EntryS,('binaryOperator',Operator,[X,Y]),Result,OutS):-
	getContent(Operator,Op),
	%write(cosa),write('\n'),
	%write(X),write('\n'),
	%write(Y),write('\n'),
	resolveExpression(EntryS,X, Operand1,EntryS1),
	resolveExpression(EntryS1,Y, Operand2,OutS),
	write('operand '),write(Operand1),write('\n'),
	write('operand '),write(Operand2),write('\n'),
	work(Op, Operand1, Operand2,Result),
	write('result '),write(Result),write('\n').

resolveExpression(EntryS,('variable',[_=OperandName],_),OperandValue,EntryS):-
	state(EntryS,Table,_,_,_),
	write('---resolveExpression---'),write('\n'),
	getValue(Table,OperandName,OperandValue),
	write('\nTable \n'),write(Table),write('\n'),
	write('\nOperandName \n'),write(OperandName),write('\n'),
	write('\nOperandValue \n'),write(OperandValue),write('\n').

resolveExpression(EntryS,('constValue',Value,_),Value,EntryS). % DEPRECATED?

resolveExpression(EntryS,('const',[_=Value],_),Result,EntryS):-
	write('const: '),write(Value),write('\n'),
	atom_number(Value,Result),
	write('result const: '),write(Result),write('\n').

resolveExpression(EntryS,('consoleIn',[_=int],_),Value,OutS):-
	state(EntryS,Table,Cin,Cout,Trace),
	inf(X), sup(Y),
	Value in X..Y,
	append(Cin,[Value],Cin1),
	state(OutS,Table,Cin1,Cout,Trace).


resolveExpression(EntryS,('callFunction',[name=Name, type=Type],Args),ValueReturned,OutS):-
	write('en la caallllfunciton '),write('\n'),
	program(Program),
	lookForFunction(Program,Name,Type,[(_,_,Params),Function]),
	write('params '),write(Params),write('\n'),
	%write('funcion '),write(Function),write('\n'),
	write('argumentos '),write(Args),write('\n'),
	apila([],TCall),
	getTuple(Type,Tuple),
	add(TCall,Tuple,TCall1),
	buildCallTable(EntryS,EntryS1,TCall1,Params,Args,TCall2),
	state(EntryS1,Table,Cin,Cout,Trace),
	state(EntryCall,TCall2,Cin,Cout,Trace),!,
	execute(EntryCall,[Function],EntryS2),
	state(EntryS2,[[(_,_,ValueReturned)|_]],Cin2,Cout2,Trace2),
	write('el estado de vuelta '),write(ValueReturned),write('\n'),
	state(OutS,Table,Cin2,Cout2,Trace2).
	%ValueReturned #= 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Inicializamos una tabla con los valores de los argumentos pasados a la llamada

buildCallTable(S,S,T,[],[],T):-!,
write('tablitaaa'),write(T),write('\n').

buildCallTable(EntryS,OutS,Tin,[(_,[_=Type,_=Name],_)|Params],[(_,_,[Arg])|Args],Tout):-
	write('estamos construyendo la tabla '),write('\n'),
	resolveExpression(EntryS,Arg,Value,EntryS1),
	write('meteremos el valor en la tabla '),write('\n'),
	add(Tin,(Type,Name,Value),T1),
	write('metido el valor en la tabla '),write('\n'),
	buildCallTable(EntryS1,OutS,T1,Params,Args,Tout).


%resolveExpression((Entry,Cin,Cout,Trace),[('callFunction',[name=Name, type=Type],Params)],ValueReturned, (Out,Cin1,Cout1,Trace1)):-!,	% he añadido el Out
%	apila(Entry,Entry1),
%	addListParams(Entry1,Params,Out1),
%	program(Program),

%	lookForFunction(Program,Name,Type,Function),
	
%	createListParams(Function,Body,ListParams),
%	updateNames(Out1,ListParams,Out2),
%	execute((Out2,Cin,Cout,Trace),Body,(Out,Cin1,Cout1,Trace1)),
%	returnesValue(Out,ValueReturned).

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
%work('/', _,0,_):- !, fail.
work('/', Op1,Op2,Z):- !, Z #= Op1 / Op2.


