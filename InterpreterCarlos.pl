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
	interpreter(EntryFile, OutFile, -5, 15, 15, FunctionName). % Defaults

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

execute(EntryS,[],EntryS):-!.

%%%%%%FUNCTION%%%%%%

execute(EntryS,[('function',[_,_=void,_=Line],FunctionBody)|RestInstructios],OutS) :- !,
	write('Metodo'),write('\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		apila(Table,Table1),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table1,Cin,Cout,Trace1),
	execute(EntryS1,FunctionBody,OutS1),
	execute(OutS1,RestInstructios,OutS).

execute(EntryS,[('function',[_,_=ExitValue,_=Line],FunctionBody)|RestInstructios],OutS) :- !,
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
	execute(EntryS1,FunctionBody,OutS1),
	execute(OutS1,RestInstructios,OutS).

%%%%%%PARAMS%%%%%%

execute(EntryS,[('params',_,Params)|RestInstructios],OutS) :- !,
	write('estoy en Params: '), write(Params),write('\n'),
	execute(EntryS,Params,OutS1),
	execute(OutS1,RestInstructios,OutS).

execute(EntryS,[('param',[_=int,_=ParamName],ParamBody)|RestInstructios],OutS) :- !,
	inf(X), sup(Y),
	Value in X..Y,

	state(EntryS,Table,Cin,Cout,Trace),
		add(Table,(int,ParamName,Value),Table1),
	state(EntryS1,Table1,Cin,Cout,Trace),

	execute(EntryS1,ParamBody,OutS1),
	execute(OutS1,RestInstructios,OutS).

execute(EntryS,[('param',[_=ParamType,_=ParamName],ParamBody)|RestInstructios],OutS) :- !,
	state(EntryS,Table,Cin,Cout,Trace),
		add(Table,(ParamType,ParamName,_),Table1),
	state(EntryS1,Table1,Cin,Cout,Trace),
	execute(EntryS1,ParamBody,OutS1),
	execute(OutS1,RestInstructios,OutS).

%%%%%%BODY%%%%%%

execute(EntryS,[('body',_,Body)|RestInstructios],OutS) :- !,
	state(EntryS,Table,Cin,Cout,Trace),
		apila(Table, Table1),
	state(EntryS1,Table1,Cin,Cout,Trace),
	write('hacer un bodyA\n'),
	write(Body),
	write('hacer un bodyB\n'),
	execute(EntryS1,Body,EntryS2),
	write('el state tras el body '),write(EntryS2),write('\n'),
	write('hecho un body\n'),
	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		write('la tabla tras el body '),write(Table2),write('\n'),
		desapila(Table2, Table3),
		write('la tabla tras el desapila '),write(Table3),write('\n'),
	state(OutS1,Table3,Cin2,Cout2,Trace2),
	write('la tabla tras el body '),write(Table3),write('\n'),
	execute(OutS1,RestInstructios,OutS).

%%%%%%DECLARATIONS%%%%%%

execute(EntryS,[('declarations',_,Body)|RestInstructios],OutS) :- !,
write('entra en declarations\n'),
	execute(EntryS,Body,OutS1),
	write('sale de declarations\n'),
	write(RestInstructios),write('\n'),
	execute(OutS1,RestInstructios,OutS).

%step(EntryS,[('declaration',[_=int,_=Name,_=Line],[(const,[value=Value],_)])|RestInstructios],OutS):- 
%	write('declara1 '),write('\n'),
%	atom_number(Value,Value1),
%	state(EntryS,Table,Cin,Cout,Trace),
%		%write(Value1),write('\n'),
%		add(Table,(int,Name,Value1),Table1),
%		%write(hechoAdd),write('\n'),
%		append(Trace,[' '],Space),
%		append(Space,[Line],Trace1),
%	state(OutS,Table1,Cin,Cout,Trace1).

execute(EntryS,[('declaration',[_=int,_=Name,_=Line],[])|RestInstructios],OutS):- !,
	inf(X), sup(Y),
	Value in X..Y,
	write('declara2 '),write(Line),write('\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS2,Table,Cin,Cout,Trace1),
	write('declara2 '),write(Line),write('\n'),
	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		add(Table2,(int,Name,Value),Table3),
	state(OutS1,Table3,Cin2,Cout2,Trace2),
	write('la tabla tras la decl '),write(Table3),write('\n'),
	execute(OutS1,RestInstructios,OutS).

execute(EntryS,[('declaration',[_=Type,_=Name,_=Line],[DecBody])|RestInstructios],OutS):- !,
	write('declara3'),write(Line),write('\n'),
	state(EntryS,Table,Cin,Cout,Trace),
	write('mi tabla recien llegado '),write(Table),write('\n'),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),
	
	resolveExpression(EntryS1,DecBody,Value,EntryS2),
	write('declara3'),write(Line),write('\n'),
	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		add(Table2,(int,Name,Value),Table3),
	state(OutS1,Table3,Cin2,Cout2,Trace2),
	write(RestInstructios),write('\n'),
	write('la tabla tras la decl '),write(Table3),write('\n'),
	execute(OutS1,RestInstructios,OutS).

%%%%%%ASSIGMENT%%%%%%

execute(EntryS,[('assignment',[_=Name,_=Line],[AssigBody])|RestInstructios],OutS) :- !,
	state(EntryS,Table,Cin,Cout,Trace),
	write('mi tabla recien llegado assignment '),write(Table),write('\n'),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),

	write('\n---\n'),
	write(AssigBody),
	resolveExpression(EntryS1,AssigBody,Value,EntryS2),
	write('\n???\n'),
	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		update(Table2,(Name,Value),Table3),
	state(OutS1,Table3,Cin2,Cout2,Trace2),
	write('mi tabla recien saliendo assignment'),write(Table3),write('\n'),
	execute(OutS1,RestInstructios,OutS).

%%%%%%ASSIGMENT-OPERATOR%%%%%%

execute(EntryS,[('assignmentOperator',[_=Name,_,_=Operator,_=Line],[AssigBody])|RestInstructios],OutS) :- !,
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),
	write('assigmentOperatorrA'),write(Line),write('\n'),

	resolveExpression(EntryS1,
		('binaryOperator',[Operator],
			[('variable',[_=Name],[]),AssigBody])
		,Value,EntryS2),
	write('assigmentOperatorrB'),write(Line),write('\n'),
	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		update(Table2,(Name,Value),Table3),
	state(OutS1,Table3,Cin2,Cout2,Trace2),
	execute(OutS1,RestInstructios,OutS).

%%%%%%UNARY-OPERATOR%%%%%%

execute(EntryS,[('unaryOperator',[_=Name,_=Operator,_=Line],[])|RestInstructios],OutS):-!,
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
	state(OutS1,Table3,Cin2,Cout2,Trace2),
	execute(OutS1,RestInstructios,OutS).

%%%%%%CONSOLE-OUT%%%%%%

execute(EntryS,[('consoleOut',[_=Line],[Expr])|RestInstructios],OutS):-!,
	write('hagoOut'),write(Line),write('\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),

	resolveExpression(EntryS1,Expr,Value,EntryS2),
	write('calculadoOut'),write(Line),write('\n'),
	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		append(Cout2,[Value],Cout3),
	state(OutS1,Table2,Cin2,Cout3,Trace2),
	execute(OutS1,RestInstructios,OutS).

%%%%%%RETURN%%%%%%

execute(EntryS,[('return',[_=Line],[Body])|_],OutS):-!,
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),
	write('\nTablaaaaaaaa:\n'), write(Table), write('\n'),
	resolveExpression(EntryS1,Body,Value,EntryS2),
	write('result: '),write(Value),write('<-\n'),
	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		update(Table2,(ret,Value),Table3),

	write('\nUpdateeee:\n'), write(Table3), write('\n'),

	state(OutS,Table3,Cin2,Cout2,Trace2),
	write('\nOutsSSSS:\n'), write(OutS), write('\n').

%%%%%%IF%%%%%%

execute(EntryS,[('if',[_=Line],[Condition,('then',_,Then)])|RestInstructios],OutS):-!,
	execute(EntryS,[('if',[_=Line],[Condition,('then',_,Then),('else',_,[])])|RestInstructios],OutS).

%%%%%%IF-THEN%%%%%%

execute(EntryS,[('if',[_=Line],[Condition,('then',_,Then),_])|RestInstructios],OutS):-
	state(EntryS,Table,Cin,Cout,Trace),
		write('la tabla entrando al ifTHEN 1'),write(Table),write('\n\n'),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),

	resolveExpression(EntryS1,Condition,R,EntryS2),
	R#=1,
	write('\n\nTRUE IF\n\n'),
	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		apila(Table2,Table3),
	state(EntryS3,Table3,Cin2,Cout2,Trace2),
	%write('hacer el then\n'),
	%write(Then),
	%write('hacer el then\n'),
	execute(EntryS3,Then,EntryS4),
	%write('hecho el then\n'),
	state(EntryS4,Table4,Cin4,Cout4,Trace4),
		desapila(Table4, Table5),
	state(OutS1,Table5,Cin4,Cout4,Trace4),
	write(Table5),
	execute(OutS1,RestInstructios,OutS),
	write('hecho el ifTRUE').

%%%%%%IF-ELSE%%%%%%

execute(EntryS,[('if',[_=Line],[_,_,('else',_,Else)])|RestInstructios],OutS):- 
	write('\n\nFALSE IF\n\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		write('la tabla entrando al ifELSE 2'),write(Table),write('\n\n'),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),

	resolveExpression(EntryS1,Condition,R,EntryS2),
	R#=0,
	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		apila(Table2,Table3),
	state(EntryS3,Table3,Cin2,Cout2,Trace2),

	execute(EntryS3,Else,EntryS4),
	%write('he llegao asta aqui'),
	state(EntryS4,Table4,Cin4,Cout4,Trace4),
		desapila(Table4, Table5),
	state(OutS1,Table5,Cin4,Cout4,Trace4),

	execute(OutS1,RestInstructios,OutS).

%%%%%%WHILE%%%%%%

execute(EntryS,[('while',Data,[C,B])|RestInstructios],OutS) :-!,
	maxDepth(N),
	write('le damos profundidad\n'),
	state(EntryS,T,_,_,_),
	write(T),write('\n'),
	executeL(EntryS,[('while',Data,[C,B])|RestInstructios],N,OutS).

%%%%%%FOR%%%%%%

execute(EntryS,[('for',Data,[Variable,C,A,('body',_,B)])|RestInstructios],OutS) :-!,
	maxDepth(N),
	write('le damos profundidad al for\n'),
	variableAdvance(EntryS,Variable,VariableName,EntryS1),
	executeL(EntryS1,[('for',Data,[Variable,C,A,('body',_,B)])|RestInstructios],N,OutS).

%%%%%%LOOPS%%%%%%



executeL(EntryS,[('while',[_=Line],[C,('body',_,B)])|RestInstructios],N,OutS):-
	write('Casa en sigue\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),
	resolveExpression(EntryS1,Condition,R,EntryS2),
	R#=1,
	write(B),write('\n'),
	execute(EntryS2,B,EntryS3),
	N1 is N-1,
	write('Sigue en bucle\n'),
	executeL(EntryS3,[('while',[_=Line],[C,('body',_,B)])|RestInstructios],N1,OutS).


executeL(EntryS,[('while',Data,[Condition,_])|RestInstructios],_,OutS) :-
	write('Casa en sale\n'),
	write('condicion:  ->'),write(Condition),write('<-- condicion\n'),
	state(EntryS,T,_,_,_),
	write('tabla:  ->'),write(T),write('<-- tabla\n'),
	resolveExpression(EntryS,Condition,R,EntryS1),
	write('condicionR:  ->'),write(R),write('<-- condicionR\n'),
	R #= 0,
	write('Sale por condicion\n'),
	execute(EntryS1,RestInstructios,OutS).

executeL(EntryS1,[('for',[_=Line],[Variable,Condition,Advance,('body',_,ForBody)])|RestInstructios],N,OutS):-!,
	write('hagamos cosas del for A\n'),
	%variableAdvance(EntryS,Variable,VariableName,EntryS1),
	%getContent(Variable,VariableName),
	%execute(EntryS,Variable,EntryS1),
	state(EntryS1,Table1,Cin1,Cout1,Trace1),
		write('la tabla entrando al for 1'),write(Table1),write('\n\n'),
		append(Trace1,[' '],Space),
		append(Space,[Line],Trace2),
	state(EntryS2,Table1,Cin1,Cout1,Trace2),
	write('hagamos cosas del for B\n'),
	resolveExpression(EntryS2,Condition,R,EntryS3),
	write(R),write('<<---es el valor de R\n'),
	R#=1,
	write('hagamos cosas del for C\n'),
	execute(EntryS3,ForBody,EntryS4),
	write('hagamos cosas del for D\n'),
	execute(EntryS4,[Advance],EntryS5),
	write('hagamos cosas del for E\n'),
	read(Ww),
	N1 is N - 1,
	executeL(EntryS5,[('for',[_=Line],[Variable,Condition,Advance,('body',_,ForBody)])|RestInstructios],N1,OutS).



executeL(EntryS1,[('for',[_=Line],[Variable,Condition,Advance,('body',_,ForBody)])|RestInstructios],_,OutS) :-
	write('saldremos del for\n'),
	%variableAdvance(EntryS,Variable,VariableName,EntryS1),
	%getContent(Variable,VariableName),
	%execute(EntryS,Variable,EntryS1),
	state(EntryS1,Table1,Cin1,Cout1,Trace1),
		write('la tabla entrando al for 2'),write(Table1),write('\n\n'),
		append(Trace1,[' '],Space),
		append(Space,[Line],Trace2),
	state(EntryS2,Table1,Cin1,Cout1,Trace2),
	write('estamos saliendo del for\n'),
	resolveExpression(EntryS2,Condition,R,EntryS3),
	write(R),write('<<---es el valor de R\n'),
	R#=0,
	read(U),
	write('resto de instrucciones'),
	write(RestInstructios),write('\n'),
	execute(EntryS3,RestInstructios,OutS).


executeL(EntryS,[_|RestInstructios],0,OutS):-!,
	write('Sale por limite\n'),
	execute(EntryS,RestInstructios,OutS).


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


resolveExpression(EntryS,('binaryOperator',Operator,[X,Y]),Result,OutS):-!,
	getContent(Operator,Op),
	%write(cosa),write('\n'),
	%write(X),write('\n'),
	%write(Y),write('\n'),
	resolveExpression(EntryS,X, Operand1,EntryS1),
	resolveExpression(EntryS1,Y, Operand2,OutS),
	write('operand '),write(Operand1),write('\n'),
	write('operand '),write(Operand2),write('\n'),
	work(Op, Operand1, Operand2,Result),!,
	write('result '),write(Result),write('\n').

resolveExpression(EntryS,('variable',[_=OperandName],_),OperandValue,EntryS):-
	state(EntryS,Table,_,_,_),
	%write('---resolveExpression---'),write('\n'),
	getValue(Table,OperandName,OperandValue).
	%write('\nTable \n'),write(Table),write('\n'),
	%write('\nOperandName \n'),write(OperandName),write('\n'),
	%write('\nOperandValue \n'),write(OperandValue),write('\n').

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


