
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%%%%%%%%%%%
					% execute %
					%%%%%%%%%%%

% Recorre la lista de instrucciones una a una para ir ejecutándolas

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

execute(1,S,_,S,1):-!.

execute(R,EntryS,[],EntryS,R):-!.

%%%%%%FUNCTION%%%%%%

execute(0,EntryS,[('function',[_,_=void,_=Line],FunctionBody)|_],OutS,R) :- 
	write('Metodo'),write('\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		apila(Table,Table1),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table1,Cin,Cout,Trace1),
	execute(0,EntryS1,FunctionBody,OutS,R).

execute(0,EntryS,[('function',[_,_=ExitValue,_=Line],FunctionBody)|_],OutS,R) :- 
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
	execute(0,EntryS1,FunctionBody,OutS,R),
	write('estamos saliendo de la funcion\n').

%%%%%%PARAMS%%%%%%

execute(0,EntryS,[('params',_,Params)|RestInstructions],OutS,R) :- !,
	write('estoy en Params: '), write(Params),write('\n'),
	execute(0,EntryS,Params,OutS1,R1),
	execute(R1,OutS1,RestInstructions,OutS,R).

execute(0,EntryS,[('param',[_=int,_=ParamName],ParamBody)|RestInstructions],OutS,R) :- !,
	inf(X), sup(Y),
	Value in X..Y,

	state(EntryS,Table,Cin,Cout,Trace),
		add(Table,(int,ParamName,Value),Table1),
	state(EntryS1,Table1,Cin,Cout,Trace),

	execute(0,EntryS1,ParamBody,OutS1,R1),
	execute(R1,OutS1,RestInstructions,OutS,R).

execute(0,EntryS,[('param',[_='_Bool',_=ParamName],ParamBody)|RestInstructions],OutS,R) :- !,
	Value in 0..1,

	state(EntryS,Table,Cin,Cout,Trace),
		add(Table,(int,ParamName,Value),Table1),
	state(EntryS1,Table1,Cin,Cout,Trace),

	execute(0,EntryS1,ParamBody,OutS1,R1),
	execute(R1,OutS1,RestInstructions,OutS,R).

execute(0,EntryS,[('param',[_=ParamType,_=ParamName],ParamBody)|RestInstructions],OutS,R) :- !,
	state(EntryS,Table,Cin,Cout,Trace),
		add(Table,(ParamType,ParamName,_),Table1),
	state(EntryS1,Table1,Cin,Cout,Trace),
	execute(0,EntryS1,ParamBody,OutS1,R1),
	execute(R1,OutS1,RestInstructions,OutS,R).

%%%%%%BODY%%%%%%

execute(0,EntryS,[('body',_,Body)|RestInstructions],OutS,R) :- !,
	state(EntryS,Table,Cin,Cout,Trace),
		apila(Table, Table1),
	state(EntryS1,Table1,Cin,Cout,Trace),
	write('hacer un bodyA\n'),
	write(Body),
	write('hacer un bodyB\n'),
	execute(0,EntryS1,Body,EntryS2,R1),
	write('el state tras el body '),write(EntryS2),write('\n'),
	write('hecho un body\n'),
	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		write('la tabla tras el body '),write(Table2),write('\n'),
		desapila(Table2, Table3),
		write('la tabla tras el desapila '),write(Table3),write('\n'),
	state(OutS1,Table3,Cin2,Cout2,Trace2),
	write('la tabla tras el body '),write(Table3),write('\n'),
	execute(R1,OutS1,RestInstructions,OutS,R).

%%%%%%DECLARATIONS%%%%%%

execute(0,EntryS,[('declarations',_,Body)|RestInstructions],OutS,R) :- !,
write('entra en declarations\n'),
	execute(0,EntryS,Body,OutS1,R1),
	write('sale de declarations\n'),
	write(RestInstructions),write('\n'),
	execute(R1,OutS1,RestInstructions,OutS,R).

execute(0,EntryS,[('declaration',[_=int,_=Name,_=Line],[])|RestInstructions],OutS,R):- !,
	inf(X), sup(Y),
	Value in X..Y,
	write('declara2 '),write(Line),write('\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
		add(Table,(int,Name,Value),Table1),
	state(OutS1,Table1,Cin,Cout,Trace1),
	write('la tabla tras la decl '),write(Table1),write('\n'),
	execute(0,OutS1,RestInstructions,OutS,R).

execute(0,EntryS,[('declaration',[_,_=Name,_=Line],[DecBody])|RestInstructions],OutS,R):- !,
	write('declara3'),write(Line),write('\n'),
	state(EntryS,Table,Cin,Cout,Trace),
	write('mi tabla recien llegado '),write(Table),write('\n'),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),
	
	resolveExpression(0,EntryS1,DecBody,Value,EntryS2,R1),
	write('declara3'),write(Line),write('\n'),
	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		add(Table2,(int,Name,Value),Table3),
	state(OutS1,Table3,Cin2,Cout2,Trace2),
	write(RestInstructions),write('\n'),
	write('la tabla tras la decl '),write(Table3),write('\n'),
	execute(R1,OutS1,RestInstructions,OutS,R).

%%%%%%ASSIGMENT%%%%%%

execute(0,EntryS,[('assignment',[_=Name,_=Line],[AssigBody])|RestInstructions],OutS,R) :- !,
	state(EntryS,Table,Cin,Cout,Trace),
	write('mi tabla recien llegado assignment '),write(Table),write('\n'),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),

	write('\n---\n'),
	write(AssigBody),
	resolveExpression(0,EntryS1,AssigBody,Value,EntryS2,R1),
	write('\n???\n'),
	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		update(Table2,(Name,Value),Table3),
	state(OutS1,Table3,Cin2,Cout2,Trace2),
	write('mi tabla recien saliendo assignment'),write(Table3),write('\n'),
	execute(R1,OutS1,RestInstructions,OutS,R).

%%%%%%ASSIGMENT-OPERATOR%%%%%%

execute(0,EntryS,[('assignmentOperator',[_=Name,_,_=Operator,_=Line],[AssigBody])|RestInstructions],OutS,R) :- !,
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),
	write('assigmentOperatorrA'),write(Line),write('\n'),

	resolveExpression(0,EntryS1,
		('binaryOperator',[Operator],
			[('variable',[_=Name],[]),AssigBody])
		,Value,EntryS2,R1),

	write('assigmentOperatorrB'),write(Line),write('\n'),
	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		update(Table2,(Name,Value),Table3),
	state(OutS1,Table3,Cin2,Cout2,Trace2),
	execute(R1,OutS1,RestInstructions,OutS,R).

%%%%%%UNARY-OPERATOR%%%%%%

execute(0,EntryS,[('unaryOperator',[_=Name,_=Operator,_=Line],[])|RestInstructions],OutS,R):-!,
	write('unary'),write(Line),write('\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),
	write('antes del menosmenos '),write(Operator),write('\n'),
	resolveExpression(0,EntryS1,
		('binaryOperator',[Operator],
			[('variable',[_=Name],[]),
				('const',[_='1'],_)])
		,Value,EntryS2,R1),
	write('despues del menos menos '),write(Line),write('\n'),
	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		update(Table2,(Name,Value),Table3),
	state(OutS1,Table3,Cin2,Cout2,Trace2),
	execute(R1,OutS1,RestInstructions,OutS,R).

%%%%%%CONSOLE-OUT%%%%%%

execute(0,EntryS,[('consoleOut',[_=Line],[Expr])|RestInstructions],OutS,R):-!,
	write('hagoOut'),write(Line),write('\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),
	write('teniendo la tabla : '),write(Table),write('\n'),
	resolveExpression(0,EntryS1,Expr,Value,EntryS2,R1),
	write('calculadoOut'),write(Line),write('\n'),
	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		append(Cout2,[' '],Space2),
		append(Space2,[Value],Cout3),
	state(OutS1,Table2,Cin2,Cout3,Trace2),
	execute(R1,OutS1,RestInstructions,OutS,R).

%%%%%%RETURN%%%%%%

execute(0,EntryS,[('return',[_=Line],[Body])|_],OutS,1):-!,
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),
	write('\nTablaaaaaaaa:\n'), write(Table), write('\n'),
	resolveExpression(0,EntryS1,Body,Value,EntryS2,_),
	write('result: '),write(Value),write('<-\n'),
	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		update(Table2,(ret,Value),Table3),

	write('\nUpdateeee:\n'), write(Table3), write('\n'),

	state(OutS,Table3,Cin2,Cout2,Trace2).
%%%%%%IF%%%%%%

execute(0,EntryS,[('if',[_=Line],[Condition,('then',_,Then)])|RestInstructions],OutS,R):-!,
	write('lno tiene else 1'),%write(Table),write('\n\n'),
	execute(0,EntryS,[('if',[_=Line],[Condition,('then',_,Then),('else',_,[])])|RestInstructions],OutS,R).

%%%%%%IF-THEN%%%%%%

execute(0,EntryS,[('if',[_=Line],[Condition,('then',_,Then),('else',_,Body)])|RestInstructions],OutS,R):-!,
	state(EntryS,Table,Cin,Cout,Trace),
		write('la tabla entrando al IF 1'),write(Table),write('\n\n'),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),

	resolveExpression(0,EntryS1,Condition,Value,EntryS2,R1),
	executeBranch(R1,EntryS2,[('if',[_=Line],[_,('then',_,Then),('else',_,Body)])|RestInstructions],Value,OutS,R).

%%%%%%WHILE%%%%%%

execute(0,EntryS,[('while',Data,[Condition,B])|RestInstructions],OutS,R) :-!,
	maxDepth(N),
	write('vamos a evaluar el while\n'),
	resolveExpression(0,EntryS,Condition,Value,EntryS1,R1),
	write('evaluando el valor '),write(Value),write('\n'),
	%read(A),
	executeLoop(R1,EntryS1,[('while',Data,[Condition,B])|RestInstructions],N,Value,OutS,R).

%%%%%%FOR%%%%%%

execute(0,EntryS,[('for',Data,[Variable,Condition,A,('body',_,B)])|RestInstructions],OutS,R):-!,
	maxDepth(N),
	write('vamos a inicializar el for\n'),
	variableAdvance(0,EntryS,Variable,EntryS1,R1),
	resolveExpression(R1,EntryS1,Condition,Value,EntryS2,R2),
	state(EntryS2,T,_,_,_),
	write('inicializada la variable de control '),write(T),write('\n'),
	%read(U),
	executeLoop(R2,EntryS2,[('for',Data,[Variable,Condition,A,('body',_,B)])|RestInstructions],N,Value,OutS,R).

%%%%%%EXECUTE-BRANCH%%%%%%
 	
executeBranch(0,EntryS,[('if',[_],[_,('then',_,Then),_])|RestInstructions],1,OutS,R):-
	write('\n\nTRUE IF\n\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		apila(Table,Table1),
	state(EntryS1,Table1,Cin,Cout,Trace),
	execute(0,EntryS1,Then,EntryS2,R1),
	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		desapila(Table2, Table3),
	state(OutS1,Table3,Cin2,Cout2,Trace2),
	write('%%%%%%%%$"·$"·$!·!"$%·$·he llegao asta THEEEEENNNNN\n'),
	write(RestInstructions),
	execute(R1,OutS1,RestInstructions,OutS,R),
	write('hecho el ifTRUE').

executeBranch(0,EntryS,[('if',[_],[_,_,('else',_,Else)])|RestInstructions],0,OutS,R):- 
	state(EntryS,Table,Cin,Cout,Trace),
		apila(Table,Table1),
	state(EntryS1,Table1,Cin,Cout,Trace),

	execute(0,EntryS1,Else,EntryS2,R1),
	write('%%%%%%%%$"·$"·$!·!"$%·$·he llegao asta aqui'),
	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		desapila(Table2, Table3),
	state(OutS1,Table3,Cin2,Cout2,Trace2),
	write(Table3),
	execute(R1,OutS1,RestInstructions,OutS,R),
	write('hecho el ifFALSE\n').

%%%%%%EXECUTE-LOOP-WHILE%%%%%%

executeLoop(0,EntryS1,[('while',_,[Condition,('body',_,_)])|RestInstructions],0,_,OutS,R):-!,
	write('Deberia salirse del while debido al limite: ejecuta el cuerpo una ultima vez'),write('\n'),
	%read(A),
	%execute(EntryS,B,EntryS1),
	resolveExpression(0,EntryS1,Condition,0,EntryS2,R1),
	execute(R1,EntryS2,RestInstructions,OutS,R).

executeLoop(0,EntryS,[('while',[_=Line],_)|RestInstructions],N,0,OutS,R):-N>=0,
	write('llegando con valor N: '),write(N),write('\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),
	write('ha casado con el salirse while\n'),
	%read(A),
	write(Table),write('\n'),
	execute(0,EntryS1,RestInstructions,OutS,R).
	
executeLoop(0,EntryS,[('while',[_=Line],[Condition,('body',_,B)])|RestInstructions],N,1,OutS,R):-N>=0,
	write('llegando con valor N: '),write(N),write('\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),
	write('ha casado con el seguir\n'),
	execute(0,EntryS1,B,EntryS2,R1),
	write('ha hecho el body\n'),
	resolveExpression(R1,EntryS2,Condition,Value,EntryS3,R2),
	N1 is N-1,
	write('evaluando el valor '),write(Value),write('\n'),
	%read(A),
	executeLoop(R2,EntryS3,[('while',[_=Line],[Condition,('body',_,B)])|RestInstructions],N1,Value,OutS,R).

%%%%%%EXECUTE-LOOP-FOR%%%%%%

executeLoop(0,EntryS,[('for',_,[_,Condition,_,('body',_,_)])|RestInstructions],0,_,OutS,R):-!,
	write('Deberia salirse del for debido al limite: NO ejecuta la ultima vuelta'),write('\n'),
	%read(A),
	%execute(R,EntryS,B,EntryS1),
	%execute(R,EntryS1,[Advance],EntryS2),
	resolveExpression(0,EntryS,Condition,0,EntryS1,R1),
	execute(R1,EntryS1,RestInstructions,OutS,R).

executeLoop(0,EntryS,[('for',[_=Line],[_,Condition,Advance,('body',_,B)])|RestInstructions],N,1,OutS,R):-N >= 0,!,
	write('ha entrado en una iteracion del for\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' ',Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),
	write('queriendo ejecutar el body\n'),write(B),write('\n'),
	execute(0,EntryS1,B,EntryS2,R1),
	write('habiendo ejecutado el body\n'),
	%read(Ujh),
	execute(R1,EntryS2,[Advance],EntryS3,R2),
	state(EntryS3,T3,_,_,_),
	write('tras hacer el avance con la tabla\n'),write(T3),write('\n'),
	%read(Ujhgg),
	resolveExpression(R2,EntryS3,Condition,Value,EntryS4,R3),
	N1 is N -1,
	write('evaluando el valor '),write(Value),write('\n'),
	%read(U),
	executeLoop(R3,EntryS4,[('for',[_=Line],[_,Condition,Advance,('body',_,B)])|RestInstructions],N1,Value,OutS,R).

executeLoop(0,EntryS,[('for',[_=Line],_)|RestInstructions],N,0,OutS,R):-N >= 0,!,
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),
	write('ha casado con el salirse for\n'),
	%read(A),
	write(Table),write('\n'),
	execute(0,EntryS1,RestInstructions,OutS,R).
