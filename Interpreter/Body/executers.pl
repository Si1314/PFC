
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%%%%%%%%%%%
					% execute %
					%%%%%%%%%%%

% Recorre la lista de instrucciones una a una para ir ejecutándolas

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

execute(EntryS,[],EntryS):-!.

%%%%%%FUNCTION%%%%%%

execute(EntryS,[('function',[_,_=void,_=Line],FunctionBody)|RestInstructions],OutS) :- 
	write('Metodo'),write('\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		apila(Table,Table1),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table1,Cin,Cout,Trace1),
	execute(EntryS1,FunctionBody,OutS1),
	execute(OutS1,RestInstructions,OutS).

execute(EntryS,[('function',[_,_=ExitValue,_=Line],FunctionBody)|RestInstructions],OutS) :- 
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
	execute(EntryS1,FunctionBody,OutS),
	write('estamos saliendo de la funcion\n').

%%%%%%PARAMS%%%%%%

execute(EntryS,[('params',_,Params)|RestInstructions],OutS) :- !,
	write('estoy en Params: '), write(Params),write('\n'),
	execute(EntryS,Params,OutS1),
	execute(OutS1,RestInstructions,OutS).

execute(EntryS,[('param',[_=int,_=ParamName],ParamBody)|RestInstructions],OutS) :- !,
	inf(X), sup(Y),
	Value in X..Y,

	state(EntryS,Table,Cin,Cout,Trace),
		add(Table,(int,ParamName,Value),Table1),
	state(EntryS1,Table1,Cin,Cout,Trace),

	execute(EntryS1,ParamBody,OutS1),
	execute(OutS1,RestInstructions,OutS).

execute(EntryS,[('param',[_=ParamType,_=ParamName],ParamBody)|RestInstructions],OutS) :- !,
	state(EntryS,Table,Cin,Cout,Trace),
		add(Table,(ParamType,ParamName,_),Table1),
	state(EntryS1,Table1,Cin,Cout,Trace),
	execute(EntryS1,ParamBody,OutS1),
	execute(OutS1,RestInstructions,OutS).

%%%%%%BODY%%%%%%

execute(EntryS,[('body',_,Body)|RestInstructions],OutS) :- !,
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
	execute(OutS1,RestInstructions,OutS).

%%%%%%DECLARATIONS%%%%%%

execute(EntryS,[('declarations',_,Body)|RestInstructions],OutS) :- !,
write('entra en declarations\n'),
	execute(EntryS,Body,OutS1),
	write('sale de declarations\n'),
	write(RestInstructions),write('\n'),
	execute(OutS1,RestInstructions,OutS).

execute(EntryS,[('declaration',[_=int,_=Name,_=Line],[])|RestInstructions],OutS):- !,
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
	execute(OutS1,RestInstructions,OutS).

execute(EntryS,[('declaration',[_,_=Name,_=Line],[DecBody])|RestInstructions],OutS):- !,
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
	write(RestInstructions),write('\n'),
	write('la tabla tras la decl '),write(Table3),write('\n'),
	execute(OutS1,RestInstructions,OutS).

%%%%%%ASSIGMENT%%%%%%

execute(EntryS,[('assignment',[_=Name,_=Line],[AssigBody])|RestInstructions],OutS) :- !,
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
	execute(OutS1,RestInstructions,OutS).

%%%%%%ASSIGMENT-OPERATOR%%%%%%

execute(EntryS,[('assignmentOperator',[_=Name,_,_=Operator,_=Line],[AssigBody])|RestInstructions],OutS) :- !,
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
	execute(OutS1,RestInstructions,OutS).

%%%%%%UNARY-OPERATOR%%%%%%

execute(EntryS,[('unaryOperator',[_=Name,_=Operator,_=Line],[])|RestInstructions],OutS):-!,
	write('unary'),write(Line),write('\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),
	write('antes del menosmenos '),write(Operator),write('\n'),
	resolveExpression(EntryS1,
		('binaryOperator',[Operator],
			[('variable',[_=Name],[]),
				('const',[_='1'],_)])
		,Value,EntryS2),
	write('despues del menos menos '),write(Line),write('\n'),
	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		update(Table2,(Name,Value),Table3),
	state(OutS1,Table3,Cin2,Cout2,Trace2),
	execute(OutS1,RestInstructions,OutS).

%%%%%%CONSOLE-OUT%%%%%%

execute(EntryS,[('consoleOut',[_=Line],[Expr])|RestInstructions],OutS):-!,
	write('hagoOut'),write(Line),write('\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),
	write('teniendo la tabla : '),write(Table),write('\n'),
	resolveExpression(EntryS1,Expr,Value,EntryS2),
	write('calculadoOut'),write(Line),write('\n'),
	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		append(Cout2,[' '],Space2),
		append(Space2,[Value],Cout3),
	state(OutS1,Table2,Cin2,Cout3,Trace2),
	execute(OutS1,RestInstructions,OutS).

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

	state(OutS,Table3,Cin2,Cout2,Trace2).
%%%%%%IF%%%%%%

execute(EntryS,[('if',[_=Line],[Condition,('then',_,Then)])|RestInstructions],OutS):-!,
	write('lno tiene else 1'),write(Table),write('\n\n'),
	execute(EntryS,[('if',[_=Line],[Condition,('then',_,Then),('else',_,[])])|RestInstructions],OutS).

%%%%%%IF-THEN%%%%%%

execute(EntryS,[('if',[_=Line],[Condition,('then',_,Then),('else',_,Body)])|RestInstructions],OutS):-!,
	state(EntryS,Table,Cin,Cout,Trace),
		write('la tabla entrando al IF 1'),write(Table),write('\n\n'),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),

	resolveExpression(EntryS1,Condition,Value,EntryS2),
	executeBranch(EntryS2,[('if',[_=Line],[_,('then',_,Then),('else',_,Body)])|RestInstructions],Value,OutS).

%%%%%%WHILE%%%%%%

execute(EntryS,[('while',Data,[Condition,B])|RestInstructions],OutS) :-!,
	maxDepth(N),
	write('vamos a evaluar el while\n'),
	resolveExpression(EntryS,Condition,Value,EntryS1),
	write('evaluando el valor '),write(Value),write('\n'),
	%read(A),
	executeLoop(EntryS1,[('while',Data,[Condition,B])|RestInstructions],N,Value,OutS).

%%%%%%FOR%%%%%%

execute(EntryS,[('for',Data,[Variable,Condition,A,('body',_,B)])|RestInstructions],OutS):-!,
	maxDepth(N),
	write('vamos a inicializar el for\n'),
	variableAdvance(EntryS,Variable,EntryS1),
	resolveExpression(EntryS1,Condition,Value,EntryS2),
	state(EntryS2,T,_,_,_),
	write('inicializada la variable de control '),write(T),write('\n'),
	%read(U),
	executeLoop(EntryS2,[('for',Data,[Variable,Condition,A,('body',_,B)])|RestInstructions],N,Value,OutS).

%%%%%%EXECUTE-BRANCH%%%%%%
 	
executeBranch(EntryS,[('if',[_],[_,('then',_,Then),_])|RestInstructions],1,OutS):-
	write('\n\nTRUE IF\n\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		apila(Table,Table1),
	state(EntryS1,Table1,Cin,Cout,Trace),
	execute(EntryS1,Then,EntryS2),
	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		desapila(Table2, Table3),
	state(OutS1,Table3,Cin2,Cout2,Trace2),
	write('%%%%%%%%$"·$"·$!·!"$%·$·he llegao asta THEEEEENNNNN\n'),
	write(RestInstructions),
	execute(OutS1,RestInstructions,OutS),
	write('hecho el ifTRUE').

executeBranch(EntryS,[('if',[_],[_,_,('else',_,Else)])|RestInstructions],0,OutS):- 
	state(EntryS,Table,Cin,Cout,Trace),
		apila(Table,Table1),
	state(EntryS1,Table1,Cin,Cout,Trace),

	execute(EntryS1,Else,EntryS2),
	write('%%%%%%%%$"·$"·$!·!"$%·$·he llegao asta aqui'),
	state(EntryS2,Table2,Cin2,Cout2,Trace2),
		desapila(Table2, Table3),
	state(OutS1,Table3,Cin2,Cout2,Trace2),
	write(Table3),
	execute(OutS1,RestInstructions,OutS),
	write('hecho el ifFALSE\n').

%%%%%%EXECUTE-LOOP-WHILE%%%%%%

executeLoop(EntryS1,[('while',_,[Condition,('body',_,B)])|RestInstructions],0,_,OutS):-!,
	write('Deberia salirse del while debido al limite: ejecuta el cuerpo una ultima vez'),write('\n'),
	%read(A),
	%execute(EntryS,B,EntryS1),
	resolveExpression(EntryS1,Condition,0,EntryS2),
	execute(EntryS2,RestInstructions,OutS).

executeLoop(EntryS,[('while',[_=Line],_)|RestInstructions],N,0,OutS):-N>=0,
	write('llegando con valor N: '),write(N),write('\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),
	write('ha casado con el salirse while\n'),
	%read(A),
	write(Table),write('\n'),
	execute(EntryS1,RestInstructions,OutS).
	
executeLoop(EntryS,[('while',[_=Line],[Condition,('body',_,B)])|RestInstructions],N,1,OutS):-N>=0,
	write('llegando con valor N: '),write(N),write('\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),
	write('ha casado con el seguir\n'),
	execute(EntryS1,B,EntryS2),
	write('ha hecho el body\n'),
	resolveExpression(EntryS2,Condition,Value,EntryS3),
	N1 is N-1,
	write('evaluando el valor '),write(Value),write('\n'),
	%read(A),
	executeLoop(EntryS3,[('while',[_=Line],[Condition,('body',_,B)])|RestInstructions],N1,Value,OutS).

%%%%%%EXECUTE-LOOP-FOR%%%%%%

executeLoop(EntryS,[('for',_,[_,Condition,Advance,('body',_,B)])|RestInstructions],0,_,OutS):-!,
	write('Deberia salirse del for debido al limite: NO ejecuta la ultima vuelta'),write('\n'),
	%read(A),
	%execute(EntryS,B,EntryS1),
	%execute(EntryS1,[Advance],EntryS2),
	resolveExpression(EntryS,Condition,0,EntryS1),
	execute(EntryS1,RestInstructions,OutS).

executeLoop(EntryS,[('for',[_=Line],[_,Condition,Advance,('body',_,B)])|RestInstructions],N,1,OutS):-N >= 0,!,
	write('ha entrado en una iteracion del for\n'),
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' ',Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),
	write('queriendo ejecutar el body\n'),write(B),write('\n'),
	execute(EntryS1,B,EntryS2),
	write('habiendo ejecutado el body\n'),
	%read(Ujh),
	execute(EntryS2,[Advance],EntryS3),
	state(EntryS3,T3,_,_,_),
	write('tras hacer el avance con la tabla\n'),write(T3),write('\n'),
	%read(Ujhgg),
	resolveExpression(EntryS3,Condition,Value,EntryS4),
	N1 is N -1,
	write('evaluando el valor '),write(Value),write('\n'),
	%read(U),
	executeLoop(EntryS4,[('for',[_=Line],[_,Condition,Advance,('body',_,B)])|RestInstructions],N1,Value,OutS).

executeLoop(EntryS,[('for',[_=Line],_)|RestInstructions],N,0,OutS):-N >= 0,!,
	state(EntryS,Table,Cin,Cout,Trace),
		append(Trace,[' '],Space),
		append(Space,[Line],Trace1),
	state(EntryS1,Table,Cin,Cout,Trace1),
	write('ha casado con el salirse for\n'),
	%read(A),
	write(Table),write('\n'),
	execute(EntryS1,RestInstructions,OutS).
