					%%%%%%%%%%%%%%%%%%%%%%%%%%%
					%       INTERPRETER       %
					%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-use_module(library(sgml)).
:-use_module(library(clpfd)).


:- include('VariablesTable.pl').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% First you have to keep this file in a folder called "PFC"
% Then open swi Prolog and write "interpreter." to taste it

% Carga el arbol dado por xml en 'Program'

interpreter :-
	cd('../PFC'),
	% Choose one to execute:
	load_xml_file('plantillaExpresionesSim.xml', Program),
	%load_xml_file('plantillaIF.xml', Program),
	%load_xml_file('plantillaWHILE.xml', Program),
	%load_xml_file('plantillaFOR.xml', Program),

	updateTV([]),!,
	removeEmpty(Program,GoodProgram),
	execute(GoodProgram),
	labelList(Table),
	write(Table), write('\n'),
	labeling([],Table),
	write('\nVariables final list:\n'), write('\n'),
	printTable,
	removeTable.

					%%%%%%%%%%%
					% execute %
					%%%%%%%%%%%

execute([]).
execute([Instruction|RestInstructios]) :-
	!,
	step(Instruction),
	execute(RestInstructios).


					%%%%%%%%
					% STEP %
					%%%%%%%%

step(('function',[_=ExitValue,_=FunctionName],FuncionBody)) :- !,
	apila,
	functionOrMethod(ExitValue,FunOrMet),
	add((ExitValue,FunctionName,FunOrMet)),
	execute(FuncionBody).
	%desapila.

step(('param',[_=int,_=ParamName],ParamBody)) :- !,
	[Value] ins 0..256,
	add((int,ParamName,Value)),
	execute(ParamBody).

step(('param',[_=ParamType,_=ParamName],ParamBody)) :- !,
	add((ParamType,ParamName,_)),
	execute(ParamBody).

step(('body',_,Body)) :- !,
	apila,
	execute(Body),
	desapila.

step(('declaration',[_=int,_=Name],DecBody)):- !,
	
	[Value] ins 0..256,
	add((int,Name,Value)),
	execute(DecBody).

step(('declaration',[_=Type,_=Name],DecBody)):- !,
	add((Type,Name,_)),
	execute(DecBody).

step(('assignment',[_=Name],[AssigBody])) :- !,
	resolveExpression(AssigBody,Value),
	update((Name,Value)).

% IF -> THEN
step(('if',_,[Condition,('then',_,Then),_])):- !,
	evaluate(Condition), !,
	apila,
	execute(Then),
	desapila.

% IF -> ELSE
step(('if',_,[_,_,('else',_,Else)])):- !,
	apila,
	execute(Else),
	desapila.

% WHILE -> TRUE
step(('while',_,[Condition,('body',_,WhileBody)])):-
	evaluate(Condition), !,
	apila,
	execute(WhileBody),
	desapila,
	step(('while',_,[Condition,('body',_,WhileBody)])).

% WHILE -> FALSE
step(('while',_,_)):-!.

% FOR
step(('for',_,[Variable,Condition,Advance,('body',_,ForBody)])):-
	variableAdvance(Variable,VariableName),
	evaluate(Condition), !,
	apila,
	execute(ForBody),
	desapila,
	execute([Advance]),
	step(('for',_,[VariableName,Condition,Advance,('body',_,ForBody)])).

% FOR -> WE GO OUT
step(('for',_,_)):-!,write('\n'), printTable, write('\n'), desapila.

step(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

						%%%%%%%%%%%%%%%%%%%
						%   EXPRESSIONS   %
						%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%--- resolveExpression ---%

resolveExpression(('binaryOperator',Operator,[X,Y]),Result):- !,
	getContent(Operator,Op),
	resolveExpression(X, Operand1),
	resolveExpression(Y, Operand2),
	work(Op, Operand1, Operand2, Result).

resolveExpression(('variable',[_=OperandName],_), OperandValue):- !,
	getValue(OperandName,OperandValue).

resolveExpression(('constant',[_=Value],_),Result):- !,
	atom_number(Value,Result).

resolveExpression(_,0).

%					-----------------
%					---> Boolean <---
%					-----------------

work('=<', Op1,Op2, true):- Op1 #=< Op2.
work('=<', _,_,false):- !.

work('<', Op1,Op2,true):- Op1 #< Op2.
work('<', _,_,false):- !.

work('>=', Op1,Op2,true):- Op1 #>= Op2.
work('>=', _,_,false):- !.

work('>', Op1,Op2,true):- Op1 #> Op2.
work('>', _,_,false):- !.

work('==', Op1,Op2,true):- Op1 #= Op2.
work('==', _,_,false):- !.

work('!=', Op1,Op2,true):- Op1 #\= Op2.
work('!=', _,_,false):- !.

%					--------------------
%					---> arithmetic <---		
%					--------------------

work('+', Op1,Op2,Z):- !, Z #= Op1 + Op2.
work('-', Op1,Op2,Z):- !, Z #= Op1 - Op2.
work('*', Op1,Op2,Z):- !, Z #= Op1 * Op2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%%%%%%%%%%%%%%%%%%%%%%%%%%%%
					%   FUNCIONES AUXILIARES   %
					%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
					
					%--- evaluate ---%

evaluate((_, [_, _= (Op)], [(_,[_=Operand1],_),(_,[_=Operand2],_)])):-
	getValue(Operand1,Value1),
	getValue(Operand2,Value2),
	work(Op, Value1, Value2, Result), Result.

%------------------------------------------------------------------------------------

					%--- variableAdvance ---%

variableAdvance(('variableField',_,Variable),VarName):-
	getContent(Variable,VarName), !,
	apila,
	execute(Variable).

variableAdvance(Variable,Variable).

%------------------------------------------------------------------------------------

					%--- functionOrMethod ---%

% Diferenciamos una funcion de un metodo.

functionOrMethod(void,'method'):- !.
functionOrMethod(_,'function').

%------------------------------------------------------------------------------------

					%--- getContent ---%

getContent([_,_= (Op)], Op):- !.
getContent((_,[_=Name],_), Name):- !.
getContent([('declaration',[_,_=VariableName],_), _] , VariableName):- !.

%------------------------------------------------------------------------------------

					%--- removeEmpty ---%

removeEmpty(List,ReturnedList):-
	removeEmptyAux(List,[],ReturnedList).

removeEmptyAux([],Ac,Ac).

removeEmptyAux([element(X,Y,Z)|List],Ac,ReturnedList):- !,
	removeEmpty(Z,Z1),
	append(Ac,[(X,Y,Z1)],Acc),
	removeEmptyAux(List,Acc,ReturnedList).

removeEmptyAux([_|List],Ac,ReturnedList):-
	removeEmptyAux(List,Ac,ReturnedList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%