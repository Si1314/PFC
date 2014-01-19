					%%%%%%%%%%%%%%%%%%%%%%%%%%%
					%		INTERPRETER		  %
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
	load_xml_file('plantillaExpresiones.xml', Program),
	%load_xml_file('plantillaIF.xml', Program),
	%load_xml_file('plantillaWHILE.xml', Program),
	%load_xml_file('plantillaFOR.xml', Program),

	updateTV([]),!,
	removeEmpty(Program,GoodProgram),
	execute(GoodProgram),

	getTV(TV),
	write('\nVariables final list:\n'),
	write(TV),
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
	functionOrMethod(ExitValue,FunOrMet),
	getTV(TV),
	append(TV,[(ExitValue,FunctionName,FunOrMet)],TVupdated),
	updateTV(TVupdated),
	execute(FuncionBody).

step(('param',[_=ParamType,_=ParamName],ParamBody)) :- !,
	add((ParamType,ParamName,'')),
	execute(ParamBody).

step(('body',_,Body)) :- !,
	execute(Body).

step(('declaration',[_=Type,_=Name],DecBody)):- !,
	add((Type,Name,'')),
	execute(DecBody).

step(('assignment',[_=Name],[AssigBody])) :- !,
	resolveExpression(AssigBody,Value),
	update((Name,Value)).

% IF -> THEN
step(('if',_,[Condition,('then',_,Then),_])):- !,
	evaluate(Condition), !,
	execute(Then).

% IF -> ELSE
step(('if',_,[_,_,('else',_,Else)])):- !,
	execute(Else).

% WHILE -> TRUE
step(('while',_,[Condition,('body',_,WhileBody)])):-
	evaluate(Condition), !,
	execute(WhileBody),
	step(('while',_,[Condition,('body',_,WhileBody)])).

% WHILE -> FALSE
step(('while',_,_)):-!.

% FOR
step(('for',_,[Variable,Condition,Advance,('body',_,ForBody)])):-
	variableAdvance(Variable,VariableName),
	evaluate(Condition), !,
	execute(ForBody),
	execute([Advance]),
	step(('for',_,[VariableName,Condition,Advance,('body',_,ForBody)])).

% FOR -> WE GO OUT
step(('for',_,_)):-!.

step(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

						%%%%%%%%%%%%%%%%%%
						%	EXPRESSIONS  %
						%%%%%%%%%%%%%%%%%%

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

work('=<', Op1,Op2, true):- Op1 =< Op2, !.
work('=<', _,_,false):- !.

work('<', Op1,Op2,true):- Op1 < Op2, !.
work('<', _,_,false):- !.

work('>=', Op1,Op2,true):- Op1 >= Op2, !.
work('>=', _,_,false):- !.

work('>', Op1,Op2,true):- Op1 > Op2, !.
work('>', _,_,false):- !.

work('==', Op1,Op2,true):- Op1 = Op2, !.
work('==', _,_,false):- !.

work('!=', Op1,Op2,true):- Op1 \= Op2, !.
work('!=', _,_,false):- !.

%					--------------------
%					---> arithmetic <---		
%					--------------------

work('+', Op1,Op2,Z):- !, Z is Op1 + Op2.
work('-', Op1,Op2,Z):- !, Z is Op1 - Op2.
work('*', Op1,Op2,Z):- !, Z is Op1 * Op2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%%%%%%%%%%%%%%%%%%%%%%%%%%%
					%	FUNCIONES AUXILIARES  %
					%%%%%%%%%%%%%%%%%%%%%%%%%%%

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