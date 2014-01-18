					%%%%%%%%%%%%%%%%%%%%%%%%%%%
					%		INTERPRETER		  %
					%%%%%%%%%%%%%%%%%%%%%%%%%%%

use_module(library(sgml)).

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

	removeEmpty(Program,GoodProgram),
	execute(GoodProgram,[],TV),

	write('\nVariables final list:\n'),
	write(TV).

					%%%%%%%%%%%
					% execute %
					%%%%%%%%%%%

execute([],TV,TV).
execute([Instruction|RestInstructios],TV,TVupdated2) :-
	!,
	step(Instruction,TV,TVupdated1),
	execute(RestInstructios,TVupdated1,TVupdated2).


					%%%%%%%%
					% STEP %
					%%%%%%%%

step(('function',[_=ExitValue,_=FunctionName],FuncionBody),TV,TVupdated1) :- !,
	functionOrMethod(ExitValue,FunOrMet),
	append(TV,[(ExitValue,FunctionName,FunOrMet)],TVupdated),
	execute(FuncionBody,TVupdated,TVupdated1).

step(('param',[_=ParamType,_=ParamName],ParamBody),TV,TVupdated2) :- !,
	add(TV,(ParamType,ParamName,''),TVupdated1),
	execute(ParamBody,TVupdated1,TVupdated2).

step(('body',_,Body),TV,TVupdated) :- !,
	execute(Body,TV,TVupdated).

step(('declaration',[_=Type,_=Name],DecBody),TV,TVupdated2):- !,
	add(TV,(Type,Name,''),TVupdated1),
	execute(DecBody,TVupdated1,TVupdated2).

step(('assignment',[_=Name],[AssigBody]),TV,TVupdated) :- !,
	resolveExpression(AssigBody,TV,Value),
	update(TV,(Name,Value),TVupdated).

% IF -> THEN
step(('if',_,[Condition,('then',_,Then),_]),TV,TVupdated):- !,
	evaluate(Condition,TV), !,
	execute(Then,TV,TVupdated).

% IF -> ELSE
step(('if',_,[_,_,('else',_,Else)]),TV,TVupdated):- !,
	execute(Else,TV,TVupdated).

% WHILE -> TRUE
step(('while',_,[Condition,('body',_,WhileBody)]),TV,TVupdated2):-
	evaluate(Condition,TV), !,
	execute(WhileBody,TV,TVupdated1),
	step(('while',_,[Condition,('body',_,WhileBody)]),TVupdated1,TVupdated2).

% WHILE -> FALSE
step(('while',_,_),TVupdated1,TVupdated1):-!.

% FOR
step(('for',_,[Variable,Condition,Advance,('body',_,ForBody)]),TV,TVupdated3):-
	variableAdvance(Variable,TV,TVupdated,VariableName),
	evaluate(Condition,TVupdated), !,
	execute(ForBody,TVupdated,TVupdated1),
	execute([Advance], TVupdated1, TVupdated2),
	step(('for',_,[VariableName,Condition,Advance,('body',_,ForBody)]),TVupdated2,TVupdated3).

% FOR -> WE GO OUT
step(('for',_,_),TV,TV):-!.

step(_,TV,TV).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

						%%%%%%%%%%%%%%%%%%
						%	EXPRESSIONS  %
						%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%--- resolveExpression ---%

resolveExpression(('binaryOperator',Operator,[X,Y]), TV , Result):- !,
	getContent(Operator,Op),
	resolveExpression(X, TV, Operand1),
	resolveExpression(Y, TV, Operand2),
	work(Op, Operand1, Operand2, Result).

resolveExpression(('variable',[_=OperandName],_), TV, OperandValue):- !,
	getValue(TV, OperandName,OperandValue).

resolveExpression(('constant',[_=Value],_),_ ,Result):- !,
	atom_number(Value,Result).

resolveExpression(_,_,0).

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

					%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
					%	VARIABLES TABLE FUNCTIONS  %
					%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%--- update ---%

update(TV,Var,TVupdated):- updateAux(TV,Var,[],TVupdated).

updateAux([],_,TVaux,TVaux) .

updateAux([(Type,Name,_)|TV],(Name,Value),TVaux, TVresult):-
	!,
	append(TVaux,[(Type,Name,Value)],TVupdatedAux),
	append(TVupdatedAux,TV,TVresult).

updateAux([(Type,Name1,Value)|TV],(Name2,V),TVaux, TVupdated):-
	append(TVaux,[(Type,Name1,Value)],TVupdatedAux),
	updateAux(TV, (Name2,V), TVupdatedAux, TVupdated).
	
%------------------------------------------------------------------------------------

					%--- add ---%

add(TV,(Type,Name,Value),TVupdated):-
	notInTable(TV,Name),
	append(TV,[(Type,Name,Value)],TVupdated).

%------------------------------------------------------------------------------------

					%--- notInTable ---%

notInTable([(_,Name,_)|_],Name) :- !, false.
notInTable([_|Rest],Name1) :-!,
	notInTable(Rest,Name1).
notInTable(_,_):-true.

%------------------------------------------------------------------------------------

					%--- getVariable ---%

getVariable([(Type,Name,Value)|_],Name,(Type,Name,Value)):- !.

getVariable([_|Rest],Name,ValueReturned):-
	getVariable(Rest,Name,ValueReturned).

%------------------------------------------------------------------------------------

					%--- getValue ---%

getValue([(_,OperandName,Result)|_], OperandName, Result):- !.
getValue([_|Rest], OperandName, Result):-
	getValue(Rest, OperandName, Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


					%%%%%%%%%%%%%%%%%%%%%%%%%%%
					%	FUNCIONES AUXILIARES  %
					%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
					
					%--- evaluate ---%

evaluate((_, [_, _= (Op)], [(_,[_=Operand1],_),(_,[_=Operand2],_)]), TV):-
	getValue(TV,Operand1,Value1),
	getValue(TV,Operand2,Value2),
	work(Op, Value1, Value2, Result), Result.

%------------------------------------------------------------------------------------

					%--- variableAdvance ---%

variableAdvance(('variableField',_,Variable),TV,TVupdated,VarName):-
	getContent(Variable,VarName), !,
	execute(Variable,TV,TVupdated).

variableAdvance(Variable,TV,TV,Variable).

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

%------------------------------------------------------------------------------------