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

	removeEmpty(Program,GoodProgram),
	execute([],GoodProgram,ExitTable),
	labelList(ExitTable,LabelTable),
	label(LabelTable),
	write(LabelTable), write('\n').

					%%%%%%%%%%%
					% execute %
					%%%%%%%%%%%

execute(Entry,[],Entry).
execute(Entry,[Instruction|RestInstructios],Out) :-
	!,
	step(Entry,Instruction,Out1),
	execute(Out1,RestInstructios,Out).


					%%%%%%%%
					% STEP %
					%%%%%%%%

step(Entry,('function',[_=ExitValue,_=FunctionName],FuncionBody),Out) :- !,
	apila(Entry,Entry1),
	functionOrMethod(ExitValue,FunOrMet),
	add(Entry1,(ExitValue,FunctionName,FunOrMet),Out1),
	execute(Out1,FuncionBody,Out).
	%desapila.

step(Entry,('param',[_=int,_=ParamName],ParamBody),Out) :- !,
	[Value] ins 0..256,
	add(Entry,(int,ParamName,Value),Out1),
	execute(Out1,ParamBody,Out).

step(Entry,('param',[_=ParamType,_=ParamName],ParamBody),Out) :- !,
	add(Entry,(ParamType,ParamName,_),Out1),
	execute(Out1,ParamBody,Out).

step(Entry,('body',_,Body),Out) :- !,
	apila(Entry, Out1),
	execute(Out1,Body,Out2),
	desapila(Out2, Out).

step(Entry,('declaration',[_=int,_=Name],DecBody),Out):- !,
	
	[Value] ins 0..256,
	add(Entry,(int,Name,Value),Out1),
	execute(Out1,DecBody,Out).

step(Entry,('declaration',[_=Type,_=Name],DecBody)):- !,
	add((Type,Name,_)),
	execute(DecBody).

step(Entry,('assignment',[_=Name],[AssigBody]),Out) :- !,
	resolveExpression(Entry,AssigBody,Value,Out1),
	update(Out1,(Name,Value),Out).

% IF -> THEN
%step(('if',_,[Condition,('then',_,Then),_])):- !,
	%evaluate(Condition), !,
	%apila(),
	%execute(Then),
	%desapila().

% IF -> ELSE
%step(('if',_,[_,_,('else',_,Else)])):- !,
	%apila,
	%execute(Else),
	%desapila.

% WHILE -> TRUE
%step(('while',_,[Condition,('body',_,WhileBody)])):-
	%evaluate(Condition), !,
	%apila,
	%execute(WhileBody),
	%desapila,
	%step(('while',_,[Condition,('body',_,WhileBody)])).

% WHILE -> FALSE
%step(('while',_,_)):-!.

% FOR
%step(('for',_,[Variable,Condition,Advance,('body',_,ForBody)])):-
	%variableAdvance(Variable,VariableName),
	%evaluate(Condition), !,
	%apila,
	%execute(ForBody),
	%desapila,
	%execute([Advance]),
	%step(('for',_,[VariableName,Condition,Advance,('body',_,ForBody)])).

% FOR -> WE GO OUT
%step(('for',_,_)):-!,write('\n'), printTable, write('\n'), desapila.

step(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

						%%%%%%%%%%%%%%%%%%%
						%   EXPRESSIONS   %
						%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%--- resolveExpression ---%

resolveExpression(Entry,('binaryOperator',Operator,[X,Y]),Result,Out):- !,
	getContent(Entry,Operator,Op,Entry1),
	resolveExpression(Entry1,X, Operand1,Out1),
	resolveExpression(Out1,Y, Operand2,Out),
	work(Op, Operand1, Operand2, Result).

resolveExpression(Entry,('variable',[_=OperandName],_), OperandValue,Out):- !,
	getValue(Entry,OperandName,OperandValue,Out).

resolveExpression(Entry,('constant',[_=Value],_),Result,Out):- !,
	atom_number(Entry,Value,Result,Out).

resolveExpression(Entry,_,0,Entry).

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

evaluate(Entry,(_, [_, _= (Op)], [(_,[_=Operand1],_),(_,[_=Operand2],_)]),Out):-
	getValue(Entry,Operand1,Value1,Entry1),
	getValue(Entry1,Operand2,Value2,Out),
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

removeEmpty(Entry,List,ReturnedList,Out):-
	removeEmptyAux(Entry,List,[],ReturnedList,Out).

removeEmptyAux(Entry,[],Ac,Ac,Entry).

removeEmptyAux(Entry,[element(X,Y,Z)|List],Ac,ReturnedList,Out):- !,
	removeEmpty(Entry,Z,Z1,Entry1),
	append(Entry1,Ac,[(X,Y,Z1)],Acc,Out1),
	removeEmptyAux(Out1,List,Acc,ReturnedList,Out).

removeEmptyAux(Entry,[_|List],Ac,ReturnedList,Out):-
	removeEmptyAux(Entry,List,Ac,ReturnedList,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%