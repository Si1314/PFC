					%%%%%%%%%%%%%%%%%%%%%%%%%%%
					%       INTERPRETER       %
					%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-use_module(library(sgml)).
:-use_module(library(clpfd)).
:-use_module(library(sgml_write)).

:- include('VariablesTableSimbolico.pl').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% First you have to keep this file in a folder called "PFC"
% Then open swi Prolog and write "interpreter." to taste it

% Carga el arbol dado por xml en 'Program'

interpreter :-
	cd('../PFC'),

	% Choose one to execute:
	load_xml_file('plantillaExpresionesSim.xml', Program),
	
	%load_xml_file('plantillaExpresiones.xml', Program),
	%load_xml_file('plantillaIF.xml', Program),
	%load_xml_file('plantillaWHILE.xml', Program),
	%load_xml_file('plantillaFOR.xml', Program),

	removeEmpty(Program,GoodProgram),
	execute([],GoodProgram,ExitTable),
	write(ExitTable),write('\n'),
	labelList(ExitTable,LabelTableNames,LabelTableValues),
	once(label(LabelTableValues)),!,
	write(LabelTableNames), write('\n'),
	write(LabelTableValues), write('\n'),

	open('output.txt', write, Stream, [encoding(utf8)]), 
    writeInXML(Stream,LabelTableNames,LabelTableValues).

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

step(Entry,('declaration',[_=Type,_=Name],DecBody),Out):- !,
	add(Entry,(Type,Name,_),Entry1),
	execute(Entry1,DecBody,Out).

step(Entry,('assignment',[_=Name],[AssigBody]),Out) :- !,
	resolveExpression(Entry,AssigBody,Value),
	update(Entry,(Name,Value),Out).

% IF -> THEN
step(Entry,('if',_,[Condition,('then',_,Then),_]),Out):- !,
	evaluate(Entry,Condition), !,
	apila(Entry,Out1),
	execute(Out1,Then,Out2),
	desapila(Out2, Out).

% IF -> ELSE
step(Entry,('if',_,[_,_,('else',_,Else)]),Out):- !,
	apila(Entry,Out1),
	execute(Out1,Else,Out2),
	desapila(Out2, Out).

% WHILE -> TRUE
step(Entry,('while',_,[Condition,('body',_,WhileBody)]),Out):-
	evaluate(Entry,Condition), !,
	apila(Entry,Out1),
	execute(Out1,WhileBody,Out2),
	desapila(Out2,Out3),
	step(Out3,('while',_,[Condition,('body',_,WhileBody)]),Out).

% WHILE -> FALSE
step(Entry,('while',_,_),Entry):-!.

% FOR
step(Entry,('for',_,[Variable,Condition,Advance,('body',_,ForBody)]),Out):-
	variableAdvance(Entry,Variable,VariableName,Entry1), 													
	evaluate(Entry1,Condition), !,
	apila(Entry1,Out1),
	execute(Out1,ForBody,Out2),
	desapila(Out2,Out3),
	execute(Out3,[Advance],Out4),
	step(Out4,('for',_,[VariableName,Condition,Advance,('body',_,ForBody)]),Out).

% FOR -> WE GO OUT
step(Entry,('for',_,_),Out):-!,write('\n'), write(Entry), write('\n'), desapila(Entry,Out).

step(Entry,_,Entry).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

						%%%%%%%%%%%%%%%%%%%
						%   EXPRESSIONS   %
						%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%--- resolveExpression ---%

resolveExpression(Entry,('binaryOperator',Operator,[X,Y]),Result):- !,
	getContent(Operator,Op),
	resolveExpression(Entry,X, Operand1),
	resolveExpression(Entry,Y, Operand2),
	work(Op, Operand1, Operand2,Result).

resolveExpression(Entry,('variable',[_=OperandName],_), OperandValue):- !,
	getValue(Entry,OperandName,OperandValue).

resolveExpression(_,('constant',[_=Value],_),Result):- !,
	atom_number(Value,Result).

resolveExpression(_,_,0).

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

evaluate(Entry,(_, [_, _= (Op)], [(_,[_=Operand1],_),(_,[_=Operand2],_)])):-
	getValue(Entry,Operand1,Value1),
	getValue(Entry,Operand2,Value2),
	work(Op, Value1, Value2, Result), Result.

%------------------------------------------------------------------------------------

					%--- variableAdvance ---%

variableAdvance(Entry,('variableField',_,Variable),VarName,Out):-
	getContent(Variable,VarName), !,
	apila(Entry, Out1),
	execute(Out1,Variable,Out).

variableAdvance(Entry,Variable,Variable,Entry).

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

writeInXML(Stream,_,[]):-!, close(Stream).
writeInXML(Stream,[],_):-!, close(Stream).
writeInXML(Stream, [N|Ns],[V|Vs]):-
	xml_write(Stream,element(variable,[name=N,value=V],[]),[header(false)]),
	xml_write(Stream,'\n',[header(false)]),
	writeInXML(Stream, Ns, Vs). 

	% Possible future useful code:
	% xml_write(Stream,element(aap,[],[noot]),[]), 
	% Possible Options: [layout(false),doctype(xml),header(true)]
	