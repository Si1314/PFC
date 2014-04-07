					%%%%%%%%%%%%%%%%%%%%%%%%%%%
					%       INTERPRETER       %
					%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-use_module(library(sgml)).
:-use_module(library(clpfd)).
:-use_module(library(sgml_write)).

:- include('VariablesTable.pl').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% First you have to keep this file in a folder called "PFC"
% Then open swi Prolog and write "interpreter." to taste it

% Carga el arbol dado por xml en 'Program'

interpreter(EntryFile, OutFile):- 
	findall((N,L),interpreterAux(EntryFile,N,L),V),
	%interpreterAux(N,L),
	%write(V), write('\n'),
	open(OutFile, write, Stream, []),

    xml_write(Stream,element(table,[],[]),[header(false)]),
    xml_write(Stream,'\n',[header(false)]),
    writeList(Stream,V),
   	%writeList(Stream,(N,L)),
    close(Stream).

interpreterAux(EntryFile,LabelTableNames, LabelTableValues):-
	cd('../PFC'),

	% Choose one to execute:
	%load_xml_file('salida.xml', Program),
	%load_xml_file('salida2.xml', Program),
	%load_xml_file('salida3.xml', Program),
	load_xml_file(EntryFile, Program),
	%load_xml_file('plantillaExpresionesSim.xml', Program),
	%load_xml_file('plantillaExpresiones.xml', Program),
	%load_xml_file('plantillaIF.xml', Program),
	%load_xml_file('plantillaWHILE.xml', Program),
	%load_xml_file('plantillaFOR.xml', Program),

	removeEmpty(Program,GoodProgram),
	execute([],GoodProgram,ExitTable),
	%write(ExitTable),
	labelList(ExitTable,LabelTableNames,LabelTableValues),

	%findall(LabelTableValues,(callLabel(LabelTableValues,0,[],Sol)),Solution),!,
	%findall(LabelTableValues,label(LabelTableValues),Solution),!,
	%once(label(LabelTableValues)).
	label(LabelTableValues).
	%write(Solution), write('\n').

					%%%%%%%%%%%
					% execute %
					%%%%%%%%%%%

execute(Entry,[],Entry):-!.

execute(Entry,[('while',_,[C,('body',_,B)])|RestInstructios],Out) :-!,
	step(Entry,('while',_,[C,('body',_,B)]),5,Out1),	% Nivel = 5
	%execute(Entry,[('while',_,[C,('body',_,B)])|RestInstructios],Out),
	execute(Out1,RestInstructios,Out).

execute(Entry,[('for',_,[V,C,A,('body',_,B)])|RestInstructios],Out) :-!,
	step(Entry,('for',_,[V,C,A,('body',_,B)]),5,Out1),	% Nivel = 5
	execute(Out1,RestInstructios,Out).

execute(Entry,[Instruction|RestInstructios],Out) :-
	step(Entry,Instruction,Out1),
	execute(Out1,RestInstructios,Out).


					%%%%%%%%
					% STEP %
					%%%%%%%%

step(Entry,('function',[_,_=void],FuncionBody),Out) :- !,
	apila(Entry,Entry1),
	execute(Entry1,FuncionBody,Out).

step(Entry,('function',[_,_=ExitValue],FuncionBody),Out) :- !,
	apila(Entry,Entry1),
	getTuple(ExitValue,Tuple),
	add(Entry1,Tuple,Out1),
	execute(Out1,FuncionBody,Out).

step(Entry,('param',[_=int,_=ParamName],ParamBody),Out) :- !,
	[Value] ins -3..3,
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
	[Value] ins -3..3,
	add(Entry,(int,Name,Value),Out1),
	execute(Out1,DecBody,Out).

step(Entry,('declaration',[_=Type,_=Name],DecBody),Out):- !,
	add(Entry,(Type,Name,_),Entry1),
	execute(Entry1,DecBody,Out).

step(Entry,('assignment',[_=Name],[AssigBody]),Out) :- !,
	resolveExpression(Entry,AssigBody,Value),
	update(Entry,(Name,Value),Out).

% IF -> THEN
step(Entry,('if',_,[Condition,('then',_,Then),_]),Out):-
	resolveExpression(Entry,Condition,'true'),
	%evaluate(Entry,Condition),
	apila(Entry,Out1),
	execute(Out1,Then,Out2),
	desapila(Out2, Out).

% IF -> ELSE
step(Entry,('if',_,[_,_,('else',_,Else)]),Out):- !,
	apila(Entry,Out1),
	execute(Out1,Else,Out2),
	desapila(Out2, Out).

step(Entry,('return',_,[Body]),Out):-!,
	resolveExpression(Entry,Body,Result),
	update(Entry,(ret,Result),Out).

step(Entry,_,Entry).

% FOR
step(Entry,('for',_,_),0,Entry):-!.

step(Entry,('for',_,[Variable,Condition,Advance,('body',_,ForBody)]),N,Out):-
	variableAdvance(Entry,Variable,VariableName,Entry1),
	resolveExpression(Entry1,Condition,true),!,
	apila(Entry1,Out1),
	execute(Out1,ForBody,Out2),
	desapila(Out2,Out3),
	execute(Out3,[Advance],Out4),
	N1 is N - 1,
	step(Out4,('for',_,[VariableName,Condition,Advance,('body',_,ForBody)]),N1,Out).

%step(Entry,('for',_,_),_,Entry):-!.

% WHILE
step(Entry,('while',_,_),0,Entry):-!.

step(Entry,('while',_,[Condition,('body',_,WhileBody)]),N,Out):-
	resolveExpression(Entry,Condition,true), !,
	apila(Entry,Out1),
	execute(Out1,WhileBody,Out2),
	desapila(Out2,Out3),
	N1 is N - 1,
	step(Out3,('while',_,[Condition,('body',_,WhileBody)]),N1,Out).

%step(Entry,('while',_,_),_,Entry):-!.%:-resolveExpression(Entry,Condition,false),!.

step(Entry,_,_,Entry).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

						%%%%%%%%%%%%%%%%%%%
						%   EXPRESSIONS   %
						%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%--- resolveExpression ---%

resolveExpression(Entry,('binaryOperator',Operator,[X,Y]),Result):-
	getContent(Operator,Op),
	resolveExpression(Entry,X, Operand1),
	resolveExpression(Entry,Y, Operand2),
	work(Op, Operand1, Operand2,Result).

resolveExpression(Entry,('variable',[_=OperandName],_),OperandValue):-
	getValue(Entry,OperandName,OperandValue).

resolveExpression(_,('const',[_=Value],_),Result):- 
	atom_number(Value,Result).

%resolveExpression(_,_,0).

%					-----------------
%					---> Boolean <---
%					-----------------

work('=<', Op1,Op2, true):- Op1 #=< Op2.
work('=<', _,_,false).

work('<', Op1,Op2,true):- Op1 #< Op2.
work('<', _,_,false).

work('>=', Op1,Op2,true):- Op1 #>= Op2.
work('>=', _,_,false).

work('>', Op1,Op2,true):- Op1 #> Op2.
work('>', _,_,false).

work('==', Op1,Op2,true):- Op1 #= Op2.
work('==', _,_,false).

work('!=', Op1,Op2,true):- Op1 #\= Op2.
work('!=', _,_,false).

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

					%--- variableAdvance ---%

variableAdvance(Entry,('declarations',_,Variable),VarName,Out):-
	getContent(Variable,VarName), !,
	apila(Entry, Out1),
	execute(Out1,Variable,Out2),
	desapila(Out2,Out).

variableAdvance(Entry,Variable,Variable,Entry).


%------------------------------------------------------------------------------------

					%--- getContent ---%

getContent([_,_= (Op)], Op):- !.
getContent((_,[_=Name],_), Name):- !.
getContent([('declaration',[_,name=VariableName],_)],VariableName).

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

writeInXML2(Stream,_,[]):-!,
    xml_write(Stream,element(table,[],[]),[header(false)]),
    xml_write(Stream,'\n',[header(false)]).

writeInXML2(Stream,[],_):-!,
    xml_write(Stream,element(table,[],[]),[header(false)]),
    xml_write(Stream,'\n',[header(false)]).

writeInXML2(Stream,[N|Ns],[V|Vs]):-
    xml_write(Stream,element(variable,[name=N,value=V],[]),[header(false)]),
	xml_write(Stream,'\n',[header(false)]),
	writeInXML2(Stream, Ns, Vs). 

	% Possible future useful code:
	% xml_write(Stream,element(aap,[],[noot]),[]), 
	% Possible Options: [layout(false),doctype(xml),header(true)]

%------------------------------------------------------------------
writeInXML(Stream,[N|Ns],[V|Vs]):-
	writeInXMLAux(Stream,[N|Ns],[V|Vs],[],Result),
	xml_write(Stream,element(caso,[],Result),[header(false)]),
	xml_write(Stream,'\n',[header(false)]).

writeInXMLAux(_,[],_,Ac,Ac):-!.
writeInXMLAux(_,_,[],Ac,Ac):-!.
writeInXMLAux(Stream,[N|Ns],[V|Vs],Ac,Zs):-
	append(Ac,[element(variable,[name=N,value=V],[])],Ac1),
	writeInXMLAux(Stream,Ns,Vs,Ac1,Zs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

callLabel(_,0,Ac,Ac):-!.
callLabel(LabelTableValues,Nivel,Ac,Sol1):-
	label(LabelTableValues),
	append(Ac,[LabelTableValues],Sol),
	Nivel1 is Nivel - 1,
	callLabel(LabelTableValues,Nivel1,Sol,Sol1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

writeList(_,[]):- !.
writeList(Stream,[(N,V)|Xs]):- !,
	writeInXML(Stream,N,V),	% quitar el "2" para guardar bien en el XML
	writeList(Stream,Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getTuple(int,(int,ret,Value)):-
	Value in -3..3.

getTuple(bool,(int,ret,Value)):-
	Value in true\/false.