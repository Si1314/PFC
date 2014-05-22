%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%%%%%%%%%%%%%%%%%%%%%%%%%%%%
					%    AUXILIARY FUNCTIONS   %
					%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Devuelve la variable de avance en un "for", si no esta declarada la declara
% si esta declarada solo devuelve el nombre

variableAdvance(Entry,('declarations',_,Variable),VarName,Out):-
	getContent(Variable,VarName), !,
	%write(hechoGetContent),write('\n'),
	%write(Variable),write('\n'),
	execute(Entry,Variable,Out).
	%write(execute),write('\n').

variableAdvance(Entry,Variable,Variable,Entry).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Devuelve el contenido de lo que se le pase por parametro: una "op", el nombre de la variable...

getContent([('declaration',[_,name=VariableName,_],_)],VariableName):-!.
getContent([Op], Op):- !.
getContent([_= (Op)], Op):- !.
getContent([_,_= (Op)], Op):- !.
getContent((_,[_=Name],_), Name):- !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Permite conformar un State = (Tabla,ConsolaInput,ConsolaOutput,Trace)

state((T,Cinput,Coutput,Trace),T,Cinput,Coutput,Trace).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Limpia la lista de entrada de cosas que no tengan la estructura "element(_,_,_)"

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

% Escribe en Stream los casos con los nombres y valores que se le pasen en las listas
% (N,V) = (nombre,valor)

writeList(_,[]):- !.
writeList(Stream,[(N,V,T,Cin,Cout)|Xs]):- !,
	%writeInXML(Stream,N,V),	% writeInXML para guardar bien en el XML, writeInXML2 para guardarlo mal
	%writeInXML(Stream,T,Cin,Cout),
	writeInXML(Stream,N,V,T,Cin,Cout),
	writeList(Stream,Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Escribe en Stream un nuevo caso con los nombres y valores que se le pasen en las listas
% [N|Ns] para nombres
% [V|Vs] para valores

writeInXML(Stream,[N|Ns],[V|Vs]):-
	writeInXMLAux(Stream,[N|Ns],[V|Vs],[],Result),
	xml_write(Stream,element(caso,[],Result),[header(false)]),
	xml_write(Stream,'\n',[header(false)]).

writeInXML(Stream,T,Cin,Cout):-
	xml_write(Stream,
		element(data,[],[
			element(traza,[],T),
			element(cin,[],Cin),
			element(cout,[],Cout)
		]),[header(false)]),
	xml_write(Stream,'\n',[header(false)]).

writeInXML(Stream,[N|Ns],[V|Vs],T,Cin,Cout):-
	writeInXMLAux(Stream,[N|Ns],[V|Vs],[],Result),
	append(Result, [element(data,[],[
			element(traza,[],T),
			element(cin,[],Cin),
			element(cout,[],Cout)
		])], R),
	xml_write(Stream,element(caso,[],R),[header(false)]),
	xml_write(Stream,'\n',[header(false)]).

writeInXMLAux(_,[],_,Ac,Ac):-!.
writeInXMLAux(_,_,[],Ac,Ac):-!.
writeInXMLAux(Stream,[N|Ns],[V|Vs],Ac,Zs):-
	append(Ac,[element(variable,[name=N,value=V],[])],Ac1),
	writeInXMLAux(Stream,Ns,Vs,Ac1,Zs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% devuelve la variable de retorno "ret" como una tupla: (int,ret,Value)

getTuple((int,ret,Value)):-	% TODO types
	inf(X), sup(Y),
	Value in X..Y.

getTuple(int,(int,ret,Value)):-
	inf(X), sup(Y),
	Value in X..Y.

getTuple(bool,(int,ret,Value)):-
	Value in 0..1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Buscamos en la lista de funciones una función en concreto pasandole el nombre

lookForFunction([],_,_,[]):-!.
lookForFunction([(_,[name=Name,type=Type,_],Body)|_],Name,Type,Body):- !.
lookForFunction([_|Xs],Name1,Type,Result):-
	lookForFunction(Xs,Name1,Type,Result).

lookForFunction([],_,[]):-!.
lookForFunction([(function,[name=Name,Type,Line],Body)|_],Name,[(function,[name=Name,Type,Line],Body)]):- !.
lookForFunction([_|Xs],Name1,Result):-
	lookForFunction(Xs,Name1,Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Añade a la tabla de variablse los parámetros pasados en la lista

addListParams(Entry,[],Entry):-!.
addListParams(Entry,[(param,[name=Name,type=Type],_)|Xs],Out):-
	getValue(Entry,Name,Value),
	add(Entry,(Type,Name,Value),Out1),
	addListParams(Out1,Xs,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Crea una lista sólo con los nombres de los parámetros

createListParams(Xs,Body,ListParams):-
	createListParamsAux(Xs,[],Body,ListParams).

createListParamsAux([],Ac,[],Ac):-!.
createListParamsAux([(body,_,Body)],Ac,Body,Ac):-!.
createListParamsAux([(param,[_,name=Name],_)|Xs],Ac,Body,ListParams):-
	append(Ac,[Name],Ac1),
	createListParamsAux(Xs,Ac1,Body,ListParams).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Devuelve el valor de la variable de retorno "ret"

returnesValue([X|_],ValueReturned):-
	returnesValueAux(X,ValueReturned).

returnesValueAux([],[]):-!.
returnesValueAux([(_,ret,Value)|_],Value):-!.
returnesValueAux([_|Xs],Return):-
	returnesValueAux(Xs,Return).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Actualiza el valor de la variable de retorno "ret"

updateReturnValue(Entry,Out):-
	returnesValue(Entry,ValueReturned),
	updateReturnValueAux(Entry,ValueReturned,Out).

updateReturnValueAux([X,Y|Xs],ValueReturned,[X,Out1|Xs]):-
	update([Y|[]],('ret',ValueReturned),[Out1]).
