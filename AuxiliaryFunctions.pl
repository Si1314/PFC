%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%%%%%%%%%%%%%%%%%%%%%%%%%%%%
					%    AUXILIARY FUNCTIONS   %
					%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

variableAdvance(Entry,('declarations',_,Variable),VarName,Out):-
	getContent(Variable,VarName), !,
	execute(Entry,Variable,Out).

variableAdvance(Entry,Variable,Variable,Entry).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getContent([_= (Op)], Op):- !.
getContent([_,_= (Op)], Op):- !.
getContent((_,[_=Name],_), Name):- !.
getContent([('declaration',[_,name=VariableName],_)],VariableName).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
	writeInXML2(Stream,N,V),	% quitar el "2" para guardar bien en el XML
	writeList(Stream,Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getTuple((int,ret,Value)):-	% TODO
	inf(X), sup(Y),
	Value in X..Y.

getTuple(int,(int,ret,Value)):-
	inf(X), sup(Y),
	Value in X..Y.

getTuple(bool,(int,ret,Value)):-
	Value in true\/false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookForFunction([],_,[]):-!.

lookForFunction([(_,[name=Name,type=Type],Body)|_],Name,Type,Body):- !.

lookForFunction([_|Xs],Name1,Type,Result):-
	lookForFunction(Xs,Name1,Type,Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addListParams(Entry,[],Entry):-!.
addListParams(Entry,[(param,[name=Name,type=Type],_)|Xs],Out):-
	getValue(Entry,Name,Value),
	add(Entry,(Type,Name,Value),Out1),
	addListParams(Out1,Xs,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

createListParams(Xs,Body,ListParams):-
	createListParamsAux(Xs,[],Body,ListParams).

createListParamsAux([],Ac,[],Ac):-!.
createListParamsAux([(body,_,Body)],Ac,Body,Ac):-!.
createListParamsAux([(param,[_,name=Name],_)|Xs],Ac,Body,ListParams):-
	append(Ac,[Name],Ac1),
	createListParamsAux(Xs,Ac1,Body,ListParams).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

returnesValue([X|_],ValueReturned):-
	returnesValueAux(X,ValueReturned).

returnesValueAux([],[]):-!.
returnesValueAux([(_,ret,Value)|_],Value):-!.
returnesValueAux([_|Xs],Return):-
	returnesValueAux(Xs,Return).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

updateReturnValue(Entry,Out):-
	returnesValue(Entry,ValueReturned),
	updateReturnValueAux(Entry,ValueReturned,Out).

updateReturnValueAux([X,Y|Xs],ValueReturned,[X,Out1|Xs]):-
	update([Y|[]],('ret',ValueReturned),[Out1]).
