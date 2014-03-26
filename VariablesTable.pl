					%%%%%%%%%%%%%%%%%%%%%%%%%
					%    VARIABLES TABLE    %
					%%%%%%%%%%%%%%%%%%%%%%%%%

use_module(library(sgml)).

% createTable, add, getTV, getVariable, getValue, update, updateTV, notInTable

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%--- create and remove Table ---%

updateTV(TV):-
	retractall(tv(_)),
	assert(tv(TV)).

removeTable:- retractall(tv(_)).

%------------------------------------------------------------------------------------

					%--- add ---%

add((Type,Name,Value)):-
	notInTable(Name),!,
	getTV([TV|TVs]),
	append(TV,[(Type,Name,Value)],TVupdated),
	updateTV([TVupdated|TVs]).

%------------------------------------------------------------------------------------

					%--- apila / desapila ---%

apila:-
	getTV(TV),
	updateTV([[]|TV]).

desapila:-
	getTV([_|TV]),
	updateTV(TV).

%------------------------------------------------------------------------------------

					%--- getTV ---%

getTV(Table):- tv(Table).

%------------------------------------------------------------------------------------

					%--- getVariable ---%

getVariable(Name,Variable):-
	getTV(TVs),
	getVariableListaDeListas(TVs,Name,Variable).

getVariableListaDeListas([TV|_],Name,Variable):-
	isThere(TV,Name,Variable),!.

getVariableListaDeListas([_|TVs],Name,Variable):-
	getVariableListaDeListas(TVs,Name,Variable).

isThere([],_,_):- false.
isThere([(Type,Name,Value)|_],Name,(Type,Name,Value)):- !, true.
isThere([_|Rest],Name,ValueReturned):-
	isThere(Rest,Name,ValueReturned).

%------------------------------------------------------------------------------------

					%--- getValue ---%

getValue(Name,Value):-
	getTV(TVs),
	getValueListaDeListas(TVs,Name,Value).

getValueListaDeListas([TV|_],Name,Value):-
	getValueLista(TV,Name,Value), !.

getValueListaDeListas([_|TVs],Name,Value):-
	getValueListaDeListas(TVs,Name,Value).

getValueLista([],_,_):- !, false.
getValueLista([(_,Name,Value)|_], Name, Value):- !,true.
getValueLista([_|Rest], Name, Value):-
	getValueLista(Rest, Name, Value).

%------------------------------------------------------------------------------------

					%--- update ---%

update(Var):-
	getTV(TVs),
	updateListaDeListas(TVs,Var,[],TVupdated),
	updateTV(TVupdated).


updateListaDeListas([],_,TVsAc,TVsAc).
updateListaDeListas([TV|TVs],Var,TVsAc,Result):-
	updateLista(TV,Var,[],TVupdated),!,
	append(TVsAc,[TVupdated],ResultAux),
	append(ResultAux,TVs,Result).

updateListaDeListas([TV|TVs],Var,TVsAc,Result):-
	append(TVsAc,[TV],ResultAux),
	updateListaDeListas(TVs,Var,ResultAux,Result).

updateLista([],_,TVac,TVac):- false.

updateLista([(Type,Name,_)|TV],(Name,Value),TVac,TVresult):-
	!,
	append(TVac,[(Type,Name,Value)],TVupdatedAux),
	append(TVupdatedAux,TV,TVresult),
	true.

updateLista([(Type,Name1,Value)|TV],(Name2,V),TVac, TVupdated):-
	append(TVac,[(Type,Name1,Value)],TVupdatedAux),
	updateLista(TV,(Name2,V),TVupdatedAux,TVupdated).

%------------------------------------------------------------------------------------

					%--- notInTable ---%

notInTable(Variable):-
	getTV(TVs),
	notInTableListaDeListas(TVs,Variable).

notInTableListaDeListas([TV|TVs],Variable):-
	notInTableLista(TV,Variable),!,
	notInTableListaDeListas(TVs,Variable).

notInTableListaDeListas(_,_):- true.

notInTableLista([],_):-true.
notInTableLista([(_,Name,_)|_],Name) :- !, false.
notInTableLista([_|Rest],Name1) :-!,
	notInTableLista(Rest,Name1).

%------------------------------------------------------------------------------------

					%--- printPant ---%

printTable:-
	getTV(TV),
	write(TV).
%------------------------------------------------------------------------------------

labelList(Sol):-
	getTV([[_|TV]]),
	labelAux(TV,[],Sol).

labelAux([],Ac,Ac).
labelAux([(_,_,Value)|TV],Ac,Sol):-
	labelAux(TV,[Value|Ac],Sol).