					%%%%%%%%%%%%%%%%%%%%%%%%%
					%    VARIABLES TABLE    %
					%%%%%%%%%%%%%%%%%%%%%%%%%

use_module(library(sgml)).

% createTable, add, getTV, getVariable, getValue, update, updateTV, notInTable

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%--- create and remove Table ---%

%------------------------------------------------------------------------------------

					%--- add ---%
add([TV|TVs],(Type,Name,Value),[TVupdated|TVs]):-
 	notInTable([TV|TVs],Name),!,
 	append(TV,[(Type,Name,Value)],TVupdated).

%------------------------------------------------------------------------------------

					%--- apila / desapila ---%

apila(TV,[[]|TV]).

desapila([_|TV],TV).

%------------------------------------------------------------------------------------

					%--- getVariable ---%

getVariable(TVs,Name,Variable):-
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

getValue(TVs,Name,Value):-
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

update(TVs,Var,TVupdated):-
	updateListaDeListas(TVs,Var,[],TVupdated).

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

notInTable(TVs,Variable):-
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
labelList([[_|TV]],Sol):-
 	labelAux(TV,[],Sol).
 
labelAux([],Ac,Ac).
labelAux([(_,_,Value)|TV],Ac,Sol):-
 	labelAux(TV,[Value|Ac],Sol).