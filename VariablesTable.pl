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
	tv(TV),
	append(TV,[(Type,Name,Value)],TVupdated),
	updateTV(TVupdated).

%------------------------------------------------------------------------------------

					%--- getTV ---%

getTV(Table):- tv(Table).

%------------------------------------------------------------------------------------

					%--- getVariable ---%

getVariable(Name,Variable):-
	getTV(TV),
	getVariableAux(TV,Name,Variable). 

getVariableAux([(Type,Name,Value)|_],Name,(Type,Name,Value)):- !.

getVariableAux([_|Rest],Name,ValueReturned):-
	getVariableAux(Rest,Name,ValueReturned).

%------------------------------------------------------------------------------------

					%--- getValue ---%

getValue(OperandName,Value):-
	getTV(TV),
	getValueAux(TV,OperandName,Value).

getValueAux([(_,OperandName,Value)|_], OperandName, Value):- !.
getValueAux([_|Rest], OperandName, Value):-
	getValueAux(Rest, OperandName, Value).

%------------------------------------------------------------------------------------

					%--- update ---%

update(Var):-
	tv(TV),
	updateAux(TV,Var,[],TVupdated),
	updateTV(TVupdated).

updateAux([],_,TVaux,TVaux).

updateAux([(Type,Name,_)|TV],(Name,Value),TVaux, TVresult):-
	!,
	append(TVaux,[(Type,Name,Value)],TVupdatedAux),
	append(TVupdatedAux,TV,TVresult),
	updateTV(TVresult).

updateAux([(Type,Name1,Value)|TV],(Name2,V),TVaux, TVupdated):-
	append(TVaux,[(Type,Name1,Value)],TVupdatedAux),
	updateAux(TV, (Name2,V), TVupdatedAux, TVupdated).

%------------------------------------------------------------------------------------

					%--- notInTable ---%

notInTable(Variable):-
	getTV(TV),
	notInTableAux(TV,Variable).

notInTableAux([(_,Name,_)|_],Name) :- !, false.
notInTableAux([_|Rest],Name1) :-!,
	notInTableAux(Rest,Name1).
notInTableAux(_,_):-true.

%------------------------------------------------------------------------------------

					%--- notInTable ---%

printPant:-
	getTV(TV),
	write(TV).
%------------------------------------------------------------------------------------