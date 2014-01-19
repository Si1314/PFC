					%%%%%%%%%%%%%%%%%%%%%%%%%
					%	 VARIABLES TABLE	%
					%%%%%%%%%%%%%%%%%%%%%%%%%

use_module(library(sgml)).

% createTable, add, getTV, getVariable, getValue, update, notInTable

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%--- create and remove Table ---%

updateTable(TV):-
	retractall(tv(_)),
	assert(tv(TV)).

removeTable:- retractall(tv(_)).

%------------------------------------------------------------------------------------

					%--- add ---%

add((Type,Name,Value),TVupdated):-
	tv(TV),
	notInTable(TV,Name),!,
	append(TV,[(Type,Name,Value)],TVupdated),
	updateTable(TVupdated).

/*
add(TV,(Type,Name,Value),TVupdated):-
        notInTable(TV,Name),
        append(TV,[(Type,Name,Value)],TVupdated).
*/

%------------------------------------------------------------------------------------

					%--- getTV ---%

getTV(Table):- tv(Table).

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

%------------------------------------------------------------------------------------

					%--- update ---%

update(Var,TVupdated):-
	tv(TV),
	updateAux(TV,Var,[],TVupdated).

updateAux([],_,TVaux,TVaux).

updateAux([(Type,Name,_)|TV],(Name,Value),TVaux, TVresult):-
	!,
	append(TVaux,[(Type,Name,Value)],TVupdatedAux),
	append(TVupdatedAux,TV,TVresult),
	updateTable(TVresult).

updateAux([(Type,Name1,Value)|TV],(Name2,V),TVaux, TVupdated):-
	append(TVaux,[(Type,Name1,Value)],TVupdatedAux),
	updateAux(TV, (Name2,V), TVupdatedAux, TVupdated).

/*
update(TV,Var,TVupdated):- updateAux(TV,Var,[],TVupdated).

updateAux([],_,TVaux,TVaux) .

updateAux([(Type,Name,_)|TV],(Name,Value),TVaux, TVresult):-
	!,
	append(TVaux,[(Type,Name,Value)],TVupdatedAux),
	append(TVupdatedAux,TV,TVresult).

updateAux([(Type,Name1,Value)|TV],(Name2,V),TVaux, TVupdated):-
	append(TVaux,[(Type,Name1,Value)],TVupdatedAux),
	updateAux(TV, (Name2,V), TVupdatedAux, TVupdated).
*/
%------------------------------------------------------------------------------------

					%--- notInTable ---%

notInTable([(_,Name,_)|_],Name) :- !, false.
notInTable([_|Rest],Name1) :-!,
	notInTable(Rest,Name1).
notInTable(_,_):-true.

%------------------------------------------------------------------------------------