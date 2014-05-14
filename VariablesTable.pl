					%%%%%%%%%%%%%%%%%%%%%%%%%
					%    VARIABLES TABLE    %
					%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% La tabla de variables es de la forma lista de listas, de forma que la tabla está
% compuesta por tablas de variables cada nuevo bloque "while", "for", "if", "body",
% "function", ... crea una tabla nueva para su ámbito.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%%%%%%%%%%%%%%%%%%%%
					% apila / desapila %
					%%%%%%%%%%%%%%%%%%%%

% Apila / Desapila una tabla de variables a la tabla de variables

apila(TV,[[]|TV]).
desapila([_|TV],TV).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
					
					%%%%%%%%%%%
					%   add   %
					%%%%%%%%%%%

% Añade una variable a la tabla de variables

add([TV|TVs],(Type,Name,Value),[TVupdated|TVs]):-
 	notInTable([TV|TVs],Name),!,
 	append(TV,[(Type,Name,Value)],TVupdated).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%%%%%%%%%%%%%%%%%%%%
					%	 getVariable   %
					%%%%%%%%%%%%%%%%%%%%

% Devuelve la variable entera pasandole solo el nombre de ésta

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%%%%%%%%%%%%%%%%%
					%	 getValue   %
					%%%%%%%%%%%%%%%%%

% Devuelve el valor de una variable

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%%%%%%%%%%%%%%%%%
					%     update    %
					%%%%%%%%%%%%%%%%%

% Actualiza la tabla de variables con la variable "Var" pasada por parámetro

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

updateLista([],_,TVac,TVac):-false.

updateLista([(Type,Name,_)|TV],(Name,Value),TVac,TVresult):-
	!,
	append(TVac,[(Type,Name,Value)],TVupdatedAux),
	append(TVupdatedAux,TV,TVresult),
	true.

updateLista([(Type,Name1,Value)|TV],(Name2,V),TVac, TVupdated):-
	append(TVac,[(Type,Name1,Value)],TVupdatedAux),
	updateLista(TV,(Name2,V),TVupdatedAux,TVupdated).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%%%%%%%%%%%%%%%%%
					%  updateNames  %
					%%%%%%%%%%%%%%%%%

% Actualiza los nombres de las variables de [X|Xs] por los pasados en Ys

updateNames([X|Xs],Ys,Out):-
	updateNamesAux(X,Ys,[],Out1),
	append([Out1],Xs,Out).

updateNamesAux([],_,Ac,Ac):-!.
updateNamesAux(_,[],Ac,Ac):-!.
updateNamesAux([(Type,_,Value)|Xs],[Name2|Ys],Ac,Out):-
	append(Ac,[(Type,Name2,Value)],Ac1),
	updateNamesAux(Xs,Ys,Ac1,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%%%%%%%%%%%%%%%%%
					%   notInTable  %
					%%%%%%%%%%%%%%%%%

% Devuelve 'true' si la variable no está en la tabla de variables, 'false' en caso contrario

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%%%%%%%%%%%%%%%%%
					%    labelList  %
					%%%%%%%%%%%%%%%%%

% Coloca en SolNames los nombres de las variables que vamos a devolver y en SolValues los valores

labelList([TV],SolNames,SolValues):- !,
 	labelAux(TV,[],[],SolNames,SolValues).
 
labelAux([],AcNames,AcValues,AcNames,AcValues):-!.
labelAux([(_,Name,Value)|TV],AcNames,AcValues,SolNames,SolValues):-
	append(AcNames,[Name],AcNames1),
	append(AcValues,[Value],AcValues1),
 	labelAux(TV,AcNames1,AcValues1,SolNames,SolValues).
