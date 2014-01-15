%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		INTERPRETE		  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Escribir "interprete." para probarlo

interprete :- 
	cd('../PFC'),
	use_module(library(sgml)),
	load_xml_file('plantillaWhile.xml', Xs),
	%write('\n'),write(Xs), write('\n'),
	eliminaVacios(Xs,Xs1),
	%write(Xs1), write('\n'),
	ejecuta(Xs1,[],TVact),
	write('\nTabla de Variables final:\n'),
	write(TVact).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		EJECUTA			  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Vamos a ejecutar con "execute" cada elemento que tengamos en la lista de entrada. TV es la Tabla de variables y TVact la Tabla actualizada despues de la ejecucion

ejecuta([],TV,TV) :- !.
ejecuta([X|Xs],TV,TVactT) :-
	!,
	execute(X,TV,TVact),
	ejecuta(Xs,TVact,TVactT).

execute((Nombre,Atributos,Resto),TV,TVactTotal) :-
	!, 
	evalua(Nombre,Atributos,TV,TVact,Resto,NuevoResto),
	ejecuta(NuevoResto,TVact,TVactTotal).
	
execute(_,TV,TV).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		EVALUA			  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% evaluamos cada clausula individualmente:

evalua('funcion',[_=NombreFuncion,_=ValorSalida],TV,TVact,Cuerpo,Cuerpo) :- !,funcionOMetodo(ValorSalida,FunOmet) ,append(TV,[(NombreFuncion,ValorSalida,FunOmet)],TVact).

evalua('param',[_=TipoParametro,_=NombreParametro],TV,TVact,Cuerpo,Cuerpo) :- !, append(TV,[(TipoParametro,NombreParametro,'')], TVact).

evalua('body',_,TV,TV,Cuerpo,Cuerpo) :- !.

evalua('declaracion',[_=Tipo,_=Nombre],TV,TVact,Cuerpo,Cuerpo):- !, meteVariable(TV,(Tipo,Nombre,''),TVact).

evalua('asignacion',[_=Nombre],TV,TVact,[Cuerpo],[]) :- !, resuelve(Cuerpo,TV,Valor), actualizaVariable(TV,(Nombre,Valor),TVact).

evalua('if',_,TV,TVact,Cuerpo,[]):- !, sentenciaIF(Cuerpo,TV,TVact), write('\nTV Antes del IF\n'), write(TV), write('\n\nTV Despues del IF\n'), write(TVact), write('\n').

evalua('while',_,TV,TVact,Cuerpo,[]):- !, sentenciaWhile(Cuerpo,TV,TVact).

evalua(_,_,TV,TV,Cuerpo,Cuerpo).


					%%%%%%%%%%%%%%%%%%%%%%%%%%%
					%	FUNCIONES AUXILIARES  %
					%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%--- sentenciaIF ---%

% CONDICION
condicion((_, [_, _= (Op)], [(_,[_=Operando1],_),(_,[_=Operando2],_)]), TV):-
	sacaValor(TV,Operando1,Valor1),
	sacaValor(TV,Operando2,Valor2),
	opera(Op, Valor1, Valor2, Resultado), Resultado.

% THEN
sentenciaIF([Condicion,('then',_,CuerpoThen),_],TV,TVact):-
	write('\nCondicion:\n'),write(Condicion),write('\n'),
	condicion(Condicion,TV), !,
	ejecuta(CuerpoThen,TV,TVact),
	write('\nThen:\n'),write(CuerpoThen),write('\n').

% ELSE
sentenciaIF([_,_,('else',_,CuerpoElse)],TV,TVact):-
	write('\nElse:\n'),write(CuerpoElse),write('\n'),
	ejecuta(CuerpoElse,TV,TVact).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%--- sentenciaWhile ---%

sentenciaWhile([Condicion,('cuerpo',_,CuerpoWhile)],TV,TVact2):-
	condicion(Condicion,TV), !,
	ejecuta(CuerpoWhile,TV,TVact1),
	sentenciaWhile([Condicion,('cuerpo',_,CuerpoWhile)],TVact1,TVact2).

sentenciaWhile(_,TV,TV).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%--- actualizaVariable ---%

% Dada una variable Var, actualizamos el valor de la variable.

actualizaVariable(TV,Var,TVact):- actualizaVariableAux(TV,Var,[],TVact).

actualizaVariableAux([],_,TVaux,TVaux) .

actualizaVariableAux([(Tipo,Nombre,_)|TV],(Nombre,Valor),TVaux, TVresul):-
	!,
	append(TVaux,[(Tipo,Nombre,Valor)],TVactAux),
	append(TVactAux,TV,TVresul).

actualizaVariableAux([(Tipo,Nombre1,Valor)|TV],(Nombre2,V),TVaux, TVact):-
	append(TVaux,[(Tipo,Nombre1,Valor)],TVactAux),
	actualizaVariableAux(TV, (Nombre2,V), TVactAux, TVact).

%------------------------------------------------------------------------------------

					%--- meteVariable ---%

% Dada una variable Var, metemos la variable en la tabla de variables TV.

meteVariable(TV,(Tipo,Nombre,Valor), TVact):-
	noEstaVariable(TV,Nombre),
	append(TV,[(Tipo,Nombre,Valor)],TVact).

%------------------------------------------------------------------------------------

					%--- funcionOMetodo ---%

% Diferenciamos una funcion de un metodo.

funcionOMetodo('void','metodo'):-!.
funcionOMetodo(_,'funcion').

%------------------------------------------------------------------------------------

					%--- noEstaVariable ---%

% True si la variable no está en la tabla de variables

noEstaVariable([(_,Nombre,_)|_],Nombre) :- !, false.
noEstaVariable([_|Resto],Nombre1) :-
	!,
	%Nombre =\= Nombre1,
	noEstaVariable(Resto,Nombre1).
noEstaVariable(_,_):-true.

%------------------------------------------------------------------------------------

					%--- resuelve expresion binaria---%

% Resolvemos la expresion binaria del tipo X = Y, dada de la forma resuelve(=,X,Y,TV,TVact)

% Caso en el que el operando es otra expresion binaria: X = "Y + Z"

resuelve(('operadorBinario',Operador,[X,Y]), TV , Resultado):- !,
	sacaContenido(Operador,Op),
	resuelve(X, TV, Operando1),
	resuelve(Y, TV, Operando2),
	opera(Op, Operando1, Operando2, Resultado).

% Caso en el que el operando es una variable: X = "y"

resuelve(('operando',[_=NombreOperando],_), TV, ValorOperando):- !,
	sacaValor(TV, NombreOperando,ValorOperando).
	%atom_number(ValorOperando,Resultado).

% Caso en el que el operando es un número entero: X = "1"

resuelve(('numero',[_=Valor],_),_ ,Resultado):- !,
	atom_number(Valor,Resultado).

% Resto de casos:

resuelve(_,_,0).

% sacamos el contenido que viene de la forma [operando, Nombre= ("y")] , [integer, Valor= ("1")] , etc.

sacaContenido([_,_= (Op)], Op).
sacaContenido((_,[_=Nombre],_), Nombre).


% ---> Booleana <---		Resolvemos expresiones booleanas

opera('=<', Op1,Op2, true):- Op1 =< Op2, !.
opera('=<', _,_,false):- !.

opera('<', Op1,Op2,true):- Op1 < Op2, !.
opera('<', _,_,false):- !.

opera('>=', Op1,Op2,true):- Op1 >= Op2, !.
opera('>=', _,_,false):- !.

opera('>', Op1,Op2,true):- Op1 > Op2, !.
opera('>', _,_,false):- !.

opera('==', Op1,Op2,true):- Op1 = Op2, !.
opera('==', _,_,false):- !.

opera('!=', Op1,Op2,true):- Op1 \= Op2, !.
opera('!=', _,_,false):- !.


% ---> Aritmetica <---		Resolvemos expresiones aritmeticas (FALTAN MUCHAS MAS) "TODO"

opera('+', Op1,Op2,Z):- !, Z is Op1 + Op2.
opera('-', Op1,Op2,Z):- !, Z is Op1 - Op2.
opera('*', Op1,Op2,Z):- !, Z is Op1 * Op2.



%------------------------------------------------------------------------------------

					%--- dameVariable ---%

% Devuelve la variable con nombre "Nombre" de la tabla de valores

dameVariable([(Tipo,Nombre,Valor)|_],Nombre,(Tipo,Nombre,Valor)):- !.

dameVariable([_|Resto],Nombre,ValorDevuelto):-
	dameVariable(Resto,Nombre,ValorDevuelto).

%------------------------------------------------------------------------------------

					%--- eliminaVacios ---%

% Elimina los elementos vacios que haya en la lista Xs y deja en Ys la lista limpia

eliminaVacios(Xs,Ys):-
	eliminaVaciosAux(Xs,[],Ys).

eliminaVaciosAux([],Ac,Ac).

eliminaVaciosAux([element(X,Y,Z)|Xs],Ac,Ys):- !,
	eliminaVacios(Z,Z1),
	append(Ac,[(X,Y,Z1)],Acc),
	eliminaVaciosAux(Xs,Acc,Ys).

eliminaVaciosAux([_|Xs],Ac,Ys):-
	eliminaVaciosAux(Xs,Ac,Ys).

%------------------------------------------------------------------------------------

					%--- sacaValor ---%

% Saca el valor de la variable "NombreOperando" y devuelve en "Resultado" su valor

sacaValor([(_,NombreOperando,Resultado)|_], NombreOperando, Resultado):- !.
sacaValor([_|Xs], NombreOperando, Resultado):-
	sacaValor(Xs, NombreOperando, Resultado).

%------------------------------------------------------------------------------------