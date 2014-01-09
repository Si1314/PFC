%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		INTERPRETE		  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

interprete :- 
	cd('../PFC'),
	use_module(library(sgml)),
	load_xml_file('plantilla.xml', Xs),
	%write('\n'),write(Xs), write('\n'),
	eliminaVacios(Xs,Xs1),
	%write(Xs1), write('\n'),
	ejecuta(Xs1,[],TVact),
	write('\nTabla de Valores:\n'),
	write(TVact).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		EJECUTA			  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%
ejecuta([],TV,TV) :- !.
ejecuta([X|Xs],TV,TVactT) :-	% TV = Tabla de Variables, TVact = Tabla de Variables actualizada
	!,
	execute(X,TV,TVact),
	ejecuta(Xs,TVact,TVactT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		EXECUTE			  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%
execute((Nombre,Atributos,Resto),TV,TVactTotal) :-
	!, 
	write('\n'),
	write('Nombre :'), write(Nombre), write('\n'),
	write('Atributos :'), write(Atributos), write('\n'),
	%write('Resto :'), write(Resto), write('\n'),
	
	evalua(Nombre,Atributos,TV,TVact,Resto,NuevoResto),
	%write('-----\n'), write(Nombre), write('   '), write(Atributos), write('\n-----'),
	ejecuta(NuevoResto,TVact,TVactTotal).
	
execute(_,TV,TV).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		EVALUA			  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

evalua('funcion',[_=NombreFuncion,_=ValorSalida],TV,TVact,Cuerpo,Cuerpo) :- !,funcionOMetodo(ValorSalida,FunOmet) ,append(TV,[(NombreFuncion,ValorSalida,FunOmet)],TVact).

evalua('param',[_=TipoParametro,_=NombreParametro],TV,TVact,Cuerpo,Cuerpo) :- !, append(TV,[(TipoParametro,NombreParametro,'')], TVact).

evalua('body',_,TV,TV,Cuerpo,Cuerpo) :- ! .

evalua('asignacion',[_=Nombre,_=Valor],TV,TVact,Cuerpo,Cuerpo) :- !, actualizaVariable(TV,(Nombre,Valor),TVact).

evalua('declaracionAsignacion',[_=Tipo,_=Nombre,_=Valor],TV,TVact,Cuerpo,Cuerpo):- !, meteVariable(TV,(Tipo,Nombre,Valor), TVact).

evalua('if',_,TV,TVact,Cuerpo,[]):- !, sentenciaIF(Cuerpo,TV,TVact).

evalua('operando',_,TV,TV,Cuerpo,Cuerpo):-!.%, write('\npasamos por Operando\n').

evalua('operadorBinario',[_,_= (Op)],TV,TVact,[Op1,Op2],[]):-!, resuelve(Op,Op1,Op2,TV,TVact).

evalua(_,_,TV,TV,Cuerpo,Cuerpo).


					%%%%%%%%%%%%%%%%%%%%%%%%%%%
					%	FUNCIONES AUXILIARES  %
					%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%--- sentenciaIF ---%

% CONDICION
condicion((_, [_, _= (Op)], [(_,[_=Operando1],_),(_,[_=Operando2],_)]), TV):-
	dameVariable(TV, Operando1, (_,_,Valor1)),
	dameVariable(TV, Operando2, (_,_,Valor2)),
	/*write('--------------------'),
	write('\nOp:  '), write(Op), write('\n'),
	write('\nNombre1:  '), write(Nombre1), write('\n'),
	write('\nNombre2:  '), write(Nombre2), write('\n'),
	write('--------------------'),*/
	atom_number(Valor1,V1),
	atom_number(Valor2,V2),
	opera(Op, V1, V2, Resultado), Resultado.

% THEN
sentenciaIF([Condicion,('then',_,CuerpoThen),_],TV,TVact):-
	write('\nCondicion:\n'),write(Condicion),write('\n'),
	condicion(Condicion,TV), !,
	ejecuta(CuerpoThen,TV,TVact).
	%write('\nThen:\n'),write(Then),write('\n').

% ELSE
sentenciaIF([_,_,('else',_,CuerpoElse)],TV,TVact):-
	write('\nElse:\n'),write(CuerpoElse),write('\n'),
	ejecuta(CuerpoElse,TV,TVact).
	%write('\n********************* Resultado FINAL:\n'), write(Resultado).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%--- actualizaVariable ---%
actualizaVariable(TV,Var, TVact):- actualizaVariableAux(TV,Var,[],TVact).

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

meteVariable(TV,(Tipo,Nombre,Valor), TVact):-
	noEstaVariable(TV,Nombre),
	append(TV,[(Tipo,Nombre,Valor)],TVact).

%------------------------------------------------------------------------------------

					%--- funcionOMetodo ---%

funcionOMetodo('void','metodo'):-!.
funcionOMetodo(_,'funcion').

%------------------------------------------------------------------------------------

					%--- noEstaVariable ---%

noEstaVariable([(_,Nombre,_)|_],Nombre) :- !, false.
noEstaVariable([_|Resto],Nombre1) :-
	!,
	%Nombre =\= Nombre1,
	noEstaVariable(Resto,Nombre1).
noEstaVariable(_,_):-true.

%------------------------------------------------------------------------------------

					%--- resuelve expresion binaria---%

resuelve('=',Op1,Op2,TV,TVact):-
	write('\nOp2:   '),write(Op2),
	resuelveAux(Op2,Resultado),
	actualizaVariable(TV,(Op1,Resultado), TVact).

resuelveAux((_,[_=Resultado],[]), Resultado):- !, write('\nResultado Parcial:    '),write(Resultado).
resuelveAux((operadorBinario,Operador,[X,Y]), Resultado):- !,
	sacaOp(Operador,Op),
	write('\nOperadorOp:   '),write(Op),
	write('\nOperandoX:    '),write(X),
	write('\nOperandoY:    '),write(Y),
	resuelveAux(X, Operando1),
	resuelveAux(Y, Operando2),
	write('\nOPERAMOS:'),
	write('\nOperando1:    '),write(Operando1),
	write('\nOperando2:    '),write(Operando2),write('\n'),
	opera(Op, Operando1, Operando2, Resultado).

resuelveAux(_,0):- !, write('\nNADA DE NADA:    ').


sacaOp([_,_= (Op)], Op).

% -> Boleana <-

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


% -> Aritmetica <-

%TODO
%opera('+', Op1,Op2,Z):- sacaValor(Op1,Val1), sacaValor(Op2,Val2), Z is Val1 + Val2, write('Operacion Aritmetica:'), write(Op1), write(' + '), write(Op2), !.
opera('+', Op1,Op2,3):- write('Operacion Aritmetica:'), write(Op1), write(' + '), write(Op2), !.


% -> Igualdad <-
opera('=', Op1,Op2,'Y = 3'):- write('Operacion Igualdad:'), write(Op1), write(' = '), write(Op2), !.



%------------------------------------------------------------------------------------

					%--- dameVariable ---%

dameVariable([(Tipo,Nombre,Valor)|_],Nombre,(Tipo,Nombre,Valor)):- !.

dameVariable([_|Resto],Nombre,ValorDevuelto):-
	dameVariable(Resto,Nombre,ValorDevuelto).

%------------------------------------------------------------------------------------

					%--- eliminaVacios ---%

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

%TODO
%sacaValor(Valor, Resultado)


%------------------------------------------------------------------------------------


/*

----------------------------------------------------------------------------

[element(funcion,[nombre=main,tipo=void],[
        ,element(param,[nombre=x,tipo=int],[]),
        ,element(param,[nombre=y,tipo=int],[]),
        ,element(body,[],[
                ,element(asignacion,[nombre=x,valor=1],[]),
                ,element(asignacion,[nombre=y,valor=2],[]),
                ,element(asignacion,[nombre=x,valor=3],[]),
        ]),
])]

----------------------------------------------------------------------------

<funcion nombre="main" tipo="void">
	<param nombre="x" tipo="int"/>
	<param nombre="y" tipo="int"/>
	<body>
		<asignacion nombre="x" valor="1"/>
		<asignacion nombre="y" valor="2"/>
		<asignacion nombre="x" valor="3"/>
	</body>
</funcion>


*/