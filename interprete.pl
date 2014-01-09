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
	%write('Nombre :'), write(Nombre), write('\n'),
	%write('Atributos :'), write(Atributos), write('\n'),
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

evalua('if',_,TV,TVact,Cuerpo,[]):- !,sentenciaIF(Cuerpo,TV,TVact), write('\nAntes del IF\n'), write(TV), write('\n\nDespues del IF\n'), write(TVact), write('\n').

evalua('operando',_,TV,TV,Cuerpo,Cuerpo):-!.

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
	atom_number(Valor1,V1),
	atom_number(Valor2,V2),
	opera(Op, V1, V2, Resultado), Resultado.

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

					%--- actualizaVariable ---%

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
	sacaContenido(Op1,Operando1),
	resuelveAux(Op2,TV,Resultado),
	actualizaVariable(TV,(Operando1,Resultado),TVact).

resuelveAux((_,[_,_=Resultado],[]), _ , Resultado):- !.

resuelveAux(('operadorBinario',Operador,[X,Y]), TV , Resultado):- !,
	sacaContenido(Operador,Op),
	resuelveAux(X, TV, Operando1),
	resuelveAux(Y, TV, Operando2),
	opera(Op, Operando1, Operando2, Resultado).

resuelveAux(('operando',[_=NombreOperando],_), TV, Resultado):- !,
	sacaValor(TV, NombreOperando,ValorOperando),
	atom_number(ValorOperando,Resultado).

resuelveAux(('integer',[_=Valor],_), _ ,Resultado):- !,
	atom_number(Valor,Resultado).

resuelveAux(_,_,0).


sacaContenido([_,_= (Op)], Op).
sacaContenido((_,[_=Nombre],_), Nombre).

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

opera('+', Op1,Op2,Z):- !, Z is Op1 + Op2.



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
sacaValor([(_,NombreOperando,Resultado)|_], NombreOperando, Resultado):- !.
sacaValor([_|Xs], NombreOperando, Resultado):-
	sacaValor(Xs, NombreOperando, Resultado).

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