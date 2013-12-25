%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		INTERPRETE		  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

interprete :- 
	cd('../PFC'),
	use_module(library(sgml)),
	load_xml_file('plantilla.xml', Xs),
	%write(Xs),
	ejecuta(Xs,[],TVact),
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
execute(element(Nombre,Atributos,Resto),TV,TVactTotal) :-
	!, 
	%write('\n'),
	%write('Nombre :'), write(Nombre), write('\n'),
	%write('Atributos :'), write(Atributos), write('\n'),
	%write('Resto :'), write(Resto), write('\n'),
	
	evalua(Nombre,Atributos,TV,TVact,Resto,NuevoResto),
	ejecuta(NuevoResto,TVact,TVactTotal).
	
execute(_,TV,TV).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		EVALUA			  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

evalua('funcion',[_=NombreFuncion,_=ValorSalida],TV,TVact,Cuerpo,Cuerpo) :- !,funcionOMetodo(ValorSalida,FunOmet) ,append(TV,[(NombreFuncion,ValorSalida,FunOmet)],TVact).

evalua('param',[_=TipoParametro,_=NombreParametro],TV,TVact,Cuerpo,Cuerpo) :- !, append(TV,[(TipoParametro,NombreParametro,'')], TVact).

evalua('body',_,TV,TV,Cuerpo,Cuerpo) :- ! .

evalua('asignacion',[_=Nombre,_=Valor],TV,TVact,Cuerpo,Cuerpo) :- !, actualizaVariable(TV,(Nombre,Valor), [], TVact).

evalua('declaracionAsignacion',[_=Tipo,_=Nombre,_=Valor],TV,TVact,Cuerpo,Cuerpo):- !, meteVariable(TV,(Tipo,Nombre,Valor), TVact).

evalua('if',_,TV,TVact,Cuerpo,Cuerpo):- !, sentenciaIF(Cuerpo,TV,TVact).

evalua('operadorBinario',_,TV,TV,Cuerpo,Cuerpo):-!.%, write('\npasamos por OperadorBinario\n').

evalua('operando',_,TV,TV,Cuerpo,Cuerpo):-!.%, write('\npasamos por Operando\n').

evalua('then',_,TV,TV,Cuerpo,Cuerpo):-!.%, write('\npasamos por then\n').

evalua('else',_,TV,TV,Cuerpo,Cuerpo):-!.%, write('\npasamos por else\n').

evalua(_,_,TV,TV,Cuerpo,Cuerpo).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	FUNCIONES AUXILIARES  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%--- actualizaVariable ---%

actualizaVariable([],_,TVaux,TVaux) .

actualizaVariable([(Tipo,Nombre,_)|TV],(Nombre,Valor),TVaux, TVresul):-
	!,
	append(TVaux,[(Tipo,Nombre,Valor)],TVactAux),
	append(TVactAux,TV,TVresul).

actualizaVariable([(Tipo,Nombre1,Valor)|TV],(Nombre2,V),TVaux, TVact):-
	append(TVaux,[(Tipo,Nombre1,Valor)],TVactAux),
	actualizaVariable(TV, (Nombre2,V), TVactAux, TVact).

%--- meteVariable ---%

meteVariable(TV,(Tipo,Nombre,Valor), TVact):-
	noEstaVariable(TV,Nombre),
	append(TV,[(Tipo,Nombre,Valor)],TVact).

%--- funcionOMetodo ---%

funcionOMetodo('void','metodo'):-!.
funcionOMetodo(_,'funcion').

%--- noEstaVariable ---%

noEstaVariable([(_,Nombre,_)|_],Nombre) :- !, false.
noEstaVariable([_|Resto],Nombre1) :-
	!,
	%Nombre =\= Nombre1,
	noEstaVariable(Resto,Nombre1).
noEstaVariable(_,_):-true.

%--- sentenciaIF ---%

% THEN
sentenciaIF([_,Condicion,_,Then,_,_,_],TV,TV):-
	condicion(Condicion,TV), !,
	write('\nThen:'),write(Then),write('\n').

% ELSE
sentenciaIF([_,_,_,_,_,Else,_],TV,TV):-
	write('\nElse:'),write(Else),write('\n').

%--- condicion ---%

condicion(element(_,[_,_= (Op)],[_,element(_,[_=Operando1],_),_,element(_,[_=Operando2],_),_]),TV):-
	dameVariable(TV, Operando1, (_,Nombre1,Valor1)),
	dameVariable(TV, Operando2, (_,Nombre2,Valor2)),
	write('\n'), write(Op), write('\n'),
	write('\n'), write(Nombre1), write('\n'),
	write('\n'), write(Nombre2), write('\n'),
	atom_number(Valor1,V1),
	atom_number(Valor2,V2),
	resuelve(Op, V1, V2).

%--- resuelve ---%

resuelve('=<', Op1,Op2):- Op1 =< Op2, !.
resuelve('=<', _,_):- !, false.

resuelve('<', Op1,Op2):- Op1 < Op2, !.
resuelve('<', _,_):- !, false.

resuelve('>=', Op1,Op2):- Op1 >= Op2, !.
resuelve('>=', _,_):- !, false.

resuelve('>', Op1,Op2):- Op1 > Op2, !.
resuelve('>', _,_):- !, false.

resuelve('=', Op1,Op2):- Op1 = Op2, !.
resuelve('=', _,_):- !, false.

resuelve('\=', Op1,Op2):- Op1 \= Op2, !.
resuelve('\=', _,_):- !, false.

%--- dameVariable ---%

dameVariable([(Tipo,Nombre,Valor)|_],Nombre,(Tipo,Nombre,Valor)):- !.

dameVariable([_|Resto],Nombre,ValorDevuelto):-
	dameVariable(Resto,Nombre,ValorDevuelto).









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