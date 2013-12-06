%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		INTERPRETE		  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

interprete :- 
	cd('/PFC'),
	use_module(library(sgml)),
	load_xml_file('plantilla.xml', Xs),
	ejecuta(Xs,[],TVact),
	write('Tabla de Valores:\n'),
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
	
	evalua(Nombre,Atributos,TV,TVact),
	ejecuta(Resto,TVact,TVactTotal).
	
execute(_,TV,TV).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		EVALUA			  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

evalua('funcion',[NombreFuncion,ValorSalida],TV,TVact) :- !, append(TV,[(NombreFuncion,ValorSalida,'')],TVact).

evalua('param',[NombreParametro,TipoParametro],TV,TVact) :- !, append(TV,[(NombreParametro,TipoParametro,'')], TVact).

evalua('body',_,TV,TV) :- ! .

evalua('asignacion',[Nombre,Valor],TV,TVact) :- !, actualizaVariable(TV,(Nombre,Valor), [], TVact).

evalua(_,_,TV,TV).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	FUNCIONES AUXILIARES  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

actualizaVariable([],_,TVaux,TVaux) .

actualizaVariable([(Nombre,Tipo,_)|TV],(Nombre,Valor),TVaux, TVresul):-
	!,
	append(TVaux,[(Nombre,Tipo,Valor)],TVactAux),
	append(TVactAux,TV,TVresul).

actualizaVariable([(Nombre1,Tipo,Valor)|TV],(Nombre2,V),TVaux, TVact):-
	append(TVaux,[(Nombre1,Tipo,Valor)],TVactAux),
	actualizaVariable(TV, (Nombre2,V), TVactAux, TVact).


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