%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		INTERPRETE		  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

interprete :- 
	cd('/PFC'),
	use_module(library(sgml)),
	load_xml_file('ejemplo.xml', Xs),
	interpreta(Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		INTERPRETA		  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

interpreta([]).
interpreta([element(Nombre,_,[Contido|Resto])|Xs]) :-
	!,
	evalua(Nombre,Contido,Resto),
	interpreta(Xs).

interpreta([_|Xs]):-
	interpreta(Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		FUNCION			  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

evalua('funcion', Contenido, []) :-
	!,
	write('Estamos en la parte de funcion\n'),
	write('Tenemos como contenido { '), write(Contenido), write(' }\n').
	
evalua('funcion', Contenido, Resto) :-
	!,
	write('Estamos en la parte de funcion\n'),
	write('Tenemos como contenido { '), write(Contenido), write(' }\n'),
	transforma(Contenido,Array),
	incluyeHechos(Array),
	interpreta(Resto).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		BODY			  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

evalua('body', Contenido, []):-
	!,
	write('Estamos en la parte de body\n'),
	write('Tenemos como contenido { '), write(Contenido), write(' }\n').
	
evalua('body', Contenido, Resto):-
	!,
	write('Estamos en la parte de body\n'),
	write('Tenemos como contenido { '), write(Contenido), write(' }\n'),
	interpreta(Resto).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		DECLARACION		  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

evalua('declaracion', Contenido, []) :-
	!,
	write('Estamos en la parte de declaracion\n'),
	write('Tenemos como contenido { '), write(Contenido), write(' }\n').

evalua('declaracion', Contenido, Resto) :-
	!,
	write('Estamos en la parte de declaracion\n'),
	write('Tenemos como contenido { '), write(Contenido), write(' }\n'),
	interpreta(Resto).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		VARIABLE    	  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

evalua('variable', Contenido, []) :-
	!,
	write('Estamos en la parte de variable con lista vacia\n'),
	write('Tenemos como contenido { '), write(Contenido), write(' }\n').
	
evalua('variable', Contenido, Resto) :-
	!,
	write('Estamos en la parte de variable\n'),
	write('Tenemos como contenido { '), write(Contenido), write(' }\n'),
	interpreta(Resto).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		VALOR			  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

evalua('valor', Contenido, []) :-
	!,
	write('Estamos en la parte de valor\n'),
	write('Tenemos como contenido { '), write(Contenido), write(' }\n').
	
evalua('valor', Contenido, Resto) :-
	!,
	write('Estamos en la parte de valor\n'),
	write('Tenemos como contenido { '), write(Contenido), write(' }\n'),
	interpreta(Resto).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		RETURN			  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

evalua('return', Contenido, []) :-
	!,
	write('Estamos en la parte de return\n'),
	write('Tenemos como contenido { '), write(Contenido), write(' }\n').
	
evalua('return', Contenido, Resto) :-
	!,
	write('Estamos en la parte de return\n'),
	write('Tenemos como contenido { '), write(Contenido), write(' }\n'),
	interpreta(Resto).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Caso Base evalua	  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

evalua(_,_,[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		TRANSFORMA		  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Transformamos el String en un array: ej: nombre:"main" tipo:"int (void)" --> ['nombre:', main, 'tipo:', int (void)]
	
transforma() :-


/*

----------------------------------------------------------------------------

<funcion> nombre:"main" tipo:"int (void)"
   <body>
      <declaracion>
          <variable> nombre:"a" tipo:"int"</variable>
          <valor> tipo:"int" cardinal:0</valor>
      </declaracion>
      <return> tipo:"int"
          <variable> nombre:"a" tipo:"int"</variable>
      </return>
   </body>
</funcion>

----------------------------------------------------------------------------

[element(funcion,[],[ nombre:"main" tipo:"int (void)"
	,element(body,[],[
   		,element(declaracion,[],[
        	,element(variable,[],[ nombre:"a" tipo:"int"]),
          	,element(valor,[],[ tipo:"int" cardinal:0]),
      	]),
      	,element(return,[],[ tipo:"int"
        	,element(variable,[],[ nombre:"a" tipo:"int"]),
      	]),
   	]),
])]

*/