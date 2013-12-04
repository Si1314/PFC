PFC
===

El interprete.pl lee el archivo ejemplo.xml
--------------------------------------------

ejemplo.xml
-----------

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

Lo saca para evaluar:
---------------------

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

Lo traduce a:
-------------
1 ?- interprete.
Estamos en la parte de funcion
Tenemos como contenido {  nombre:"main" tipo:"int (void)"
    }
Estamos en la parte de body
Tenemos como contenido { 
       }
Estamos en la parte de declaracion
Tenemos como contenido { 
           }
Estamos en la parte de variable con lista vacia
Tenemos como contenido {  nombre:"a" tipo:"int" }
Estamos en la parte de valor
Tenemos como contenido {  tipo:"int" cardinal:0 }
Estamos en la parte de return
Tenemos como contenido {  tipo:"int"
           }
Estamos en la parte de variable con lista vacia
Tenemos como contenido {  nombre:"a" tipo:"int" }
true.