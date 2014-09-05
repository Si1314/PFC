Intérprete simbólico en Prolog de Árboles de Sintaxis Abstracta obenidos a partir de código en C++
===

Este proyecto consiste en un intérprete simbólico que a partir de la representación en XML de un código en C++ realiza una ejecución simbólica de éste.

##Funcionamiento

El interprete recorre la representación del código de una función asignando valores simbólicos a una tabla de variables. Cuando ya ha recorrido el código razona sobre dichos valores y devuelve un conjunto de posibles entradas, posibles salidas, interacciones por consola y la traza de dicha función. Dichos valores resultantes se recogen en un fichero XML.

La ejecución se controla mediante las variables **Inf**, **Sup** y **MaxDepth**. 
Inf y Sup acotan inferior y superiormente el rango que pueden adoptar los valores enteros de las variables.
MaxDepth restringe el número de iteraciones que puede ejecutarse un bucle en la simulación. 

##Uso

Para ejecutar el interprete se invoca el predicado interpreter. Puede haber dos modalidades:

		interpreter(EntryFile, OutFile, Inf, Sup, MaxDepth, FunctionName).
Donde se puede simular con valores específicos en las variables Inf, Sup y MaxDepth.

		interpreter(EntryFile, OutFile, FunctionName).

Con valores predeterminados **Inf=-5**, **Sup=5** y **MaxDepth=10**.

---

##La representación en XML del código

Este proyecto va de la mano del proyecto AST2XML.

		https://github.com/si1314/AST2XML

El proyecto AST2XML se encarga de traducir el código fuente en una estructura basada en XML equivalente. De esta forma el código:

		int dameMayor(int a, int b){
			if(a > b){
				return a;
			}else{
				return b;
			}
		}

se convierte en:

		<function name="dameMayor" type="int" line="3">
		    <params>
		        <param type="int" name="a"/>
		        <param type="int" name="b"/>
		    </params>
		    <body>
		        <if line="4">
		            <binaryOperator type="comparison" operator="&gt;">
		                <variable name="a"/>
		                <variable name="b"/>
		            </binaryOperator>
		            <then>
		                <body>
		                    <return line="5">
		                        <variable name="a"/>
		                    </return>
		                </body>
		            </then>
		            <else>
		                <body>
		                    <return line="7">
		                        <variable name="b"/>
		                    </return>
		                </body>
		            </else>
		        </if>
		    </body>
		</function>

##Los resultados devueltos

		<caso>
		  <variable name="ret" value="-4"/>
		  <variable name="a" value="-4"/>
		  <variable name="b" value="-5"/>
		  <data>
		    <traza> 3 4 5</traza>
		    <cin/>
		    <cout/>
		  </data>
		</caso>
		<caso>
		  <variable name="ret" value="-5"/>
		  <variable name="a" value="-5"/>
		  <variable name="b" value="-5"/>
		  <data>
		    <traza> 3 4 7</traza>
		    <cin/>
		    <cout/>
		  </data>
		</caso>

---

##Librerías usadas

###CLPFD

###SGML & SGML_WRITE

###Executers & Expressions

Librerías de creación propia para gestionar la ejecución simbólica de las instrucciones y el cálculo de las expresiones.

Las instrucciones se simulan una a una consumiendo la lista de instrucciones que se obtiene a partir del fichero de entrada mediante el predicado **execute**. Este predicado es recursivo, de forma que cada instrucción en algun momento tiene la forma:

		execute(EntryS,[Instruccion|RestoDeInstrucciones],OutS):-
			
			...
			
			execute(EntryS1,RestoDeInstrucciones,OutS).

Las expresiones se calculan de forma similar:

		resolveExpression(EntryS,('NombreDeLaOperacion',Operador,[ExpresionA,ExpresionB]),Resultado,OutS):-

			...

			work(Operador,OperandoA,OperandoB,Result).

Siendo en nuestra nomenclatura EntryS y OutS los estados previo y posterior a la simulación de la instrucción o expresión.

####El estado

En cada ejecución del interprete simulamos la fluctuación de los valores de un conjunto de variables. 
Estas variables las manejamos mediante la tupla **state**.

		state = (Tabla,Consola,Trace)

Esta tupla contiene una tabla variable-valor, una lista de interacciones de consola simuladas y una lista de las líneas ejecutadas del código.
