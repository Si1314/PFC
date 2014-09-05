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

