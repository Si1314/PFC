Intérprete simbólico en Prolog de Árboles de Sintaxis Abstracta obenidos a partir de código en C++
===

Este proyecto consiste en un intérprete simbólico que a partir de la representación en XML de un código en C++ realiza una ejecución simbólica de éste.

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

