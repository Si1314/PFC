Cosas para hacer/hechas
===

##Lista de cosas para ir haciendo ya:

 - Hacer una copia interna en el intérprete de los parámetros de entrada para guardarlo y poder sacarlo por pantalla. Ejemplo:

	`
	void main(int x, int y){

	 	/* Copia parámetro */

	 	int w = x;

	 	int z = y;

	 	/* Fin copia */


	 	/* Cuerpo método */

	}
	`

 - El usuario tiene que poder elegir el dominio de los números: `inf y sup`

 - Poner límite en el número de iteraciones por bucle.

 - Leer todas las formas de implementar el for, formas:

 	´
 	**1.** 	for(int i = 0; i < n; i++){...}

 	**2.** 	for(int i = 0, j = 0; i < n, j < m; i++, j++){...}

 	**3.** 	for(int i = 0; int j < m, i < n; i++){...}

 	**4.** 	for(int i = 0; (i < n)&&(i > m); i++){...}

 	**5.** 	for(; i < n; i++){...}

 	**...**´

 - Sustituir `MaxDepth por LoopK´



 - Errores RunTimer como:

  	**1.**	`División por 0`

  	**...**

 - Añadir funciones y llamada a funciones.
  
 - Añadir tipos Booleanos

##Cosas futuras para hacer:

 - Poder sacar tanto resultados como restricciones

 - Ver/Pensar cómo poner un árbol

 - Intérprete: Devuelva la traza del camino explorado, pinchando por ejemplo en un resultado.


##Cosas hechas
 - Ahora se puede poner `++ / --` y `+= / -=`
 - Hacer llamadas a funciones (no metodos)
 - El usuario ya puede elegir el loopK. Añadido al intertpreter(...) estas tres variables de entrada: `inf, sup y loopk`