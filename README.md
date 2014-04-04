PFC
===

##Hay que hacer hoy: (En orden de prioridad)

 - Conseguir que funcione tanto los "while" como los "for".

 - Ponerle un tope de profundidad.

 - Conseguir que escriba bien el fichero de salida, por ahora tenemos:
 	
	 	<table/>
			
			<variable name="a" value="1"/>
			
			<variable name="b" value="1"/>
			
		<table/>

			<variable name="a" value="1"/>

			<variable name="b" value="-255"/>

		<table/>

			<variable name="a" value="-255"/>

			<variable name="b" value="1"/>

		<table/>

			<variable name="a" value="-255"/>

			<variable name="b" value="-255"/>

		<table/>

	y estaría bien conseguir:

		<table>

			<caso>
				
				<variable name="a" value="1"/>
				
				<variable name="b" value="1"/>
				
			<caso/>

			<caso>

				<variable name="a" value="1"/>

				<variable name="b" value="-255"/>

			<caso/>

			<caso>

				<variable name="a" value="-255"/>

				<variable name="b" value="1"/>

			<caso/>

			<caso>

				<variable name="a" value="-255"/>

				<variable name="b" value="-255"/>

			<caso/>

		<table/>

##Cosas que hacer despues de la entrega
 
 - Recibir el nombre del fichero para crear un fichero .xml con ese nombre que acabe en output, por ejemplo: `ficheroPrueba.xml`saldría: `ficheroPruebaOutput.xml`.

 - Conseguir leer el for tambien asi: for(int i=0, j=0;....)