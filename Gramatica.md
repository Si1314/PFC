Gramatica
===

##Reglas gramaticales


ElementosEntrada : ElementoEntrada

ElementoEntrada : whitespace | comentario | token 

comentario : “/” “*” caracter_ASCII* - { “/” } “*” “/” | “/” “/” carácter_ASCII* - { “\n” } 

digito : 0 | 1 | 2 |3 | 4 | 5 | 6 | 7 | 8 | 9 

 letra : a | b | c | d | e | f | g | h | i | j | k | l | m | n | ñ | o | p | q | r | s | t | u | v | w | x | y | z | A | B | C | D | E | F | G | H | I | J | K | L | M | N | Ñ | O | P | Q | R | S | T | U | V | W | X | Y | Z | Á | É | Í | o | Ú 

whitespace : espacio en blanco

ascii  Caracteres del estándar ASCII

caracter_ASCII : ascii | Ñ | ñ 

token : identificador | palabra_reservada | literal | delimitador | operador 

identificador : “_” [ letra | digito | “_” | “$” ]* letra [ letra | digito | “_” | “$” ]* “$” [ letra | digito | “_” | “$” ]* 

palabra_reservada : “class” | “else” | “for” | “if” | “int” | “return” | “void” | “while” 


### LITERALES y OPERADORES

literal : literal_integer

literal_integer : [“+” | “-“] [digito - {0}]* digito* | [“+” | “-“] “0” 

delimitador : ( | ) | { | } | ; | , | . 

operador : operadorInfijo | operadorPrefijo | operadorPostfijo 

operadorInfijo : || | && | == | != | < | > | <= | >= | + | - | * | / 

operadorPrefijo : ! | + | -

operadorPostfijo : ++ | --

operadorAsignacion : = | += | -= | *= | /=

operadorUnario : & | * | + | – | !


### TIPOS DE DATOS

tipo : tipo_primitivo identificador

tipo_primitivo : “int”

listaArgumentos : expresionAsignacion | listaArgumentos , expresionAsignacion


### EXPRESIONES

expresion : expresionAsignacion | expresion , expresionAsignacion

expresionAsignacion : expresionCondicional | expresionUnaria operadorAsignacion expresionAsignacion

expresionCondicional : expresionOrLogico 

expresionOrLogico : expresionAndLogico | expresionOrLogico || expresionAndLogico

expresionAndLogico : expresionIgualdad | expresionAndLogico && expresionIgualdad

expresionIgualdad : expresionRelacional | expresionIgualdad == expresionRelacional | expresionIgualdad != expresionRelacional

expresionRelacional : expresionAditiva | expresionRelacional < expresionAditiva | expresionRelacional > expresionAditiva | expresionRelacional <= expresionAditiva | expresionRelacional >= expresionAditiva

expresionAditiva : expresionMultiplicativa | expresionAditiva + expresionMultiplicativa | expresionAditiva – expresionMultiplicativa

expresionMultiplicativa : expresionConversion | expresionMultiplicativa * expresionConversion | expresionMultiplicativa / expresionConversion

expresionConversion : expresionUnaria | ( nombreTipo ) expresionConversion

expresionUnaria : expresionSufijo | ++ expresionUnaria | operadorUnario expresionConversion

expresionSufijo : expresionPrimaria | expresionSufijo [ expresion ] | expresionSufijo ( listaArgumentos? ) | expresionSufijo ++

expresionPrimaria : identificador | digito | literalCadena | ( expresion )


### EXPRESIONES CONSTANTES

expresionConstante : expresionCondicional


### SENTENCIAS

sentencia : sentenciaCompuesta | sentenciaExpresion | sentenciaSeleccion | sentenciaIteracion | sentenciaSalto

default  : sentencia

sentenciaCompuesta : { listaDeclaraciones? listaSentencias? }

listaDeclaraciones : declaracion | listaDeclaraciones declaracion

listaSentencias : sentencia | listaSentencias sentencia

sentenciaExpresion : expresion? ;

sentenciaSeleccion : if ( expresion ) sentencia | if ( expresion ) sentencia else sentencia

sentenciaIteracion : while ( expresion ) sentencia | for ( expresion? ; expresion? ; expresion? ) sentencia

sentenciaSalto : return expresion? ;