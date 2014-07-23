Gramatica
===

##Reglas gramaticales
==

ElementosEntrada : ElementoEntrada>

ElementoEntrada : whitespace | comentario | token 

comentario : “/” “*” caracter_ASCII>* - { “/” } “*” “/” | “/” “/” carácter_ASCII>* - { “\n” } 

digito : 0 | 1 | 2 |3 | 4 |5 | 6 | 7 | 8 | 9 

 letra : a | b | c | d | e | f | g | h | i | j | k | l | m | n | ñ | o | p | q | r | s | t | u | v | w | x | y | z | A | B | C | D | E | F | G | H | I | J | K | L | M | N | Ñ | O | P | Q | R | S | T | U | V | W | X | Y | Z | Á | É | Í | Ó | Ú 

whitespace : espacio en blanco

ascii  Caracteres del estándar ASCII

caracter_ASCII : ascii | Ñ | ñ 

token : identificador | palabra_reservada | literal | delimitador | operador 

identificador : “_” [ letra | digito | “_” | “$” ]* letra [ letra | digito | “_” | “$” ]* “$” [ letra | digito | “_” | “$” ]* 

palabra_reservada : “class” | “else” | “for” | “if” | “int” | “return” | “void” | “while” 


## LITERALES y OPERADORES
==

literal : literal_integer>

literal_integer : [“+” | “-“] [digito - {0}]* digito>* | [“+” | “-“] “0” 

delimitador : ( | ) | { | } | ; | , | . 

operador : op_infijo | op_prefijo | op_postfijo 

op_infijo : || | && | == | != | < | > | <= | >= | + | - | * | / 

op_prefijo : ! | + | -

op_postfijo : ++ | --

op_asignacion : = | += | -= | *= | /=


## TIPOS DE DATOS
==

tipo : tipo_primitivo identificador>

tipo_primitivo : “int”

lista_de_atributos : identificador [ “,” identificador ]* 

