%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
					
					%%%%%%%%%%%%%%%%%%%%%%%%%%%
					%       INTERPRETER       %
					%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-use_module(library(sgml)).
:-use_module(library(clpfd)).
:-use_module(library(sgml_write)).


:- include('Functions/VariablesTable.pl').
:- include('Functions/AuxiliaryFunctions.pl').
:- include('Body/interpreterAuxiliar.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					%%%%%%%%%%%%%%%
					% interpreter %
					%%%%%%%%%%%%%%%

% First you have to keep this file in a folder called "PFC"
% Then open swi Prolog and write "interpreter('funcionesBasicas.xml','output4.xml',potencia)." to test it

% Funcion principal, se le puede meter el fichero de entrada y salida, o incluirle también
% las variables inf, sup y maxDepth, si no se incluyen se ponen por defecto a: -3, 3 y 10

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interpreter(EntryFile, OutFile, FunctionName):- 
	interpreter(EntryFile, OutFile, -5, 15, 10, FunctionName). % Defaults

interpreter(EntryFile, OutFile, Inf, Sup, MaxDepth, FunctionName):- 
	write('\n\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n\n'),
	assert(inf(0)),
	assert(sup(0)),
	assert(maxDepth(0)),
	assert(program(0)),

	retractall(inf(_)),
	assert(inf(Inf)),
	retractall(sup(_)),
	assert(sup(Sup)),
	retractall(maxDepth(_)),
	assert(maxDepth(MaxDepth)),

	findall((N,L,T,Cin,Cout),interpreterAux(EntryFile,N,L,FunctionName,T,Cin,Cout),V),
	%interpreterAux(EntryFile, N,L),
	%write(V), write('\n'),
	open(OutFile, write, Stream, []),

    writeList(Stream,V),
   	%writeList(Stream,(N,L)),
    close(Stream).
