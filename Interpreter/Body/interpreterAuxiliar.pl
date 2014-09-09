:- include('executers.pl').
:- include('expresions.pl').

interpreterAux(EntryFile,LabelTableNames, LabelTableValues,FunctionName,Trace,Cinput,Coutput):-
	load_xml_file(EntryFile, Program),
	%write(Program), write('\n'),
	removeEmpty(Program,GoodProgram),
	%write(GoodProgram), write('\n'),
	retractall(program(_)),
	assert(program(GoodProgram)),
	
	write(FunctionName),write('\n'),
	lookForFunction(GoodProgram,FunctionName,Function),
	
	state(InitS,[],[],[],[]),
		execute(0,InitS,Function,EndS,_),
		write('\ndespues de la funcion\n\n'),
	state(EndS,ExitTable,Cinput,Coutput,Trace),
	write('\n\n\n'),write(ExitTable),write('\n\n\n'),
	labelList(ExitTable,LabelTableNames,LabelTableValues),
	write('\ndespues del label\n\n'),
	write('\n LabelTableValues \n'),
	write(LabelTableValues),
	write('\n'),
	once(label(LabelTableValues)).
	%write(ExitTable),
	%write(Cinput),
	%write(Coutput),
	%write(Trace),
	%label(LabelTableValues).