%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

						%%%%%%%%%%%%%%%%%%%
						%   EXPRESSIONS   %
						%%%%%%%%%%%%%%%%%%%

% Resuelve la expresión que se le pasa, puede ser operación:
% 'binaria', 'unaria', 'llamada', 'buleana' ó 'aritmética' ...

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resolveExpression(Entry,[],_,Entry):-!.

resolveExpression(EntryS,('notOperator',_,[Expr]),NotResult,OutS):-
	resolveExpression(EntryS,Expr,Result,OutS),
	not(Result,NotResult).

resolveExpression(EntryS,('signOperator',[type='-'],[Expr]),InvResult,OutS):-
	write(Expr),write('\n'),
	resolveExpression(EntryS,Expr,Result,OutS),
	work('*',-1,Result,InvResult),write(InvResult),write('\n').

resolveExpression(EntryS,('signOperator',[type='+'],[Expr]),Result,OutS):-
	write('signOp'),write('\n'),
	resolveExpression(EntryS,Expr,Result,OutS),write(Result),write('\n').


resolveExpression(EntryS,('binaryOperator',Operator,[X,Y]),Result,OutS):-
	getContent(Operator,Op),
	write('\nOperator: '),write(Operator),write('\n'),
	%write(X),write('\n'),
	%write(Y),write('\n'),
	resolveExpression(EntryS,X, Operand1,EntryS1),
	resolveExpression(EntryS1,Y, Operand2,OutS),
	write('operand '),write(Operand1),write('\n'),
	write('operand '),write(Operand2),write('\n'),
	work(Op, Operand1, Operand2,Result),
	write('result '),write(Result),write('\n').

resolveExpression(EntryS,('variable',[_=OperandName],_),OperandValue,EntryS):-
	state(EntryS,Table,_,_,_),
	%write('---resolveExpression---'),write('\n'),
	getValue(Table,OperandName,OperandValue).
	%write('\nTable \n'),write(Table),write('\n'),
	%write('\nOperandName \n'),write(OperandName),write('\n'),
	%write('\nOperandValue \n'),write(OperandValue),write('\n').

resolveExpression(EntryS,('const',[_=Value],_),Result,EntryS):-
	write('const: '),write(Value),write('\n'),
	atom_number(Value,Result),
	write('result const: '),write(Result),write('\n').

resolveExpression(EntryS,('consoleIn',[_=int],_),Value,OutS):-
	state(EntryS,Table,Cin,Cout,Trace),
	inf(X), sup(Y),
	Value in X..Y,
	append(Cin,[' ',Value],Cin1),
	state(OutS,Table,Cin1,Cout,Trace).


resolveExpression(EntryS,('callFunction',[name=Name, type=Type],Args),ValueReturned,OutS):-
	write('en la caallllfunciton '),write('\n'),
	program(Program),
	lookForFunction(Program,Name,Type,[(_,_,Params),Function]),
	write('params '),write(Params),write('\n'),
	%write('funcion '),write(Function),write('\n'),
	write('argumentos '),write(Args),write('\n'),
	apila([],TCall),
	getTuple(Type,Tuple),
	add(TCall,Tuple,TCall1),
	buildCallTable(EntryS,EntryS1,TCall1,Params,Args,TCall2),
	state(EntryS1,Table,Cin,Cout,Trace),
	state(EntryCall,TCall2,Cin,Cout,Trace),!,
	execute(EntryCall,[Function],EntryS2),
	state(EntryS2,[[(_,_,ValueReturned)|_]],Cin2,Cout2,Trace2),
	write('el estado de vuelta '),write(ValueReturned),write('\n'),
	state(OutS,Table,Cin2,Cout2,Trace2).
	%ValueReturned #= 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Inicializamos una tabla con los valores de los argumentos pasados a la llamada

buildCallTable(S,S,T,[],[],T):-!,
write('tablitaaa'),write(T),write('\n').

buildCallTable(EntryS,OutS,Tin,[(_,[_=Type,_=Name],_)|Params],[(_,_,[Arg])|Args],Tout):-
	write('estamos construyendo la tabla '),write('\n'),
	resolveExpression(EntryS,Arg,Value,EntryS1),
	write('meteremos el valor en la tabla '),write('\n'),
	add(Tin,(Type,Name,Value),T1),
	write('metido el valor en la tabla '),write('\n'),
	buildCallTable(EntryS1,OutS,T1,Params,Args,Tout).


%resolveExpression((Entry,Cin,Cout,Trace),[('callFunction',[name=Name, type=Type],Params)],ValueReturned, (Out,Cin1,Cout1,Trace1)):-!,	% he añadido el Out
%	apila(Entry,Entry1),
%	addListParams(Entry1,Params,Out1),
%	program(Program),

%	lookForFunction(Program,Name,Type,Function),
	
%	createListParams(Function,Body,ListParams),
%	updateNames(Out1,ListParams,Out2),
%	execute((Out2,Cin,Cout,Trace),Body,(Out,Cin1,Cout1,Trace1)),
%	returnesValue(Out,ValueReturned).

						%%%%%%%%%%%%
						%   BOOL   %
						%%%%%%%%%%%%

not(Value,1):-Value#=0.
not(Value,0):-Value#=1.

work('<', Op1,Op2,1):- Op1 #< Op2.
work('<', Op1,Op2,0):- Op1 #>= Op2.

work('<=', Op1,Op2, 1):- Op1 #=< Op2.
work('<=', Op1,Op2,0):- Op1 #> Op2.

work('>=', Op1,Op2,1):- Op1 #>= Op2.
work('>=', Op1,Op2,0):- Op1 #< Op2.

work('>', Op1,Op2,1):- Op1 #> Op2.
work('>', Op1,Op2,0):- Op1 #=< Op2.

work('==', Op1,Op2,1):- Op1 #= Op2.
work('==', Op1,Op2,0):- Op1 #\= Op2.

work('!=', Op1,Op2,1):- Op1 #\= Op2.
work('!=', Op1,Op2,0):- Op1 #= Op2.

work('&&', Op1,Op2,1):- Op1 #/\ Op2.
work('&&', Op1,Op2,0):- not(Op1,Op1N),not(Op2,Op2N),work('||', Op1N,Op2N,1).

work('||', Op1,Op2,1):- Op1 #\/ Op2.
work('||', Op1,Op2,0):-	not(Op1,Op1N),not(Op2,Op2N),work('&&', Op1N,Op2N,1).



						%%%%%%%%%%%%%%%%%%
						%   Arithmetic   %
						%%%%%%%%%%%%%%%%%%

work('+', Op1,Op2,Z):- !, Z #= Op1 + Op2.
work('-', Op1,Op2,Z):- !, Z #= Op1 - Op2.
work('*', Op1,Op2,Z):- !, Z #= Op1 * Op2.
%work('/', _,0,_):- !, fail.
work('/', Op1,Op2,Z):- !, Z #= Op1 / Op2.


