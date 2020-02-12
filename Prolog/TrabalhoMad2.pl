:- use_module(library(ic)).
:- use_module(library(ic_global)).
:- use_module(library(branch_and_bound)).
:- use_module(library(ic_cumulative)).

testDB(DataBase) :-
	compile(DataBase),
	findall(I,tarefa(I,_,_,_,_),L),
	length(L,N), writeln('Numero de tarefas' : N).
	
hours([8,9,10,11,13,14,15,16]).
lenHours(X):-
	hours(H),
	length(H,X).
proj():-
	findall(Id,tarefa(Id,_,_,_,_),Tasks),
	length(Tasks,N),
	domainDates(Dates,N),
	domainTimes(Times,N),
	domainSii(Sii,N),
	domainConcl(Concl),
	prec_constrs(Tasks,Dates,Times),
	minAfter_constrs(Tasks,Dates,Times),
	maxAfter_constrs(Tasks,Dates,Times),
	sii_constrs(Tasks,Tasks,Sii,Dates,Times),
	inter_constrs(Tasks,Times),
	createTarik(Tarik),
	single_constr(Tasks,Tarik),
	spec_constrs(Tasks,Tarik),
	simul_constrs(Tasks,Tasks,Tarik,Sii),
	minimize(labeling([Concl]),Concl),
	writeConcl(Concl),
	writeDates(Dates),
	writeWorkers(Tarik).

writeConcl(Concl):-
	writeDate(Concl).

	
writeDate(Date):-
	Calendario(Dates),
	element(Date,Dates,d(Day,Month,DayW)),
	writeln("dia: " : Day : " Mes: " : Month : "\nDia da semmana: " : DayW).

wireteDates([]).
writeDates([Date|Dates]):-
	writeDate(Date),
	writeDates(Dates).
	
writeWorkers(Tariks):-
	writeWorkers_(Tariks,1).

writeWorkers_([],_).
writeWorkers_([Tarik|Tariks],Id):-
	writeWorker(Tarik,Id),
	NextId is Id+1,
	writeWorkers_(Tariks,NextId,0).

writeWorkers_([],_,_).
writeWorkers_([Tark|Tarks],IdT,IdW):-
	writeWorkers__(Tark,IdT,IdW),
	NextIdW is IdW+1,
	writeWorkers_(Tarks,IdT,NextIdW).

writeWorkers__(Tark,IdT,IdW):-
	writeln("Tarefa " : IdT : " feita por " : IdW : " com especialidade " : Tark).
	
domainSii(Sii,N):-
	length(Sii,N),
	domainSi(Sii,N).

domainSi([]).
domainSi([Si|Sis],N):-
	length(Si,N),
	Si #:: 0..1,
	domainSi(Sis,N).

	
simul_constrs([],_,_,_).
simul_constrs([Task|RTasks],Tasks,Tarik,Sii):-
	simul_constrs_(Task,Tasks,Tarik,Sii),
	simul_constrs(RTasks,Tasks,Tarik,Sii).
	
simul_constrs_(_,[],_,_).
simul_constrs_(Task1,[Task2|Tasks],Tarik,Sii):-
	Task1\=Task2,
	simul_constrs__(Task1,Task2,Tarik,Sii),
	simul_constrs_(Task1,Tasks,Tarik,sii).
	
simul_constrs__(Task1,Task2,Tark,Sii):-
	element(Task1,Tark,Tark1),
	element(Task2,Tark,Tark2),
	element(Task1,Sii,Si),
	element(Task2,Si,S),
	simul_constrs___(Tark1,Tark2,S).

simul_constrs___([],_,_).
simul_constrs___([Tark1|Tarks1],[Tark2,Tarks2],S):-
	S$=1 and Tark1$\=0 => Tark2#=0,
	simul_constrs___(Tarks1,Tarks2,S).
	
	
sii_constrs([],_,_,_,_).
sii_constrs([Task|RTasks],Tasks,Sii,Dates,Times):-
	sii_constrs_(Task,Tasks,Sii,Dates,Times),
	sii_constrs(RTasks,Tasks,Sii,Dates,Times).

sii_constrs_(_,[],_,_,_).
sii_constrs_(Task1,[Task2|RTasks],Sii,Dates,Times):-
	tarefa(Task1,_,Duracao,_,_),
	element(Task1,Sii,Si),
	element(Task2,Si,S),
	element(Task1,Dates,DateI),
	element(Task2,Dates,DateJ),
	element(Task1,Times,TimeI),
	element(Task2,Times,TimeJ),
	lenHours(NH),
	DateI*NH+TimeI$=<DateJ*NH+TimeJ and DateJ*NH+TimeJ$=<DateI*NH+TimeI+Duracao => S#=0,
	sii_constrs_(Task1,[RTasks],Sii,Dates,Times).
	
spec_constrs([],_).
spec_constrs([Task|Tasks],Tarik):-
	element(Task,Tarik,Tark),
	tarefa(Task,_,_,Specs,_),
	spec_constrs_(Specs,Tark),
	spec_constrs(Tasks,Tark).

spec_constrs_([],_).
spec_constrs_([r(E,Ntrabs)|Specs],Tark):-
	spec_constrs__(E,Tark,Ne),
	Ne=Ntrabs,
	spec_constrs_(Specs,Tark).

spec_constrs__(_,[],0).
spec_constrs__(E,[Tark|Tarks],Ne):-
	=:=(Tark,E,Bool),
	spec_constrs__(E,Tarks,Ne2),
	Ne is Ne2+Bool.
	
	
createTarik(Tarik):-
	findall(Id,tarefa(Id,_,_,_,_),Tasks),
	length(Tasks,N),
	length(Tarik,N),
	createTark(Tarik).

createTark([]).
createTark([Tark|Tarsk]):-
	findall(Id,trabalhador(Id,_),Workers),
	length(Workers,NW),
	length(Tark,NW),
	trabalhador(Id,SpecList),
	append(SpecList,[0],SpecList0),
	Tark #:: SpecList0,
	createTark(Tarsk).

single_constr([]).
single_constr([Tarik|Tariks]):-
	single_constr_(Tarik),
	single_constr(Tariks).

single_constr_([]).
single_constr_([Tark|Tarks]):-
	=:=(tark,0,Bool),!,
	single_constr__(Tarks,Bool),
	single_constr_(Tarks).
	
single_constr__([Tark|Tarks],0):-
	Tark#=0,
	single_constr__(Tarks,0).
single_constr__([Tark|Tarks],1).
	

domainConcl(Concl):-
	prazo(d(D1,M1,_)),
	calendario(Days),
	element(I1,Days,d(D2,M2,_)),
	I2 is I1-1,
	element(I2,Days,d(D3,M3,_)),
	(M2>M1;M2=M1,D2>=D1),
	(M3<M1;M3=M1,D3<=D1),
	Concl #:: 0..I2.
domainDates(Dates,N):-
	length(Dates,N),
	calendario(Days),
	length(Days,ND),
	Dates #:: 1,ND.
domainTimes(Times,N):-
	length(Times,N),
	lenHours(NH),
	Times #:: 1..NH,
	
minAfter_constrs([],_,_).
minAfter_constrs([Task|Tasks],Dates,Times):-
	findall(minafter(Ntask,Min),intervalo(Task,NTask,Min),NextTasks),
	minAfter_constrs_(Task,Dates,Times,NextTasks),
	minAfter_constrs(Tasks,Dates,Times).

	
minAfter_constrs_(_,_,_,[]).
minAfter_constrs_(Task,Dates,Times,[minafter(Ntask,Min)|NextTasks]):-
	tarefa(Task,_Duracao,_,_),
	element(Task,Dates,DateI),
	element(Ntask,Dates,DateJ),
	element(Task,Times,TimeI),
	element(Ntask,Times,TimeJ),
	lenHours(NH),
	DateI*NH+TimeIDuracao+Min #=< DateJ*NH*TimeJ,
	minAfter_constrs_(Task,Dates,Times,NextTasks).

	
maxAfter_constrs([],_,_).
maxAfter_constrs([Task|Tasks],Dates,Times):-
	findall(maxafter(Ntask,Max),intervalo(Task,NTask,Max),NextTasks),
	maxAfter_constrs_(Task,Dates,Times,NextTasks),
	maxAfter_constrs(Tasks,Dates,Times).

maxAfter_constrs_(_,_,_,[]).
maxAfter_constrs_(Task,Dates,Times,[maxafter(Ntask,Min)|NextTasks]):-
	tarefa(Task,_Duracao,_,_),
	element(Task,Dates,DateI),
	element(Ntask,Dates,DateJ),
	element(Task,Times,TimeI),
	element(Ntask,Times,TimeJ),
	lenHours(NH),
	DateI*NH+TimeIDuracao+Max #>= DateJ*NH*TimeJ,
	maxAfter_constrs_(Task,Dates,Times,NextTasks).
	
	
prec_constrs([],_,_,_).
prec_constrs([Task|Tasks],Dates,Times,Concl):-
	element(Task,Dates,DateI),
	element(Task,Times,TimeI),
	tarefa(Task,PrecTasks,Duration,_,_),
	prec_constrs_(PrecTasks,Dates,Times,DateI,TimeI,Duration),
	lenHours(NH),
	Concl #=> DateI+mod(Duration,NH),
	prec_constrs(Tasks,Dates,Times,Concl).
	
prec_constrs_([],_,_,_,_).
prec_constrs_([J|RTSegs],Dates,Times,DateI,TimeI,Duration) :-
	element(J,Dates,DateJ),
	element(J,Times,TimeJ),
	lenHours(NH),
	DateI*NH+TimeI+Duration #=< DateJ*NH+TimeJ,
	prec_constrs_(RTSegs,Dates,DateI,TimeI,Duration).
	
inter_constrs([Task|Tasks],Times):-
	tarefa(Task,_,Duration,_,0),
	element(Task,Times,TimeI),
	lenHours(NH),
	TimeI+Duration #=<NH,!,
	inter_constrs(Tasks,Times).

inter_constrs([Task|Tasks],Times):-
	tarefa(Task,_,Duration,_,1),
	inter_constrs(Tasks,Times).