:- use_module(library(ic)).
:- use_module(library(ic_global)).
:- use_module(library(branch_and_bound)).
:- use_module(library(ic_cumulative)).

testDB(DataBase) :-
	compile(DataBase),
	findall(I,tarefa(I,_,_,_),L),
	length(L,N), writeln('Numero de tarefas' : N).

/*obtains the minimm number of workers needed and the times that tasks have to start at*/
minimumWorkers(sol(NumWorkers,Dates)):-
	minWorkers_(NumWorkers,Dates),!,
	labeling(Dates).

/*obtains the dates and number of workers of a solution*/
minWorkers_(NumWorkers,Dates):-
	proj(_,Dates),!,
	findall(Duration,tarefa(_,_,Duration,_),Durations),
	findall(Workers,tarefa(_,_,_,Workers),Resources),
	workersForES(MaxW),
	criticalWorkers(MinW),
	NumWorkers #:: MinW..MaxW,
	minimize(binSearch(NumWorkers,MinW,MaxW,Dates,Durations,Resources),NumWorkers),
	writeln("success").

/*minimizes the number of workers using binary search algoritm*/
binSearch(MaxW,MinW,MaxW,_,_,_):-
	MinW>=MaxW,!.
binSearch(NumWorkers,MinW,MaxW,Dates,Durations,Resources):-
	Temp is MaxW+MinW,
	TryNumWorkers is div(Temp,2),
	write("trying "),write(TryNumWorkers),writeln(" workers"),
	cumulative(Dates,Durations,Resources,TryNumWorkers),
	binSearch(NumWorkers,MinW,TryNumWorkers,Dates,Durations,Resources).
binSearch(NumWorkers,MinW,MaxW,Dates,Durations,Resources):-
	Temp is MaxW+MinW,
	NewMinW is div(Temp,2)+1,!,
	NumWorkers #>=NewMinW,
	binSearch(NumWorkers,NewMinW,MaxW,Dates,Durations,Resources).

/*calculates the number of workers needed for a project if all tasks start at the earliest start*/
workersForES(NumWorkers):-
	proj(_,Dates),!,
	findall(Id,tarefa(Id,_,_,_),Tasks),
	labeling(Dates),!,
	workersUsed(Tasks,Dates,NumWorkers).
	
	
/*calculates the number of workers needed to complete the critical tasks*/
criticalWorkers(NumWorkers):-
	proj(_,Dates),!,
	findall(Id,tarefa(Id,_,_,_),Tasks),!,
	criticalTasks(Tasks,Dates,CritTasks),
	maxConcurrentWorkers(CritTasks,CritTasks,Dates,NumWorkers),
	labeling(Dates),!.

/*obtains all critical tasks*/
criticalTasks([],_,[]).
criticalTasks([Task|Tasks],Dates,[Task|CritTasks]):-
	criticalTask(Task,Dates),!,
	criticalTasks(Tasks,Dates,CritTasks).
criticalTasks([_|Tasks],Dates,CritTasks):-
	criticalTasks(Tasks,Dates,CritTasks).

/*is true if a task is a critical task*/
criticalTask(Task,Dates):-
	element(Task,Dates,Date),
	get_domain_size(Date,1).

/*calculates the maximum number of workers working simultaneously*/
maxConcurrentWorkers([],_,_,0).
maxConcurrentWorkers([Task|RTasks],Tasks,Dates,NumWorkers):-
	element(Task,Dates,DateI),!,
	concurrentWorkers(Tasks,Dates,DateI,CurWorkers),
	maxConcurrentWorkers(RTasks,Tasks,Dates,NextWorkers),
	NumWorkers is max(CurWorkers,NextWorkers).

/*calculates the number of workers needed at the time a task starts*/
concurrentWorkers([],_,_,0).
concurrentWorkers([Task|RTasks],Dates,DateI,SimulWorkers):-
	element(Task,Dates,Date),
	Date =< DateI,
	tarefa(Task,_,Duration,Workers),
	Date+Duration > DateI,!,
	concurrentWorkers(RTasks,Dates,DateI,NextWorkers),
	SimulWorkers is Workers + NextWorkers.
concurrentWorkers([_|RTasks],Dates,DateI,Workers):-
	concurrentWorkers(RTasks,Dates,DateI,Workers).

/*calculates the maximum number of workers working simultaneously*/
workersUsed(Tasks,Dates,SimulWorkers):-
	findall(Date,(member(Task,Tasks),element(Task,Dates,Date)),ResDates),
	findall(Duration,(member(Task,Tasks),tarefa(Task,_,Duration,_)),Durations),
	findall(Workers,(member(Task,Tasks),tarefa(Task,_,_,Workers)),Resources),
	cumulative(ResDates,Durations,Resources,SimulWorkers),labeling([SimulWorkers]),!.
	
/*calculates the earliest finish time and the ES for each task in a project*/
proj(Concl,Dates) :-
	findall(Id,tarefa(Id,_,_,_),Tasks),
	length(Tasks,N),
	length(Dates,N),
	maxduration(Tasks,MaxConcl),
	[Concl|Dates] #:: 0..MaxConcl,!,
	prec_constrs(Tasks,Dates,Concl),
	get_min(Concl,Concl).

/*creates the precendence restrictions for the project*/
prec_constrs([],_,_).
prec_constrs([Task|RTasks],Dates,Concl) :-
	element(Task,Dates,DateI),
	tarefa(Task,PrecTasks,Duration,_),
	prec_constrs_(PrecTasks,Dates,DateI,Duration),
	DateI+Duration #=< Concl,
	prec_constrs(RTasks,Dates,Concl).

/*creates the precendence restrictions for a task*/
prec_constrs_([],_,_,_).
prec_constrs_([J|RTSegs],Dates,DateI,Duration) :-
	element(J,Dates,DateJ),
	DateI+Duration #=< DateJ,
	prec_constrs_(RTSegs,Dates,DateI,Duration).

/*sums the duration of all tasks(its the upper bound of the project total duration)*/
maxduration([],0).
maxduration([Task|RTasks],Sumf) :-
	tarefa(Task,_,Duration,_),
	maxduration(RTasks,Sum), 
	Sumf is Sum+Duration.

