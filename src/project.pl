% DECLARATIVE PROGRAMMING PROJECT - TASK-PARALLEL SCHEDULER
% @author: Brian Delhaisse.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% useful predicates %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% load/1
% @param: Filename - name of the file to load.
load(Filename) :- load_files(Filename), assert(file(Filename)).

% unload/1
% @param: Filename - name of the file to unload.
unload(Filename) :- unload_file(Filename), retract(file(Filename)).

% listCores/1
% @return: L - the sorted list of cores.
listCores(L) :- setof(C, core(C), LC), predsort(predCore, LC, L).

% listTasks/1
% @return: L - the sorted list of tasks.
listTasks(L) :- setof(T, task(T), LT), predsort(predTask, LT, L).

% predTask/3   -   predCore/3
% predTask(-Delta, +V1, +V2)   -   predCore(-Delta, +V1, +V2).
% @params: V1, V2.
% @return: Delta - Delta = '<', '>' or '=' based on V1 and V2.
% Those predicates are used by predSort.
predTask(Delta, V1, V2) :- 
   check(t,V1,V2,Delta), !.
predCore(Delta, V1, V2) :-
   check(c,V1,V2,Delta), !.
check(Type,V1,V2,Delta) :-
   atom_concat(Type, X1, V1),
   atom_concat(Type, X2, V2),
   atom_number(X1, N1),
   atom_number(X2, N2),
   check(N1,N2,Delta).
check(N1,N2,<) :- N1 < N2.
check(N1,N2,>) :- N1 > N2.
check(_,_,=).


% is_dependent/2
% @params: T1, T2. 
% @return: true if T1 depends directly or undirectly on the task T2.
is_dependent(T1,T2) :- depends_on(T1,T2,_).
is_dependent(T1,T2) :- depends_on(T1,T,_), is_dependent(T,T2).

% listDependentTask/2
% @params: Task
% @return: ListTask - list of tasks that are directly or undirectly dependent on the Task given in argument. 
%                     If no tasks are dependent, returns an empty list.
listDependentTask(Task, ListTask) :- setof(T, is_dependent(Task,T) , ListTask), !.
listDependentTask(_, []).

% listDirectDependentTask/2
% @params: Task
% @return: ListTask - list of tasks that are directly dependent on the Task given in argument. 
%                     If no tasks are directly dependent, returns false.
listDirectDependentTask(Task, ListTask) :- setof(T, depends_on(Task,T,_), ListTask).

% getCore/3
% @params: Task, Solution.
% @return: Core - Core that will execute the Task in the given Solution.
%          returns false if the Task does not exist.
% @note: use the predicate member (http://www.swi-prolog.org/pldoc/doc_for?object=member/2)
getCore(Task, solution([schedule(Core,ListTask)|_]), Core) :- member(Task, ListTask), !.
getCore(Task, solution([schedule(_,_)|Rest]), Core) :- getCore(Task, solution(Rest), Core).

% communication_cost/4
% @param: C1, C2, Data.
% @return : Cost - the communication cost when moving Data (megabytes) from the core C1 to the core C2.
% @note: if C1 == C2, then there is no communication cost (Cost=0).
communication_cost(C1,C1,_,0).
communication_cost(C1,C2,Data,Cost) :- channel(C1,C2,Latency,Bandwith), Cost is Latency+Data/Bandwith.

% communication_cost/5
% @param: C1, C2, T1, T2.
% @return : Cost. - the communication cost when moving Data (megabytes) from the core C1 (where the old task T2 ended)
%                   to the core C2 (where the new task T1 is about to start).
% @note: if C1 == C2, then there is no communication cost (Cost=0).
communication_cost(C1,C1,_,_,0).
communication_cost(C1,C2,T1,T2,Cost) :- depends_on(T1,T2,Data), communication_cost(C1,C2,Data,Cost).

% dependency/0
% @returns: false if the tasks are independents.
dependency :- setof(T1-T2, depends_on(T1,T2,_), L), L \== [].

% assign/4
% @params: Task, Core, InitialSolution.
% @return: Solution - this solution is the InitialSolution with the Task assigned to the Core.
assign(Htask, C, solution([schedule(C,[])|Rest]), solution([schedule(C,[Htask])|Rest])) :- !.
assign(Htask, C, solution([schedule(C, ListTask)|Rest]), solution([schedule(C, [Htask|ListTask])|Rest])) :- !.
assign(Htask, C, solution([schedule(C1,L)|Rest1]), solution([schedule(C1,L)|Rest2])) :-
   assign(Htask, C, solution(Rest1), solution(Rest2)).

% mergeSorted/3
% merge two solutions S1, S2 that are already sorted (i.e. all the tasks in S2 > S1) into one Solution.
% Ex: mergeSorted(solution([schedule(c1, [t1,t2]), schedule(c2,[t3,t4])]), solution([schedule(c1, [t5,t6]), schedule(c2,[t7,t8])]), S).
mergeSorted(solution([]), solution([]), solution([])) :- !.
mergeSorted(S1, [], S1).
mergeSorted([], S2, S2).
mergeSorted(solution([schedule(Core,ListTask1)|Rest1]), solution([schedule(Core,ListTask2)|Rest2]), solution([schedule(Core,R)|Rest]) ) :-
   append(ListTask1, ListTask2, R),
   mergeSorted(solution(Rest1), solution(Rest2), solution(Rest)).

% merge/3
% merge two solutions S1, S2 that are not obligatory sorted into one Solution.
merge(solution([]), solution([]), solution([])) :- !.
merge(S1, [], S1).
merge([], S2, S2).
merge(solution([schedule(Core,ListTask1)|Rest1]), solution([schedule(Core,ListTask2)|Rest2]), solution([schedule(Core,R)|Rest]) ) :-
   append(ListTask1, ListTask2, R1),
   predsort(predTask, R1, R),
   merge(solution(Rest1), solution(Rest2), solution(Rest)).

% saveSolution/4
% @params: Filename, Solution, Cost, Method.
% Save the solution along with the cost and the method that was used to find the solution into a file (specified by the Filename),
% in the form of the predicate bestSol(Solution, Cost, Method).
saveSolution(Filename, Solution, Cost, Method) :-
   open(Filename, append, Stream),
   file(Name),
   write(Stream, bestSol(Solution, Cost, Method, Name)),
   nl(Stream), nl(Stream),
   close(Stream).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% isSolution(S) %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% isSolution/1
% @params: S (=solution, must be in the form solution([schedule(c1,ListTasks1)|Rest]).
% @return: true if the argument given is a valid solution.
% Checks if the solution given has no duplicates, has all the cores and tasks, and if it 
% respects the dependencies between the tasks.
isSolution(solution([])) :- fail.
isSolution(S) :-
   listOfCoreAndTask(S, LC, LT),
   checkDuplicates(LC, LT),
   checkPresenceOfAllCoresAndTasks(LC, LT),
   checkDependencies(S), !.

% checkDuplicates/2
% @params: ListCores, ListTasks.
% @return: true if no duplicates detected in the 2 lists given in arguments.
% Checks for duplicates in the list of Cores and Tasks given in arguments.
% @note: use the predicate is_set (http://www.swi-prolog.org/pldoc/doc_for?object=is_set/1)
checkDuplicates(ListCores, ListTasks) :-
   is_set(ListCores),
   is_set(ListTasks).

% checkPresenceOfAllCoresAndTasks/2
% @params: ListCores, ListTasks.
% @return: true if all the cores and tasks are in the 2 lists given in arguments.
% Checks if all the cores and tasks are respectively in the list of cores and tasks given in argument.
% @note: use the predicate sort (http://www.swi-prolog.org/pldoc/man?predicate=sort/2).
checkPresenceOfAllCoresAndTasks(ListCores, ListTasks) :-
   listCores(LC),
   listTasks(LT),
   predsort(predCore, ListCores, SortedLC),
   LC == SortedLC,
   predsort(predTask, ListTasks, SortedLT),
   LT == SortedLT.

% checkDependencies/1
% @params: Solution
% @return: true if the dependencies are respected.
% Checks if the tasks are in the correct order, i.e. checks if the dependency between the tasks is respected.
% @note: use the predicate intersection (http://www.swi-prolog.org/pldoc/doc_for?object=intersection/3)
checkDependencies(solution([])).
checkDependencies(solution([schedule(Core,[Htask|Ttask])|Rest])) :- 
   listDependentTask(Htask, ListDependentTask),
   intersection(ListDependentTask, Ttask, Result),
   Result == [],
   checkDependencies(solution([schedule(Core,Ttask)|Rest])), !.
checkDependencies(solution([schedule(_,[])|Rest])) :- 
   checkDependencies(solution(Rest)).

% listOfCoreAndTask/3
% @params: Solution.
% @return: ListCores, ListTasks.
% Extracts the list of all the cores and tasks from the Solution.
% @note: uses the listOfCore/5 with accumulator, and wraps it.
listOfCoreAndTask(Solution, ListCores, ListTasks) :-
   listOfCoreAndTask(Solution, [], [], ListCores, ListTasks).
listOfCoreAndTask(solution([]), AccCore, AccTask, AccCore, AccTask).
listOfCoreAndTask(solution([schedule(Core,Task)|Rest]), AccCore, AccTask, ResultListCore, ResultListTask) :-
   append(AccCore, [Core], ResultCore),
   append(AccTask, Task, ResultTask),
   listOfCoreAndTask(solution(Rest), ResultCore, ResultTask, ResultListCore, ResultListTask).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% execution_time(S,ET) %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% execution_time/2
% @params: Solution
% @return: ExecTime - the time it took to execute all the tasks on the various cores 
%                     (defined in the Solution given in argument).
execution_time(solution([]),0).
execution_time(solution([schedule(Core,Task)|Rest]),ExecTime) :- 
   not(dependency),
   sumTaskTime(Core, Task, Time1),
   execution_time(solution(Rest), Time2),
   ExecTime is max(Time1, Time2), !.
execution_time(Solution, ExecTime) :- 
   listTasks(LTasks),
   last(LTasks, Task),
   getCost(Solution, [Task], [ExecTime]),
   cleanCost, !.

% cleanCost/0
% removes all the totCost(_,_,_) in the database.
% @note: totCost(_,_,_) is created when calling getCost/4 (Dynamic Programming).
cleanCost :- retractall(totCost(_,_,_)).

% calculateCost/4
% @params: Solution, Core, Task.
% @return: TotCost - the cost it takes for running a Task on a specified Core knowing the Solution,
% that is, knowing the time it took to run all the dependent Tasks on the various Cores.
% PREVIOUS FORMULA (WRONG):
% Cost(core, task) = process_cost(task, core) + sum_d communication_cost(core_d, core, task_d, task)
%		    + max( cost(core, previousTask), {cost(core_d, task_d)}_d ).
% UPDATE FORMULA:
% Cost(core, task) = process_cost(task, core) + max( cost(core, previousTask), 
%		     {cost(core_d, task_d) + communication_cost(core_d, core, task_d, task}_d ).
% where d = directly dependent tasks.
calculateCost(Sol, Core, Task, TotCost) :-
   listDirectDependentTask(Task, ListDependentTask),
   process_cost(Task, Core, Cost),
   getPreviousTaskCost(Sol, Core, Task, PrevTaskCost),
   getDependentTaskCost(Sol, Core, Task, ListDependentTask, ListDependCost),
   max_list([PrevTaskCost|ListDependCost], MaxCost),
   TotCost is Cost + MaxCost, !.
calculateCost(_, Core, Task, TotCost) :-
   process_cost(Task, Core, TotCost).

% getDependentTaskCost/5
% @params: Solution, Core, Task, ListDependentTask.
% @return: ListDependentCost - the list of cost of the dependent tasks.
% cost_d = cost(core_d, task_d) + communication_cost(core_d, core, task_d, task)
getDependentTaskCost(_, _, _, [], []).
getDependentTaskCost(Sol, Core, Task, [HtaskD|TtaskD], [HcostD|TcostD]) :-
   getCore(HtaskD, Sol, CoreD),
   communication_cost(CoreD, Core, Task, HtaskD, CommCost),
   getCost(Sol, CoreD, HtaskD, Cost),
   HcostD is CommCost + Cost,
   getDependentTaskCost(Sol, Core, Task, TtaskD, TcostD).

% getPreviousTaskCost/4
% @params: Solution, Core, Task.
% @return: Cost - the cost of the possible task in front of Task in the Solution. Returns 0 if no such task.
getPreviousTaskCost(Sol, Core, Task, Cost) :-
   getPreviousTask(Sol, Core, Task, PrevTask),
   getCost(Sol, Core, PrevTask, Cost), !.   
getPreviousTaskCost(_, _, _, 0).

% getPreviousTask/4
% @params: Solution, Core, Task.
% @return: PrevTask - the task which is in front of Task in the Solution.
getPreviousTask(solution([]), _, _, _) :- fail.
getPreviousTask(solution([schedule(Core,[Task|_])|_]), Core, Task, _) :- fail.
getPreviousTask(solution([schedule(Core,[PrevTask,Task|_])|_]), Core, Task, PrevTask) :- !.
getPreviousTask(solution([schedule(Core,[_|RestTask])|_]), Core, Task, PrevTask) :-
   getPreviousTask(solution([schedule(Core,RestTask)|_]), Core, Task, PrevTask), !.
getPreviousTask(solution([schedule(Core1,_)|Rest]), Core, Task, PrevTask) :- 
   Core1 \== Core,
   getPreviousTask(solution(Rest), Core, Task, PrevTask), !.

%% getCost/3
%% @params: Solution, ListOfTask.
%% @return: ListOfCost - a list which contains the cost for each task in ListTask described in the Solution.
%% @note: calls getCost/4 to get the cost for one task.
getCost(_, [], []).
getCost(Sol, [HdependTask|TdependTask], [Hcost|Tcost]) :-
   getCore(HdependTask, Sol, Core),
   getCost(Sol, Core, HdependTask, Hcost),
   getCost(Sol, TdependTask, Tcost).

%% getCost/4
%% @params: Solution, Task, Core.
%% @return: Cost - the Cost it takes to run the Task on the specified Core knowing the Solution 
%%                 (i.e. knowing the time it took to run all the dependent tasks on the various cores).
%% @note: calls calculateCost/4 if it doesn't know the cost, and cache it (Dynamic Programming).
:- dynamic totCost/3.
getCost(_, Core, Task, Cost) :-
   totCost(Core, Task, Cost), !.
getCost(Sol, Core, Task, Cost) :-
   calculateCost(Sol, Core, Task, Cost),
   assert(totCost(Core, Task, Cost)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% find_optimal(S) %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% buildEmptySolution/1
% @return: Solution - solution([schedule(c1, []), ..., schedule(cn,[])])
% Returns an empty solution (i.e. the tasks have not been attributed yet).
buildEmptySolution(solution(S)) :- setof(schedule(C,[]), core(C), S).

% getSolution/1
% @return: Solution - a possible solution
% Gets all the possible solutions (brute force search).
getSolution(solution(S)) :-
   listTasks(ListTask),
   buildEmptySolution(solution(S1)),
   buildSolution(ListTask, S1, S).

getSolution(solution(S), ListTask) :-
   buildEmptySolution(solution(S1)),
   buildSolution(ListTask, S1, S).

% buildSolution/3
% @params: ListTask, EmptySolution.
% @return: FinalSolution - Solution that has been built.
buildSolution([], [], []).
buildSolution([], [schedule(C,_)|Rest], [schedule(C,[])|Rest]) :- !.
buildSolution(ListTask, [schedule(C,_)|Rest1], [schedule(C,ListTask2)|Rest2]) :-
   subl(ListTask, ListTask2),
   subtract(ListTask, ListTask2, ListTask3),
   buildSolution(ListTask3, Rest1, Rest2).

% subl/2
% @params: List.
% @return: Sublist - a possible sublist of List.
% Eventually, it will return all the possible sublists.
subl([],[]).    
subl([First|Rest],[First|Sub]):-
   subl(Rest,Sub).
subl([_|Rest],Sub):-
   subl(Rest,Sub).

% better/3 
% @params: S1, S2.
% @return: S - the one that is better. 
better(S1,S2,S1) :-
   execution_time(S1,ET1),
   execution_time(S2,ET2),
   ET1 =< ET2,
   !.
better(_,S2,S2).

% find_optimal/1
% @return: Solution - the optimal solution.
% Gets all the possible solution and checks which one is the best.
% Saves the best in a database (using assert).
:- dynamic best/2.
find_optimal(_) :- 
   assert(best(nil,100000)),
   getSolution(S),
   execution_time(S,ET),
   update_best(S,ET),
   fail.
find_optimal(S) :-
   best(S,_), retractall(best(_,_)), !.

% update_best/2
% @params: Solution, ExecTime.
% Updates the best solution found, removes the old one, and saves the new one.
update_best(S,ET) :- 
   best(_,BestET),
   ET < BestET,
   !,
   retract(best(_,_)),
   assert(best(S,ET)).
update_best(_,_).

% find_heuristically/1
% @return: the solution - for now, the best solution is calling find_randomly(S,N,Cost).
% where N is the number of iterations.
find_heuristically(S) :-
   find_randomly(S,8000,_).

%%%%%%%%%%%%%%%%%%%
% RANDOM SOLUTION %
%%%%%%%%%%%%%%%%%%%
randomSolution(S) :-
   buildEmptySolution(S1),
   listTasks(ListTask),
   listCores(ListCore),
   length(ListCore, Nb),
   N is Nb+1,
   buildRandomSolution(S1, ListCore, ListTask, N, S), !.

buildRandomSolution(Sol, _, [], _, Sol).
buildRandomSolution(EmptySol, ListCore, [Htask|Ttask], N, S) :-
   random(1, N, R),
   nth1(R, ListCore, Core),
   buildRandomSolution(EmptySol, ListCore, Ttask, N, S1),
   assign(Htask, Core, S1, S).

find_randomly(Sol, N, Cost) :-
   find_bestRandomSol(Sol, N, Cost).
%  saveSolution('solutions.txt', Sol, Cost, random).

find_bestRandomSol(Sol, 1, Cost) :- 
   randomSolution(Sol),
   execution_time(Sol, Cost), !.
find_bestRandomSol(Sol, N, Cost) :-
   randomSolution(S1),
   execution_time(S1, Cost1),
   Nb is N-1,
   find_bestRandomSol(S2, Nb, Cost2),
   bestRandomSolution(S1, S2, Cost1, Cost2, Sol, Cost).

bestRandomSolution(S1, _, Cost1, Cost2, S1, Cost1) :- Cost1 < Cost2, !.
bestRandomSolution(_, S2, _, Cost2, S2, Cost2).

%%%%%%%%%%%%%%%%%%%%%%%%%
% BUILD GREEDY SOLUTION %
%%%%%%%%%%%%%%%%%%%%%%%%%
buildGreedy(Solution, Cost) :-
   listTasks(ListTask),
   listCores(ListCore),
   buildEmptySolution(S),
   computeCostForEachTask(ListCore, ListTask, S, Solution, Cost),
   retractall(buildCost(_,_,_)),
   saveSolution('solutions.txt', Solution, Cost, greedy), !.

computeCostForEachTask(_,[],S,S,0).
computeCostForEachTask(ListCore, [Htask|Ttask], InitSol, Sol, TotCost) :-
   computeCostForEachCore(ListCore, Htask, Cost, Core),
   computeCostForEachTask(ListCore, Ttask, InitSol, S, Cost1),
   TotCost is Cost + Cost1,
   assign(Htask, Core, S, Sol).

:- dynamic buildCost/3.
computeCostForEachCore([],_,-1,_).
computeCostForEachCore([Hcore|Tcore], Task, TotalCost, Core) :-
   listDirectDependentTask(Task, ListDependentTask),
   process_cost(Task, Hcore, Cost),
   getDependentTaskCost(Hcore, Task, ListDependentTask, ListDependCost),
   min_list(ListDependCost, MinCost),
   TotCost is Cost + MinCost, 
   assert(buildCost(Task, Hcore, TotCost)), 
   computeCostForEachCore(Tcore, Task, TotCost1, Core1),
   checkCost(Task, TotCost1, Core1, TotCost, Hcore, TotalCost, Core).
computeCostForEachCore([Hcore|Tcore], Task, TotalCost, Core) :- % called if the task doesn't depend on other tasks.
   process_cost(Task, Hcore, TotCost),
   assert(buildCost(Task, Hcore, TotCost)), 
   computeCostForEachCore(Tcore, Task, TotCost1, Core1),
   checkCost(Task, TotCost1, Core1, TotCost, Hcore, TotalCost, Core).

checkCost(_, -1, _, TotCost, Hcore, TotCost, Hcore).
checkCost(Task, TotCost1, Core1, TotCost, Hcore, TotCost1, Core1) :-
   TotCost > TotCost1,
   retract(buildCost(Task, Hcore, TotCost)).
checkCost(Task, TotCost1, Core1, TotCost, Hcore, TotCost, Hcore) :-
   TotCost =< TotCost1,
   retract(buildCost(Task, Core1, TotCost1)).

getDependentTaskCost(_, _, [], []).
getDependentTaskCost(Core, Task, [HtaskD|TtaskD], [HcostD|TcostD]) :-
   buildCost(Task, CoreD, CostD),
   communication_cost(CoreD, Core, Task, HtaskD, CommCost),
   HcostD is CommCost + CostD,
   getDependentTaskCost(Core, Task, TtaskD, TcostD).


%%%%%%%%%%%%%%%%%%%%%%%
% SUBOPTIMAL SOLUTION %
%%%%%%%%%%%%%%%%%%%%%%%
% Note: For now, it works for the instances where there is no dependencies.
% it will be a little bit more complicated with the dependencies (need to stop getCost() at a certain point).
find_subOptimal(Sol, N) :-
   integer(N), !,
   not(dependency),
   listTasks(ListTask),
   find_subOptimal(ListTask, Sol, N), 
   saveSolution('solutions.txt', Sol, null, subOptimal), !.
find_subOptimal(_, L) :-
   assert(best(nil,100000)),
   getSolution(S, L),
   execution_time(S,ET),
   update_best(S,ET),
   fail.
find_subOptimal(S, _) :-
   best(S,_), retractall(best(_,_)), !.

find_subOptimal([], [], _).
find_subOptimal(ListTask, S, N) :-
   length(ListTask, Len),
   (Len >= N -> length(L, N) ;
      otherwise -> length(L, Len)),
   append(L, RestList, ListTask), !,
   find_subOptimal(S1, L),
   find_subOptimal(RestList, S2, N),
   mergeSorted(S1,S2,S).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% pretty_print(S) %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pretty_print/1
% @params: Solution - the solution to print.
pretty_print(S) :-
   write('The solution is: '), nl, 
   printSol(S),
   write('Total Execution Time: '),
   execution_time(S,ET),
   write(ET),
   write(' ms.'), nl, !.

printSol(solution([])).
printSol(solution([schedule(C,Task)|Rest])) :-
   write('On the core '),
   write(C),
   write(' : '),
   write(Task),
   nl,
   printSol(solution(Rest)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% speedup(S,SpeedUp) %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% sumTaskTime/3
% @params: Core, ListTask.
% @return: TotTime - the time of running all the tasks defined in ListTask sequentially on the given Core.
sumTaskTime(_, [], 0).
sumTaskTime(Core, [Htask|Ttask], TotTime) :-
   process_cost(Htask,Core,Time1), 
   sumTaskTime(Core,Ttask,Time2),
   TotTime is Time1+Time2, !.

% checkCore/3
% @params: ListCore, ListTask.
% @return: Time - the minimum time to run all the tasks (that are in ListTask) sequentially on one of the cores.
checkCore([],_,0).
checkCore([HCore|TCore], ListTask, Time) :- 
   sumTaskTime(HCore, ListTask, T1), 
   checkCore(TCore, ListTask, T2), 
   getTime(T1,T2,Time).

% getTime/3
% @params: T1, T2.
% return: Time - min(T1,T2).
getTime(T1,T2,Time):- T2 > 0, Time is min(T1,T2).
getTime(T1,_,T1).

% speedup/2
% @params: Solution.
% @return: SpeedUp - The speedup of the given solution.
speedup(solution([]), 0).
speedup(S,SpeedUp) :- 
   execution_time(S,ET), 
   listTasks(ListTask),
   listCores(ListCore),
   checkCore(ListCore, ListTask, ET1), 
   SpeedUp is ET1/ET, !.
