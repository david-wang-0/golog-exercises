:- ensure_loaded('golog_interpreter_eclipse.pl').

:- discontiguous(at/3).

primitive_action(walk(Robot, To)).

poss(walk(Robot, To),S) :- 
    at(Robot, From, S), 
    (next_to(From, To); next_to(To, From)).


at(Robot, X, do(A, S)) :- 
    (A = walk(Robot, X));
    ((not(A = walk(Robot, Y))), at(Robot, X, S)).

/* We only need this for fluents */
restoreSitArg(at(R, L), S, at(R, L, S)).

robot(r1).

next_to(room1, room2).
next_to(room2, room1).
next_to(room2, room3).
next_to(room3, room2).
next_to(room3, room4).
next_to(room4, room3).

at(r1, room1, s0).



proc(wspbf(N), ?(initializeSitCount) : ?(initializeCPU) : plans(0,N)).
proc(plans(M,N),
    ?(M =< N) :
    (actionSequence(M) : ?(goal) :
    ?(reportSuccess) : ?(prettyPrintSituation) #
    pi(m1, ?(m1 is M + 1) :
    ?(reportLevel(m1)) : plans(m1,N)))).
    proc(actionSequence(N),
    ?(N = 0) #
    ?(N > 0) : pi(a,?(primitive_action(a)) : a) :
    ?(-badSituation) : ?(incrementSitCount) :
    pi(n1, ?(n1 is N - 1) : actionSequence(n1))).

planbf(N) :- do(wspbf(N),s0,S), askForMore.

reportLevel(N) :- write('Starting level '), write(N),
    reportCPUtime, write(' Good situations: '),
    getval(sitCount,C), write(C), nl.

initializeSitCount :- setval(sitCount,0). /* Eclipse Prolog provides global variables. */

initializeCPU :- cputime(T), setval(cpu,T).
incrementSitCount :- incval(sitCount).
 % Increment global variable.

reportCPUtime :- 
    cputime(T), 
    write(' CPU time (sec): '),
    getval(cpu,T2), 
    T1 is T - T2, 
    write(T1).

reportSuccess :- nl, 
    write('Success.'), 
    reportCPUtime,
    write(' Good situations: '), 
    getval(sitCount,C), 
    write(C), 
    nl.

prettyPrintSituation(S) :- 
    makeActionList(S,Alist), 
    nl, write(Alist), nl.
makeActionList(s0,[]).
makeActionList(do(A,S),L) :- makeActionList(S,L1), append(L1,[A],L).

restoreSitArg(prettyPrintSituation,S,prettyPrintSituation(S)).
restoreSitArg(badSituation,S,badSituation(S)).

restoreSitArg(goal,S,goal(S)).
askForMore :- write('More? '), read(n).

badSituation(do(move(X, Y), do(A, S))) :-
    not(poss(move(X, Y), S)).
    
goal(S) :- at(r1, room4, S).
