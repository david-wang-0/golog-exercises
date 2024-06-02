:- ensure_loaded('golog_interpreter_eclipse.pl').

% GOLOG version of 2023 IPC domain recharging robots: https://github.com/ipc2023-classical/domain-recharging-robots
% Also includes p01.prob and p01.plan from here: https://github.com/ipc2023-classical/ipc2023-dataset/tree/main/opt/recharging-robots

:- dynamic(at/3).
:- dynamic(battery/3).
:- dynamic(total_cost/2).
:- dynamic(stopped/2).
:- dynamic(guarded/2).
:- dynamic(config_fulfilled/2).
:- dynamic(badSituation/2).

primitive_action(move(Robot, From, To, FPre, FPost)).
primitive_action(recharge(RFrom, RTo, Loc, FPreFrom, FPostFrom, FPreTo, FPostTo)).
primitive_action(stop_and_guard(Robot, Location)).
primitive_action(verify_guard_config(Config)).

poss(move(Robot, From, To, FPre, FPost), S) :- 
    at(Robot, From, S),
    battery(Robot, FPre, S),
    battery_predecessor(FPost, FPre),
    (connected(From, To); connected(To, From))., 
    not(stopped(Robot, S))

poss(recharge(RFrom, RTo, Loc, FPreFrom, FPostFrom, FPreTo, FPostTo), S) :-
    at(RFrom, Loc, S),
    at(RTo, Loc, S),
    battery(RFrom, FPreFrom, S),
    battery(RTo, FPreTo, S),
    battery_predecessor(FPostFrom, FPreFrom),
    battery_predecessor(FPreTo, FPostTo),
    not(RFrom = RTo)
    .
    % This was a pain to debug
    % Checking that the actual entities denoted by terms are not equal must be done using = (semantic equality?)
    % after unification

poss(stop_and_guard(R, L), S) :-
    \+ (stopped(R, S)),
    at(R, L, S).

% The following has been created using the modified LLoyd-Topor Transformations by hand.
% Whether this is correct is not immediately apparent.
poss(verify_guard_config(C), S) :- 
    not (
        guard_config(C, L),
        not(guarded(L, S))
    ).

at(Robot, Loc, do(A, S)) :- 
    A = move(Robot, From, Loc, FPre, FPost);
    (not(A = move(Robot, X, Y, FPre, FPost)), at(Robot, Loc, S)).

battery(Robot, FPost, do(A, S)) :-
    (
        A = move(Robot, X, Y, Z, FPost);
        A = recharge(Robot, RTo, Loc, FPreFrom, FPost, FPreTo, FPostTo);
        A = recharge(RFrom, Robot, Loc, FPreFrom, FPostFrom, FPreTo, FPost)
    );
    (
        not(A = move(Robot, X, Y, Pre, Post)),
        not(A = recharge(Robot, RTo, Loc, FPreFrom, FPostFrom, FPreTo, FPostTo)),
        not(A = recharge(RFrom, Robot, Loc, FPreFrom, FPostFrom, FPreTo, FPostTo)),
        battery(Robot, FPost, S)
    ).

total_cost(Level, do(A, S)) :-
    (
        (
            A = move(Robot, X, Y, From, To), 
            add(Old, C, Level), 
            move_cost(C), 
            total_cost(Old, S)
        );
        (
            A = recharge(RFrom, RTo, Loc, FPreFrom, FPostFrom, FPreTo, FPostTo), 
            add(Old, C, Level),
            recharge_cost(C),
            total_cost(Old, S)
        )
    );
    (
        not(A = move(Robot, X, Y, From, To)), 
        not(A = recharge(RFrom, RTo, Loc, FPreFrom, FPostFrom, FPreTo, FPostTo)),
        total_cost(Level, S)
    ).

stopped(R, do(A, S)) :-
    (
        A = stop_and_guard(R, L)
    );
    (
        not (A = verify_guard_config(C)),
        stopped(R, S)
    ).

guarded(L, do(A, S)) :-
    (
        A = stop_and_guard(R, L);
        (
            A = stop_and_guard(R, L1),
            (connected(L, L1), connected(L1, L))
        )
    );
    (
        \+ (A = verify_guard_config(C))
    ).


config_fulfilled(Config, do(A, S)) :-
    (
        A = verify_guard_config(Config)
    );
    (
        \+ (A = verify_guard_config(C)),
        config_fulfilled(Config, S)
    ).


% It is necessary to restore situation arguments

restoreSitArg(at(R, L), S, at(R, L, S)).
restoreSitArg(battery(R, B), S, battery(R, B, S)).
restoreSitArg(total_cost(L), S, total_cost(L, S)).
restoreSitArg(stopped(R), S, stopped(R, S)).
restoreSitArg(guarded(L), S, guarded(L, S)).
restoreSitArg(config_fulfilled(C), S, config_fulfilled(C, S)).

% Non-fluent predicates

move_cost(1).
recharge_cost(1).
total_cost(0, s0).
connected(location_0003, location_0013).
connected(location_0012, location_0025).
connected(location_0009, location_0017).
connected(location_0009, location_0026).
connected(location_0018, location_0019).
connected(location_0014, location_0015).
connected(location_0000, location_0016).
connected(location_0010, location_0020).
connected(location_0015, location_0016).
connected(location_0002, location_0025).
connected(location_0018, location_0021).
connected(location_0014, location_0017).
connected(location_0009, location_0021).
connected(location_0016, location_0017).
connected(location_0012, location_0013).
connected(location_0003, location_0010).
connected(location_0022, location_0023).
connected(location_0014, location_0028).
connected(location_0016, location_0019).
connected(location_0022, location_0025).
connected(location_0023, location_0024).
connected(location_0003, location_0021).
connected(location_0019, location_0020).
connected(location_0002, location_0013).
connected(location_0024, location_0025).
connected(location_0001, location_0023).
connected(location_0011, location_0025).
connected(location_0003, location_0005).
connected(location_0020, location_0021).
connected(location_0006, location_0015).
connected(location_0026, location_0027).
connected(location_0015, location_0027).
connected(location_0005, location_0013).
connected(location_0017, location_0018).
connected(location_0027, location_0028).
connected(location_0000, location_0011).
connected(location_0010, location_0021).
connected(location_0011, location_0020).
connected(location_0000, location_0020).
connected(location_0026, location_0029).
connected(location_0007, location_0025).
connected(location_0004, location_0008).
connected(location_0017, location_0029).
connected(location_0028, location_0029).
connected(location_0000, location_0022).
connected(location_0011, location_0022).
connected(location_0009, location_0018).
connected(location_0000, location_0015).
connected(location_0002, location_0012).
connected(location_0003, location_0004).
connected(location_0004, location_0021).
connected(location_0001, location_0006).
connected(location_0009, location_0029).
connected(location_0004, location_0005).
connected(location_0000, location_0001).
connected(location_0010, location_0011).
connected(location_0014, location_0027).
connected(location_0002, location_0007).
connected(location_0000, location_0019).
connected(location_0016, location_0018).
connected(location_0007, location_0024).
connected(location_0005, location_0008).
connected(location_0010, location_0013).
connected(location_0011, location_0012).
connected(location_0017, location_0028).
connected(location_0001, location_0022).

battery_predecessor(battery_0000, battery_0001).
battery_predecessor(battery_0001, battery_0002).
battery_predecessor(battery_0003, battery_0004).
battery_predecessor(battery_0004, battery_0005).
battery_predecessor(battery_0002, battery_0003).

% Initial situation

at(robot_00, location_0027, s0).
battery(robot_00, battery_0005, s0).
at(robot_01, location_0027, s0).
battery(robot_01, battery_0000, s0).

% The goal

goal(S) :- 
    at(robot_00, location_0028, S),
    at(robot_01, location_0003, S).

% A bfs planner from Raymond Reiter's book: Knowledge in Action

proc(wspbf(N), 
    ?(initializeSitCount) : 
    ?(initializeCPU) : 
    plans(0,N)
).

proc(plans(M,N),
    ?(M =< N) : (
        actionSequence(M) : 
        ?(goal) :
        ?(reportSuccess) : 
        ?(prettyPrintSituation) #
        pi(m1, 
            ?(m1 is M + 1) :
            ?(reportLevel(m1)) : 
            plans(m1,N)
        )
    )
).

proc(actionSequence(N),
    ?(N = 0) #
    ?(N > 0) : 
    pi(a,
        ?(primitive_action(a)) : 
        a
    ) :
    ?(-badSituation) : 
    ?(incrementSitCount) :
    pi(n1, 
        ?(n1 is N - 1) : 
        actionSequence(n1)
    )).

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

% General filter for more situations (like a PDDL problem)

badSituation(do(A, S)) :- false.

guard_config(C, L) :- false.

% Partial plans and search in one

proc(search(N),
    ?(initializeSitCount) :
    ?(initializeCPU) :
    plans(0, N)
).

proc(p_plan,
    recharge(robot_00, robot_01, location_0027, battery_0005, battery_0004, battery_0000, battery_0001) :
    recharge(robot_00, robot_01, location_0027, battery_0004, battery_0003, battery_0001, battery_0002) :
    recharge(robot_00, robot_01, location_0027, battery_0003, battery_0002, battery_0002, battery_0003) :
    recharge(robot_00, robot_01, location_0027, battery_0002, battery_0001, battery_0003, battery_0004) :
    pi(a,
        ?(primitive_action(a)) :
        a
    ) :
    move(robot_01, location_0027, location_0026, battery_0004, battery_0003) :
    search(6)
).

proc(f_plan,
    recharge(robot_00, robot_01, location_0027, battery_0005, battery_0004, battery_0000, battery_0001) :
    recharge(robot_00, robot_01, location_0027, battery_0004, battery_0003, battery_0001, battery_0002) :
    recharge(robot_00, robot_01, location_0027, battery_0003, battery_0002, battery_0002, battery_0003) :
    recharge(robot_00, robot_01, location_0027, battery_0002, battery_0001, battery_0003, battery_0004) :
    move(robot_00, location_0027, location_0028, battery_0001, battery_0000) :
    move(robot_01, location_0027, location_0026, battery_0004, battery_0003) :
    move(robot_01, location_0026, location_0009, battery_0003, battery_0002) :
    move(robot_01, location_0009, location_0021, battery_0002, battery_0001) :
    move(robot_01, location_0021, location_0003, battery_0001, battery_0000) :
    ?(goal)
).


partial_plan :- do(p_plan, s0, S).

full_plan :- do(f_plan, s0, S).
    
