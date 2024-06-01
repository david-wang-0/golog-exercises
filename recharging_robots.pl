consult('golog_interpreter.pl').

primitive_action(move(Robot, From, To)).
primitive_action(recharge(RFrom, RTo, Loc, FPreFrom, FPostFrom, FPreTo, FPostTo)).
primitive_action(stop_and_guard(Robot, Location)).
primitive_action(verify_guard_config(Config)).

poss(move(Robot, From, To),S) :- 
    at(Robot, From, S), 
    not(stopped(Robot)),
    battery(Robot, FPre),
    (connected(From, To); connected(To, From))
    .

poss(recharge(RFrom, RTo, Loc, FPreFrom, FPostFrom, FPreTo, FPostTo), S) :-
    not(RFrom = RTo),
    at(RFrom, Loc, S),
    at(RTo, Loc, S),
    battery(RFrom, FPreFrom, S),
    battery(RTo, PreTo, S)
    .

poss(stop_and_guard(R, L), S) :-
    \+ (stopped(R, S)),
    at(R, L, S).

% The following has been created using the modified LLoyd-Topor Transformations by hand.
% Whether this is correct is not immediately apparent.
poss(verify_guard_config(C), S) :- 
    not (
        location(L),
        guard_config(C, L),
        not(guarded(L, S))
    ).

at(Robot, Loc, do(A, S)) :- 
    A = move(Robot, From, Loc);
    (not(A = move(Robot, Y)), at(Robot, Loc, S)).

battery(Robot, FPost, do(A, S)) :-
    (
        (
            A = move(Robot, X, Y), 
            succ(FPost, FPre), 
            battery(Robot, FPre, S)
        );
        (
            A = recharge(RFrom, RTo, Loc, FPreFrom, FPostFrom, FPreTo, FPostTo), 
            succ(FPostFrom, FPreFrom),
            FPostFrom = FPost,
            RFrom = Robot
        );
        (
            A = recharge(RFrom, RTo, Loc, FPreFrom, FPostFrom, FPreTo, FPostTo), 
            succ(FPostTo, FPreTo),
            FPostTo = FPost,
            RTo = Robot
        )
    );
    (
        not(A = move(Robot, X, Y)),
        not(A = recharge(Robot, RTo, Loc, FPreFrom, FPostFrom, FPreTo, FPostTo)),
        not(A = recharge(RFrom, Robot, Loc, FPreFrom, FPostFrom, FPreTo, FPostTo)),
        battery(Robot, FPost, S)
    ).

total_cost(Level, do(A, S)) :-
    (
        (
            A = move(Robot, X, Y), 
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
        not(A = move(Robot, X, Y)), 
        not(A = recharge(RFrom, RTo, Loc, FPreFrom, FPostFrom, FPreTo, FPostTo))
        total_cost(Level, S)
    ).

stopped(R, do(A, S)) :-
    (
        A = stop_and_guard(R, L)
    );
    (
        \+ (A = verify_guard_config(C))
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
        \+ (A = verify_guard_config(Config)),
        config_fulfilled(Config, S)
    ).



restoreSitArg(at(R, L), S, at(R, L, S)).
restoreSitArg(battery(R, B), S, battery(R, B, S)).
restoreSitArg(total_cost(L), S, total_cost(L, S)).
restoreSitArg(stopped(R), S, stopped(R, S)).
restoreSitArg(guarded(L), S, guarded(L, S)).
restoreSitArg(config_fulfilled(C), S, config_fulfilled(C, S)).
