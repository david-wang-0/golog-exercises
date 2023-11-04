:- ['golog-interpreter'].

/* with some adaptation, the syntax can be shortened by a lot */
/*restoreSitArg(poss(A),S,poss(A,S)).*/

primitive_action(sail(From, To)).
primitive_action(embark(Car, Loc)).
primitive_action(disembark(Car, Loc)).

% if the ferry is somewhere it can sail somewhere else
poss(sail(From,To),S) :- at_ferry(From, S), From =\= To.

% the ferry has either just moved here or the past few
% actions have not affected its location
at_ferry(Loc, do(A, S)) :- 
    A = sail(From, Loc);
    not(A = sail(X, Y)), at_ferry(Loc, S).
% TOOD: read up on the frame problem, because this is not going to work 
% with more complex domains

% we can embark a car when the ferry is 
% at its location and empty
poss(embark(Car, Loc), S) :-
    at(Car, Loc, S),
    at_ferry(Loc, S),
    empty_ferry(S).


% this works, because we cannot embark
% a car if the ferry is not empty
% TODO: read up on properly formulating successor-state
% axioms
on(Car, do(A, S)) :- 
    A = embark(Car, Loc);
    not(A = disembark(Car, L1)), on(Car, S).

% a ferry is empty, if cars have been disembarked
% or no car has been added since it turned empty
empty_ferry(do(A, S)) :- 
    A = disembark(C, L);
    not(A = embark(C, L)), empty_ferry(S).

% it is possible to disembark a car, if there is one
poss(disembark(Car, Loc), S) :- 
    at_ferry(Loc, S),
    on(Car, S).

% the location at a car stay constant, unless
% it has been disembarked somewhere
at(Car, Loc, do(A, S)) :- 
    A = disembark(Car, Loc);
    not(A = embark(Car, L1)), at(Car, Loc, S).

restoreSitArg(done(C, L), S, done(C, L, S)).
restoreSitArg(at(C, L), S, at(C, L, S)).
restoreSitArg(empty_ferry, S, empty_ferry(S)).
restoreSitArg(on(C), S, on(C, S)).
restoreSitArg(at_ferry(L), S, at_ferry(L, S)).

proc(goto(Car), ?(some(loc, at(Car, loc) & at_ferry(loc))) # pi(to, ?(at(Car, to)) : pi(from, ?(at_ferry(from)) : sail(from, to)))).
proc(transport(Car),
    goto(Car):
    pi(from, 
        embark(Car, from):
        pi(to, 
            ?(should_be(Car, to)):
            sail(from, to):
            disembark(Car, to)
        )
    )
).
proc(disembark_all, 
    ?(-some(c, on(c)))# 
    pi(l, 
        ?(at_ferry(l)): 
        pi(c, 
            ?(on(c)): 
            disembark(c,l)
        )
    )
).
proc(loop, 
    star(
        pi(c,
            ?(car(c)):
            transport(c)
        )
    ) :
    ?(at(1, 3)) :
    ?(at(0, 2))
).


at(0, 4, s0).
at(1, 8, s0).

car(0).
car(1).

should_be(0, 2).
should_be(1, 3).

at_ferry(5, s0).
empty_ferry(s0).
