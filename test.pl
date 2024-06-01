% Complete list of rooms

room(green).
room(blue).
room(red).
room(white).

% Complete list of persons

person(jimmy).
person(ricky).
person(sally).
person(cindy).
person(nancy).
person(johnny).

% List of room occupations.
% If this predicate is subject to a "closed world assumption", then
% a room is unoccupied exactly if it doesn't appear in the location/2
% fact list. If our knowledge can be considered incomplete, other rooms
% than those listed below may be occupied.

location(jimmy,red).
location(ricky,blue).
location(cindy,green).

% ---
% Goal based on positive logic
% ---

% If "Room" is nonvar: Is the room "Room" provably occupied?
% If "Room" is var: Is there any room "Room" such that "Room" is provably occupied?

occupied(Room) :-
   assertion((var(Room);room(Room))),
   location(_Person,Room).

% ---
% Goal based on negation as failure.
% The second argument gives the "approach"
% ---

% If "Room" is nonvar, the question is:
%   "Is Room unoccupied (as far as we know)?"
%   or "Is there no evidence that the Room is occupied?"
   
not_occupied(Room,_) :-
   nonvar(Room),
   assertion(room(Room)),
   \+ location(_Person,Room).

% If "Room" is var, the question is:
%    "Is there any Room such that the room is unoccupied?"
%    or "Is there any Room such that there is no info that the room is occupied?"

% Approach 'x': Doesn't work, as the proof procedure answers the question "is there
% no room that is occupied?"

not_occupied(Room,x) :-
   var(Room),
   !,
   \+ location(_Person,Room).

% Approach 'a': You can only ask about a *specific* room. If no specific "Room"
%               has been given, the negated goal is delayed until the "Room" has
%               been instantiated.

not_occupied(Room,a) :-
   var(Room), % this test applies only "now", "Room" can be instantiated later
   !,
   when(
      ground(Room),
      (format("Querying with ~q~n",[Room]), \+ location(_Person,Room))
   ).
  
% Approach 'b': We will enumerate them all. This works nicely for a small number
%               of rooms, less well for infinitely large domain.

not_occupied(Room,b) :-
   var(Room),
   !,
   room(Room),
   \+ location(_Person,Room).



p(b) :- false.
p(a).
q(a).
q(b).

