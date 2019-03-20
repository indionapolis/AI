% to start: load file in run time and enter "run."

% knowledge_base - knolage base

% declaring dynamic methods with their arity
:- dynamic ([
	     agent_location/1,
	     gold_location/1,
	     pit_location/1,

	     time_taken/1,
	     score/1,
	     visited/1,
	     visited_cells/1,

	     world_size/1,
	     wumpus_location/1,

       isPit/2,
       isWumpus/2,
       isGold/2
	    ]).


% main entry point
run :-
    writeln('initializing the game world'),
    init_all,
    writeln('start the game'),
    % start from position [1, 1]
    try_new_position([[1,1]]).


% initializing the game world
init_all :-
    init_game,
    init_fild,
    init_agent,
    init_wumpus.

% set all parameters to initial values
init_game :-
    % created as a dynamic predicate
    retractall(time_taken(_)),
    retractall(visited_cells(_)),
    retractall(visited(_)),
    retractall(score(_)),

    % set values
    assertz(time_taken(0)),
    assertz(visited_cells([[1,1]])),
    assertz(visited(1)),
    assertz(score(0)),

    retractall(isWumpus(_,_)),
    retractall(isGold(_,_)).

% initiate field layout
init_fild :-
    % created as a dynamic predicate
    retractall(world_size(_)),
    retractall(gold_location(_)),
    retractall(pit_location(_)),

    % add fact about world size
    assertz(world_size(4)),

    % add fact about gold location
    assertz(gold_location([3,2])),

    % add facts about pit location
    assertz(pit_location([4,4])),
    assertz(pit_location([3,3])),
    assertz(pit_location([1,3])).

% create agent object
init_agent :-
    retractall(agent_location(_)),
    % initial position on the bord
    assertz(agent_location([1,1])).


% create wumpus object
init_wumpus :-
    retractall(wumpus_location(_)),
    % position of wumpus
    assertz(wumpus_location([4,1])).



% look for new position to go
try_new_position(VisitedList) :-
    look_around(Perception),
    agent_location(AL),

    format('agent in ~p, seeing: ~p~n', [AL,Perception]),

    update_knowledge_base(Perception),

    ask_knowledge_base(VisitedList, Action),

    format('agent going to: ~p~n', [Action]),

    update_time,
    update_score,

    agent_location(A_L),
    NewVisitedList = [A_L|VisitedList],

    check_new_position(NewVisitedList).

% see what around agent
look_around([Stench,Bleeze,Glitter]) :-
		smelly(Stench),
		bleezy(Bleeze),
		glittering(Glitter).



bleezy(breeze) :-
    agent_location(AL),
    is_bleezy(AL).
bleezy(no_breeze).

smelly(smelly) :-
    agent_location(AL),
    is_smelly(AL).
smelly(no_smelly).

glittering(glittering) :-
    agent_location(AL),
    is_glittering(AL).
glittering(no_glittering).

% if wumpus is nearly the agent percives smell
is_smelly(AL) :-
    wumpus_location(WL),
    adjacent(AL, WL).

% if a pit is nearly the agent percives breaze
is_bleezy(AL) :-
    pit_location(PL),
    adjacent(AL, PL).

% if the gold at the spot where the agent is
is_glittering([X1, Y1]):-
    gold_location([X2, Y2]),
    X1 = X2,
    Y1 = Y2.


% add new information about environment
update_knowledge_base([Stench,Bleeze,Glitter]) :-
    add_wumpus_knowledge_base(Stench),
    add_pit_knowledge_base(Bleeze),
    add_gold_knowledge_base(Glitter).

% add info that there is no wumpus around
add_wumpus_knowledge_base(no_smelly) :-
    agent_location([X,Y]),

    Z1 is Y+1, assume_wumpus(no,[X,Z1]),
    Z2 is Y-1, assume_wumpus(no,[X,Z2]),
    Z3 is X+1, assume_wumpus(no,[Z3,Y]),
    Z4 is X-1, assume_wumpus(no,[Z4,Y]).

% add info that there is might be wumpus around
add_wumpus_knowledge_base(smelly) :-
    agent_location([X,Y]),

    Z1 is Y+1, assume_wumpus(yes,[X,Z1]),
    Z2 is Y-1, assume_wumpus(yes,[X,Z2]),
    Z3 is X+1, assume_wumpus(yes,[Z3,Y]),
    Z4 is X-1, assume_wumpus(yes,[Z4,Y]).

% add info that there is no pit around
add_pit_knowledge_base(no_breeze) :-
    agent_location([X,Y]),

    Z1 is Y+1, assume_pit(no,[X,Z1]),
    Z2 is Y-1, assume_pit(no,[X,Z2]),
    Z3 is X+1, assume_pit(no,[Z3,Y]),
    Z4 is X-1, assume_pit(no,[Z4,Y]).

% add info that there might be a pit around
add_pit_knowledge_base(breeze) :-
    agent_location([X,Y]),

    Z1 is Y+1, assume_pit(yes,[X,Z1]),
    Z2 is Y-1, assume_pit(yes,[X,Z2]),
    Z3 is X+1, assume_pit(yes,[Z3,Y]),
    Z4 is X-1, assume_pit(yes,[Z4,Y]).

% add info that there is no gold here
add_gold_knowledge_base(no_glittering) :-
    agent_location(AL),
    assume_gold(no, AL).

% add info that there is gold here
add_gold_knowledge_base(glittering) :-
    gold_location([X1,Y1]),
    agent_location([X2,Y2]),
    X1 = X2, Y1 = Y2,
    assume_gold(yes, [X1,Y1]).

assume_wumpus(no, Location) :-
    retractall(isWumpus(_, Location)),
    assert(isWumpus(no, Location)),
    format('knowledge_base learn ~p - no wumpus there~n', [Location]).

assume_wumpus(yes, Location) :-
		isWumpus(Info, Location);
		(
				Info = no -> true;
				retractall(isWumpus(_, Location)),
		    assert(isWumpus(yes, Location)),
		    format('knowledge_base learn ~p - myght be the wumpus there~n', [Location])
		).


assume_pit(no, Location) :-
    retractall(isPit(_, Location)),
    assert(isPit(no, Location)),
    format('knowledge_base learn ~p - no pit there~n', [Location]).

assume_pit(yes, Location) :-
    retractall(isPit(_, Location)),
    assert(isPit(yes, Location)),
    format('knowledge_base learn ~p - myght be a pit there~n', [Location]).

assume_gold(no, Location) :-
    retractall(isGold(_, Location)),
    assert(isGold(no, Location)),
    format('knowledge_base learn ~p - there\'s no gold here~n', [Location]).

assume_gold(yes, Location) :-
    retractall(isGold(_, Location)),
    assert(isGold(yes, Location)),
    format('knowledge_base learn ~p - GOT THE GOLD!!!~n', [Location]).

%
ask_knowledge_base(VisitedList, Action) :-
    isWumpus(no, Location),
    isPit(no, Location),
    permitted(Location),
    not_member(Location, VisitedList),
    update_agent_location(Location),
    Action = Location.



% check state in new position on map
check_new_position(VisitedList) :-
    agent_location(Agent_location),
    gold_location(Gold_location),
    wumpus_location(Wumpus_location),
		pit_location(Pit_location).
    score(Score),
    time_taken(Time),

    (
		% TODO end of the game
		Agent_location = Gold_location -> writeln('agent found gold'), format('\nscore: ~p,~nmoves: ~p', [Score,Time]);
		Agent_location = Wumpus_location -> writeln('agent was eaten by wumpus'), format('\nscore: ~p,~nmoves: ~p', [Score,Time]);
		Agent_location = Pit_location -> writeln('agent was fallen into a pit'), format('\nscore: ~p,~nmoves: ~p', [Score,Time]);
		try_new_position(VisitedList)
    ).





is_pit(no,  X) :-
    \+ pit_location(X).
is_pit(yes, X) :-
    pit_location(X).



standing :-
    wumpus_location(WL),
    gold_location(GL),
    agent_location(AL),

    ( is_pit(yes, AL) -> format('Agent was fallen into a pit!~n', []),
      fail
    ; stnd(AL, GL, WL)
      %\+ pit_location(yes, Al),
    ).

stnd(_, _, _).

stnd(AL, _, AL) :-
    fail.

stnd(AL, AL, _) :-
    true.


adj(X,Y) :-
   world_size(WS),
   ( X is Y+1, Y < WS
   ; X is Y-1, Y-1 > 0
   ).


adjacent([X1, Y1], [X2, Y2]) :-
    ( X1 = X2, adj(Y1, Y2)
    ; Y1 = Y2, adj(X1, X2)
    ).







% check if agent wants to cross the border
permitted([X,Y]) :-
    world_size(WS),
    0 < X, X < WS+1,
    0 < Y, Y < WS+1.


not_member(_, []).
not_member([X,Y], [[U,V]|Ys]) :-
    ( X=U,Y=V -> fail
    ; not_member([X,Y], Ys)
    ).


update_time :-
    time_taken(Current_time),
    New_time is Current_time + 1,
    % reset timer
    retractall(time_taken(_)),
    assert(time_taken(New_time)).

update_score :-
    agent_location(AL),
    gold_location(GL),
    wumpus_location(WL),
    update_score(AL, GL, WL).

update_score(P) :-
    score(S),
    NewScore is S + P,
		% reset timer
    retractall(score(_)),
    assert(score(NewScore)).

% found gold +1000 points
update_score(AL, AL, _) :-
    update_score(1000).

% just move -1 point
update_score(_,_,_) :-
    update_score(-1).

update_agent_location(NewAL) :-
    retractall(agent_location(_)),
    assert(agent_location(NewAL)).
