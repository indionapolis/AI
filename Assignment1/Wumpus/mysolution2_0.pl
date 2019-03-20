% usage: "run."
% configure the world in create_game_world/0

% constats
moves_limit(40).
planning_limit(20).

% reset predicates
:- abolish(wall_location/1).
:- abolish(wumpus_location/1).
:- abolish(gold_location/1).
:- abolish(pit_location/1).

% declaring dynamic methods with their arity
:- dynamic ([
  wall_location/1,
  wumpus_location/1,
  gold_location/1,
  pit_location/1
]).



% main entry point
run :-
    write('Creating the world'),
    create_game_world,
    writeln('  ...  Done'),
    writeln('Starting the game'),
    timer_wrapper(main_loop(0,s0)).

create_game_world :-
    reset_game_world,
    set_walls,
    set_world.

reset_game_world :-
    retractall(wall_location(_)),
    retractall(gold_location(_)),
    retractall(wumpus_location(_)),
    retractall(pit_location(_)).

% set wall layout 4 by 4 sample
set_walls :-
    asserta(wall_location(position(0,0))),
    asserta(wall_location(position(0,1))),
    asserta(wall_location(position(0,2))),
    asserta(wall_location(position(0,3))),
    asserta(wall_location(position(0,4))),
    asserta(wall_location(position(0,5))),
    asserta(wall_location(position(1,5))),
    asserta(wall_location(position(2,5))),
    asserta(wall_location(position(3,5))),
    asserta(wall_location(position(4,5))),
    asserta(wall_location(position(5,5))),
    asserta(wall_location(position(5,4))),
    asserta(wall_location(position(5,3))),
    asserta(wall_location(position(5,2))),
    asserta(wall_location(position(5,1))),
    asserta(wall_location(position(5,0))),
    asserta(wall_location(position(4,0))),
    asserta(wall_location(position(3,0))),
    asserta(wall_location(position(2,0))),
    asserta(wall_location(position(1,0))).

% set main parts of game world
set_world :-
    asserta(gold_location(position(4,4))),
    asserta(wumpus_location(position(3,3))).


% stop loop if goes beyond limit
main_loop(T,_) :-
    moves_limit(Limit),
    T >= Limit,
    write('Reached moves limit'), !, false.

% main loop
main_loop(T,S0) :-
    % current agent position
    agent(position(X,Y),D,S0),
    format('agent at [~d,~d], ~w, ', [X,Y,D]),!,

    % get action from ask_knowledge_base for S0 state
    ask_knowledge_base(S0,Action),
    format('wants ~w, ', [Action]),

    % perform actions
    do_action(Action,S,S0),
    write('does '),
    get_action_list(S,S0,[],L),
    write_list(L),    % write list of actions
    ((predict_wumpus(RW,S),\+was_scream(S)) -> format('agent predicts the wumpus is at ~w',RW);true),
    format('~n'),

    NT is T+1, % update number of staps

    %The following are needed to check if agent climbed out of the cave
    do(A,_) = S, %Get last action done
    agent(RN,_,S), %Get agents position now
    agent(R0,_,s0), %Get Cave entry

    %If agent climbed out or died, endloop. If not, run next loop.
    (   ((A = climb, RN = R0)  ; \+agent_alive(S)) -> brake_loop(S,A)
    ;   (!,main_loop(NT,S))
    ),!.

% print result of the game
brake_loop(S,A) :-
    format('~n~n| Result:~n|~n'),
    (has_gold(S) -> write('> agent got the gold'); write('> agent couldnt find the gold')),
    ( \+agent_alive(S) -> write(' and died') ; write(' and left the cave')),

    % calculate scoer
    count_actions(S,s0,N),
    ((has_gold(S),A = climb) -> GoldPoints is 1000 ; GoldPoints is 0),
    (\+agent_alive(S) -> DeathPoints is -1000 ; DeathPoints is 0),
    (has_arrow(S) ->ArrowPoints is 0 ; ArrowPoints is -10),
    Score is GoldPoints + DeathPoints + ArrowPoints - N,
    format('~n|~n> final score: ~d',Score).






% agent state
agent(position(1,1),e,s0). % start position

agent(R,D, do(A,S)) :-
    agent(R0,D0,S), % get agent state
    ( % if action changes agent position
        (A = left, R = R0, ((D0 = n, D = w);(D0 = e, D = n);(D0 = s, D = e);(D0 = w, D = s))); %turn left
        (A = right, R = R0, ((D0 = n, D = e);(D0 = e, D = s);(D0 = s, D = w);(D0 = w, D = n))); %turn right
        (A = forward, D = D0, get_forward_position(R0,D0,RN), !, (wall_location(RN) -> R = R0 ; R = RN)) %go forward
    );
    ( % no actions that change agent position
        agent(R,D,S), % agent position same as before
        \+A = left,
        \+A = right,
        \+A = forward
    ).


agent_alive(s0).
agent_alive(do(A,S)) :- agent_alive(S),
    (agent(R,_,do(A,S)), \+pit_location(R), (wumpus_alive(do(A,S)) -> \+wumpus_location(R) ; true)).


wumpus_alive(s0).
wumpus_alive(do(A,S)) :- wumpus_alive(S), % if he was dead in previous situations, remain dead
    ((A = shoot, has_arrow(S), agent(R0,D0,S), wumpus_location(RW), is_facing(R0,D0,RW)) -> false ; true). % if he has shot, he dies

% one arrow that can kill the wumpus
has_arrow(s0). % initiali has arrow
has_arrow(do(A,S)) :- % stays false after using shoot action
    (   A = shoot -> false ; has_arrow(S)).

% whether agent has the gold
has_gold(s0) :- false. % starts without gold
has_gold(do(A,S)) :-
    (   has_gold(S) %If agent had the gold before, still has now
    ;   (A = grab, gold_location(R), agent(R,_,S)) %If agent use grab at the same position as gold, pick it up
    ).

% learn knowledge base
learn_bump(do(A,S)) :- A = forward, agent(R,D,S), agent(R,D,do(A,S)). %If uses forward but remains the same, hit a wall, hence feels bump
learn_glitter(S) :- agent(R,_,S), gold_location(R). %If at the same position as gold, perceive Glitter
learn_breeze(S) :- agent(R,_,S), pit_location(RP), is_adjacent(R,RP). %If adjacent to a Pit, perceibe breeze
learn_tench(S) :- agent(R,_,S), wumpus_location(RP), is_adjacent(R,RP). %If adjacent to a Wumpus (Dead or Alive), perceive Stench
learm_scream(do(A,S)) :- wumpus_alive(S), \+wumpus_alive(do(A,S)). %If Wumpus was alive and now is dead, perceive Scream


wall(R,do(A,S)) :- (learn_bump(do(A,S)), agent(Rh,Dh,do(A,S)), get_forward_position(Rh,Dh,R)) ; wall(R,S). %Agent remembers bumping into walls

visited(position(1,1),s0).
visited(R,do(A,S)) :- agent(R,_,do(A,S)) ; visited(R,S). %Agente remembers all positions he has visited

gold(R,do(A,S)) :- (learn_glitter(do(A,S)),agent(R,_,do(A,S))) ; gold(R,S). %Once gold is found, remember where it was

breeze(R,s0) :- learn_breeze(s0),agent(R,_,s0).
breeze(R,do(A,S)) :- (learn_breeze(do(A,S)),agent(R,_,do(A,S))) ; breeze(R,S). %Remember where breeze was perceived

stench(R,s0) :- learn_tench(s0),agent(R,_,s0).
stench(R,do(A,S)) :- (learn_tench(do(A,S)),agent(R,_,do(A,S))) ; stench(R,S). %Remember where stench was perceived

was_scream(s0) :- false.
was_scream(do(A,S)) :- learm_scream(do(A,S)) ; was_scream(S). %Represent actual knowledge of hearing the wumpus scream (and hopefully die)


% ok if there is no chance it has a pit or an alive wumpus
check_position(R,S) :- \+possible_pit(R,S), (\+was_scream(S) -> \+possible_wumpus(R,S);true).

% check possibility of pit
possible_pit(R,S) :- \+visited(R,S), get_adjacent(R,LA), remove_unvisited(LA,S,LT), (LT = []; check_breeze_list(LT,S)).
check_breeze_list([],_).
check_breeze_list([H|T],S) :- check_breeze_list(T,S), breeze(H,S).

% One can only be certain of a pits position if there is a position with
% breeze where 3 adjacent positions were visited and don't have a pit. The
% pit is in the fourth position certainly.
certain_pit(RP,S) :-
    get_adjacent(RP,LA),
    remove_unvisited(LA,S,LT),
    check_pit(RP,LT,S).

check_pit(_,[],_) :- false.
check_pit(RP,[H|T],S) :-
    breeze(H,S),
    (
        (
        get_adjacent(H,LA),
        remove_visited(LA,S,LT),
        remove_wall(LT,S,LT2),
        LT2 = [RP]
        )
        ; check_pit(RP,T,S)
    ).

% Evaluates possibility of Wumpus in a certain position. Checks if all
% adjacent positions that were visited had stench
possible_wumpus(R,S) :-
    (predict_wumpus(RW,S) -> R = RW %a certain Wumpus is also a possible Wumpus
    ;   (\+visited(R,S), get_adjacent(R,LA), remove_unvisited(LA,S,LT), (LT = []; check_stench_list(LT,S)))).
check_stench_list([],_).
check_stench_list([H|T],S) :- check_stench_list(T,S), stench(H,S).

% More easily than checking for pits, as we know there is only one
% Wumpus, one can mix and match adjacent positions of two or more positions with
% stench. If only one position that wasnt visited remains, the Wumpus must
% be there.
predict_wumpus(RW,do(A,S)) :-
     predict_wumpus(RW,S); %Check first Wumpus certainty before, because if he is killed and the agent visits the space the rest of the code returns false
     (
     setof(R,stench(R,do(A,S)),[H|T]), %H is going to be used as reference, and T will help
     get_adjacent(H,LA),
     remove_visited(LA,S,LAT),
     remove_unadjacent(LAT,T,LT),
     LT = [RW] %If only one position is reached, that is where the wumpus is
     ).


ask_knowledge_base(S,H) :-
    get_explorable_positions(S,L), %Get entire list of all positions adjacent to positions that were visited
    (\+L=[] -> get_best_position(S,L,P,R) ; P = 5000), %Only run ranking of positions if there are positions to rank
    (   has_gold(S) -> H = exit_cave %If agent has gold, proceed to exit
    ;   (\+has_gold(S), gold(_R,S)) -> H = grab_gold %If doesn't have gold but knows where it is, go get it
    ;   (predict_wumpus(_RW,S),has_arrow(S),\+was_scream(S)) -> H = shoot_wumpus %If is certain of where the Wumpus is, has arrow and Wumpus is alive, shoot him
    ;   P < 100 -> H = move(R) %Only move if best position to explore is not dangerous
    ;   H = exit_cave %If no positions to explore, exit cave
    ).

get_best_position(S,L,P,R) :-
    rank_positions(L,S,RL),
    sort(RL,SRL),
    [Head|_] = SRL,
    rr(P,R) = Head.

% Ranks positions by number of actions to explore and danger levels
rank_positions([],_,[]).
rank_positions([H|T],S,RL) :-
    rank_positions(T,S,LT),
    %Count actions
    move(H,ST,S),
    count_actions(ST,S,NActions),
    %Check breeze and stench
    (check_position(H,S) -> DangerPoints = 0; DangerPoints = 100),
    %Check certain Pit and Wumpus
    (certain_pit(H,S) -> CertainPitPoints = 1000; CertainPitPoints = 0),
    ((\+was_scream(S), predict_wumpus(H,S)) -> CertainWumpusPoints = 1000; CertainWumpusPoints = 0),
    Total is NActions + DangerPoints + CertainPitPoints + CertainWumpusPoints, %Saves rank for each position
    RR = rr(Total,H),
    add(RR,LT,RL).


% Preconditions for primitive actions. Define whether an action can be
% taken at each situation.
poss(forward,S) :-
    agent(R,D,S),
    get_forward_position(R,D,RF),
    check_position(RF,S).
poss(left,s0).
poss(left,do(A,_S)) :- \+A = right. %Limit redundant turning
poss(right,s0).
poss(right,do(A,_S)) :- \+A = left. %Limit redundant turning


%legal(S,S0) reads: If S0 is legal, return whether S is legal
legal(S,S). %If S is legal, S is legal
legal(do(A,S),S0):-
    planning_limit(Max), %Get maximum allowed number of actions
    legal(S,S0), %Tries to find legal actions, starting from provided situation S0
    count_actions(S,S0,N), %Count number of actions from S0 to S
    (N > Max -> (!, write('REACHED MAX NUMBER OF ACTIONS PLANNED'),false) ; true), %If too many actions are being taken, probably there is no solution, hence return false
    poss(A,S). %Check which actions are allowed at S

% Movement planner - The last forward action is forced, even if it
% doesn't result in the agent's movement. That must be done because if
% the agent hits a wall it won't know it hasn't moved until it receive
% a bump as a perception.
% move(R,S,S0) returns a plan of a sequence of movement actions that
% make the agent in situation S0 move to R. S is returned as the
% resulting situation.
move(Rm, S0, S0) :- agent(Rm,_,S0). %Moving to where the agent is returns no actions
move(Rm, do(forward,S), S0) :- legal(S,S0),agent(R,D,S),is_adjacent(R,Rm),is_facing(R,D,Rm),!. %Reads: Which is a situation S supposing S0 is legal, where the agent is at R?

doFace(Rm, S, S0) :- legal(S,S0),agent(R,D,S),is_facing(R,D,Rm),!. %Similar to move, but only faces de target


% After the ask_knowledge_base defines a major action, this clause will convert
% that action to a situation with planning. Passing this situation to
% the next loops counts as acting.
do_action(H,S,S0) :-
    (   H = move(R) -> move(R,S,S0) %Move
    ;   H = grab_gold -> (gold(R,S0), move(R,SI,S0), S = do(grab, SI)) %Move and then grab
    ;   H = shoot_wumpus -> (predict_wumpus(RW,S0), doFace(RW,SI,S0), S = do(shoot, SI)) %Face Wumpus and shoot
    ;   H = exit_cave -> (agent(R0,_,s0), move(R0,SI,S0), S = do(climb, SI)) %Moves to entry and climbs
    ;   H = climb -> S = do(climb, S0) %Climb
    ;   H = forward -> S = do(forward, S0) %Does Forward
    ;   H = left -> S = do(left, S0) %Does Left
    ;   H = right -> S = do(right, S0) %Does Right
    ;   H = grab -> S = do(grab, S0) %Does Grab
    ;   H = shoot -> S = do(shoot, S0) %Does Shoot
    ).



%These are helper functions that make the programming above easier
add(E,L,[E|L]). %Adds element to list

list_len([],0). %Counts number of elements in list
list_len([_|Tail], N) :- list_len(Tail, N1), N is N1 + 1.

remove_unvisited([],_,[]). %Removes positions that weren't visited from list of positions
remove_unvisited([H|T],S,LT) :- remove_unvisited(T,S,L), (visited(H,S) -> append([H],L,LT); LT = L).
remove_visited([],_,[]). %Removes positions that were visited from list of positions
remove_visited([H|T],S,LT) :- remove_visited(T,S,L), (visited(H,S) -> LT = L; append([H],L,LT)).
remove_wall([],_,[]). %Removes positions that have been confirmed as walls from list of positions
remove_wall([H|T],S,LT) :- remove_wall(T,S,L), (wall(H,S) -> LT = L; append([H],L,LT)).
remove_unadjacent([],_,[]). %used as remove_unadjacent(L,T,LT)
remove_unadjacent(_,[],[]). %Removes positions from List L that are no adjacent to any position in list T
remove_unadjacent([LAH|LAT],[TH|TT],LT) :-
    remove_unadjacent([LAH],TT,LT1),
    remove_unadjacent(LAT,[TH|TT],LT2),
    append(LT1,LT2,LT3),
    (is_adjacent(LAH,TH) -> append([LAH],LT3,LT) ; LT = LT3).

% converts plan (Actions from one situation to another) to Action list
get_action_list(S,S,ACC,ACC).
get_action_list(do(A,S1),S0,ACC,X) :- get_action_list(S1,S0,[A|ACC],X).

% prints list
write_list([]).
write_list([A|B]) :-
    format('~w, ', A),
    write_list(B).

% returns position in front of another in a certain direction
get_forward_position(position(X0,Y0),D0,position(XN,YN)) :-
    (D0 = n, XN is X0, YN is Y0+1);
    (D0 = e, XN is X0+1, YN is Y0);
    (D0 = s, XN is X0, YN is Y0-1);
    (D0 = w, XN is X0-1, YN is Y0).

% checks if one position is adjacent to another position
is_adjacent(position(X,Y),position(XT,YT)) :-
    (X =:= XT, Y =:= YT+1);
    (X =:= XT, Y =:= YT-1);
    (X =:= XT+1, Y =:= YT);
    (X =:= XT-1, Y =:= YT).

% checks if a agent in position R, looking to Direction D is facing position RT
is_facing(position(X,Y),D,position(XT,YT)) :-
    (D = n, X =:= XT, YT > Y);
    (D = s, X =:= XT, YT < Y);
    (D = e, Y =:= YT, XT > X);
    (D = w, Y =:= YT, XT < X).

% returns list of all adjacent positions
get_adjacent(position(X,Y),L) :-
    XL is X-1,
    XR is X+1,
    YD is Y-1,
    YU is Y+1,
    append([position(XL,Y), position(XR,Y), position(X,YU), position(X,YD)],[],L).

% The following functions are used to get a list of explorable positions.
% Those are positions adjacent to positions that were already visited. All positions
% on the border of what has been explored. In a certain situation S a
% list L is returned with all possible positions.
get_explorable_positions(S,L) :- get_explorable_positions(S,S,L). %Simplifies call
get_explorable_positions(S,s0,L) :-
    agent(R,_,s0),
    get_adjacent(R,LA),
    append_with_explorable_check(LA,S,[],L).
get_explorable_positions(S,do(A,S0),L) :-
    get_explorable_positions(S,S0,L0),
    agent(R,_,do(A,S0)),
    get_adjacent(R,LA),
    append_with_explorable_check(LA,S,L0,L).

append_with_explorable_check([],_,L2,L2).
append_with_explorable_check([H|T],S,L2,L) :-
    append_with_explorable_check(T,S,L2,LT),
    (   is_explorable(H,S,LT) -> L = [H|LT] ; L = LT).

is_explorable(R,S,L) :- \+member(R,L), \+wall(R,S), \+visited(R,S).

%Counts number of actions between two situations
count_actions(s0,s0,0).
count_actions(S,S,0).
count_actions(do(_A,S),S0,N) :- %Count number of actions between two situations
    count_actions(S,S0,N0),
    N is N0+1.

% timer wrapper
timer_wrapper(F,Time) :-
    statistics(runtime,[Start_time|_]),
    F,
    statistics(runtime,[End_time|_]),
    Time is End_time - Start_time.

timer_wrapper(F) :-
    timer_wrapper(F,T),
    format('~n|~n> time taken: ~d ms~n|~n',T).
