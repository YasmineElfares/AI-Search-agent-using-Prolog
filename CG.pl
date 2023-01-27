:- include('KB.pl').

%grid(3,3).
%agent_loc(0,1).
%ships_loc([[2,2],[1,2]]).
%station(1,1).
%capacity(1).
%P = passengers with CG


currentSituation(agent_loc(X1,Y1), ships_loc(L1), Passengers1, result(Action, agent_loc(X,Y), ships_loc(L), Passengers)):-
	(Action = down, step(down, X, Y, X1, Y1, Passengers, Passengers1, L, L1));
	(Action = up, step(up, X, Y, X1, Y1, Passengers, Passengers1, L, L1));
	(Action = right, step(right, X, Y, X1, Y1, Passengers, Passengers1, L, L1));
	(Action = left, step(left, X, Y, X1, Y1, Passengers, Passengers1, L, L1));
	(Action = pickup, step(pickup, X, Y, X1, Y1, Passengers, Passengers1, L, L1));
	(Action = drop, step(drop, X, Y, X1, Y1, Passengers, Passengers1, L, L1)).


step(down, X, Y, X1, Y, P, P, Ships, Ships) :- 
	grid(A,B),
	succ(X, X1),	
	between(1, A, X1).
	
step(up, X, Y, X1, Y, P, P, Ships,Ships) :- 
	grid(A,B),
	succ(X1, X), 
	between(1,A,X1).
	
step(right, X, Y, X, Y1, P, P, Ships,Ships) :- 
	grid(A,B),
	succ(Y, Y1), 
	between(1, B, Y1).
	
step(left, X, Y, X, Y1, P, P, Ships,Ships) :-
	grid(_,B),
	succ(Y1, Y), 
	between(1,B, Y1).
	
step(pickup, X, Y, X, Y, P, Pnew, Ships, ShipsNew) :-
	member([X,Y],Ships),
	capacity(N),
	P < N,
	succ(P, Pnew),
	delete(Ships, [X,Y], ShipsNew).

step(drop, X, Y, X, Y, P, Pnew, Ships,Ships) :-
	station(X,Y),
	P > 0,
	Pnew = 0.

	
%GoalTest: ships is empty, CG with P = 0, CG location on station
goalTest(X,Y,ShipsNew,Pnew):-
	station(X,Y),
	length(ShipsNew, 0),
	Pnew is 0.

path(X, Y, Pnew, ShipsNew, Actions, Result) :- 
	goalTest(X, Y, ShipsNew, Pnew), 
	Result = Actions.

path(X0, Y0, P, Ships, Actions, Result) :-	
	currentSituation(agent_loc(X1,Y1), ships_loc(L1), Passengers1, result(Action, agent_loc(X0,Y0), ships_loc(Ships), P)),
	NewAction = result(Action, Actions),
	path(X1, Y1, Passengers1, L1, NewAction, Result).
	
goal(S):-
	agent_loc(X0,Y0),
    ships_loc(Ships),
	ids(X0, Y0, 0, Ships, s0, S, 1).
	
	
goal(Res,Runtime):-
statistics(runtime,[Start|_]),
goal(Res),
statistics(runtime,[Stop|_]),
Runtime is (Stop - Start)*0.001.



ids(X0, Y0, 0, Ships, s0, Result, L):-
(call_with_depth_limit(path(X0, Y0, 0, Ships, s0, Result),L,R), number(R));
(call_with_depth_limit(path(X0, Y0, 0, Ships, s0, Result),L,R), R=depth_limit_exceeded,
L1 is L+1, ids(X0, Y0, 0, Ships, s0, Result, L1)).

%goal(result(drop, result(up, result(left, result(pickup, result(down, result(right, result(drop, result(left, result(pickup, result(down, result(right, s0)))))))))))).