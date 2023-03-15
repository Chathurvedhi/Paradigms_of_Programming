% partition L into P,S equal halves
partition([],[],[]) :- !.
partition([H],[H],[]) :- !.
partition(L,P,S) :- append(P,S,L), length(P,N), length(S,N).
partition(L,P,S) :- append(P,S,L), length(P,N), N1 is N+1, length(S,N1).