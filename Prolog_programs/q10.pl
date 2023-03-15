partition([],[],[]) :- !.
partition([H],[H],[]) :- !.
partition(L,P,S) :- append(P,S,L), length(P,N), length(S,N).
partition(L,P,S) :- append(P,S,L), length(P,N), N1 is N+1, length(S,N1).

merge(X,[],X) :- !.
merge([],X,X) :- !.
merge([X|Y], [A|B], [A|Z]) :- X >= A, merge([X|Y], B, Z), !.
merge([X|Y], [A|B], [X|Z]) :- X < A, merge(Y, [A|B], Z), !.

mergesort([],[]) :- !.
mergesort([X],[X]) :- !.
mergesort(L,SL) :- partition(L,P,S), mergesort(P,SP), mergesort(S,SS), merge(SP,SS,SL).

