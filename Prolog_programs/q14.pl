partition([],[],[]) :- !.
partition([H],[H],[]) :- !.
partition(L,P,S) :- append(P,S,L), length(P,N), length(S,N).
partition(L,P,S) :- append(P,S,L), length(P,N), N1 is N+1, length(S,N1).

merge(X,[],X) :- !.
merge([],X,X) :- !.
merge([X|Y], [A|B], [A|Z]) :- length(X,X1), length(A,A1), X1 >= A1, merge([X|Y], B, Z), !.
merge([X|Y], [A|B], [X|Z]) :- length(X,X1), length(A,A1), X1 < A1, merge(Y, [A|B], Z), !.

lsort([],[]) :- !.
lsort([X],[X]) :- !.
lsort(L,SL) :- partition(L,P,S), lsort(P,SP), lsort(S,SS), merge(SP,SS,SL).

