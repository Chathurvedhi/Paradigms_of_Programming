of_int(0,0) :- !.
of_int(N,s(Y)) :- N1 is N-1, of_int(N1,Y).

to_int(0,0) :- !.
to_int(s(Y),N) :- to_int(Y,N1), N is N1+1.