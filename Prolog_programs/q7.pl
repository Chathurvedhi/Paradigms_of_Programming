natural_number(0).
natural_number(s(X)) :- natural_number(X).

plus(X, s(0), s(X)) :- !.
plus(X,0,X) :- !.
plus(X,s(Y),s(Z)) :- plus(X,Y,Z),! .

mult(X,0,0) :- !.
mult(X,s(Y),Z) :-  mult(X,Y,Z1), plus(X,Z1,Z), !. 

of_int(0,0) :- !.
of_int(N,s(Y)) :- N1 is N-1, of_int(N1,Y).

to_int(0,0) :- !.
to_int(s(Y),N) :- to_int(Y,N1), N is N1+1.

sub(X,0,X) :- !.
sub(s(X),s(Y),Z) :- sub(X,Y,Z), !.