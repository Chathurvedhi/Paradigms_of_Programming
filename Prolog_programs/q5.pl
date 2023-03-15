natural_number(0).
natural_number(s(X)) :- natural_number(X).

plus(X, s(0), s(X)) :- !.
plus(X,0,X) :- !.
plus(X,s(Y),s(Z)) :- plus(X,Y,Z),! .

mult(X,0,0) :- !.
mult(X,s(Y),Z) :-  mult(X,Y,Z1), plus(X,Z1,Z), !. % mult(2,X,6) error