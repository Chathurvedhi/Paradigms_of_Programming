natural_number(0).
natural_number(s(X)) :- natural_number(X).

plus(X, s(0), s(X)) :- !.
plus(X,0,X) :- !.
plus(X,s(Y),s(Z)) :- plus(X,Y,Z),! .
