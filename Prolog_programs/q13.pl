split([],[],X) :- !.
split([A,A|X],[A,A|Y],Z) :- split([A|X],[A|Y],Z),!.
split([A],[A],[]) :- atomic(A).
split([A,B|X],[A],[B|X]) :- A \= B, !.

pack([],[]) :- !.
pack([X],[[X]]) :- atomic(X),!.
pack(X,Z) :- split(X,A,B), pack(B,W), Z = [A|W].

encode(X,Y) :- pack(X,Z), encount(Z,Y).
encount([],[]) :- !.
encount([X|Y],Z) :- length(X,N), encount(Y,W), value(X,X1), Z = [[N,X1]|W].

value([],0) :- !.
value([X|Y],X) :- atomic(X),!.
