split([],[],X) :- !.
split([A,A|X],[A,A|Y],Z) :- split([A|X],[A|Y],Z),!.
split([A],[A],[]) :- atomic(A).
split([A,B|X],[A],[B|X]) :- A \= B, !.

pack([],[]) :- !.
pack([X],[[X]]) :- atomic(X),!.
pack(X,Z) :- split(X,A,B), pack(B,W), Z = [A|W].
