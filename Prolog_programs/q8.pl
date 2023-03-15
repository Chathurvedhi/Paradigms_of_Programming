merge(X,[],X) :- !.
merge([],X,X) :- !.
merge([X|Y], [A|B], [A|Z]) :- X >= A, merge([X|Y], B, Z), !.
merge([X|Y], [A|B], [X|Z]) :- X < A, merge(Y, [A|B], Z), !.