append([],L,L).
append([H|T],L,[H|L1]) :- append(T,L,L1).

reverse([],[]).
reverse([H|T],R) :- reverse(T,TR), append(TR,[H],R).