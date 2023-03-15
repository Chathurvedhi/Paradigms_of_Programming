prime_factors(1,[]) :- !.
prime_factors(N,L) :- lowest_prime(N,V1), N1 is N/V1, prime_factors(N1,L1), L = [V1|L1].

lowest_prime(N,V) :- low_prime(N,2,V).
low_prime(N,V,V) :- N mod V =:= 0, !.
low_prime(N,V1,V) :- V2 is V1+1, low_prime(N,V2,V).
