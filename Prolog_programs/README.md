# Prolog programs

Q1: Implement reverse of a list using the predicate reverse(X,RevX) where RevX is the reverse of the list X. 
You might want to use the append function studied in the class.

Expected: 

    ?- reverse([],X).
     X = [].
    ?- reverse([1,2,3],X).
     X = [3,2,1].
    ?- reverse([A,B,C],X).
     X = [C,B,A].

Q2. Implement a predicate range(S,E,M) which holds if the integer M is within the range S to E including S and E.

Expected:

    ?- not(range(1,2,0)).
     true.
    ?- range(1,2,1).
     true.
    ?- range(1,2,2).
     true.
    ?- not(range(1,2,3)).
     true.

Q3. Implement the predicate natural_number such that the following tests pass.

Expected:

    ?- natural_number(0).
    true.
    ?- natural_number(s(s(s(0)))).
     true.
    ?- not(natural_number(s(s(z)))).
    true.

Q4. Define the plus operator over the above natural numbers:

Expected: 

    ?- plus(s(s(0)),s(s(0)),s(s(s(s(0))))).
     true.
    ?- plus(s(s(0)),X,s(s(s(s(s(0)))))).
     X = s(s(s(0))).
    ?- not(plus(s(s(0)),X,0)).
     true.
    ?- plus(X,Y,s(s(s(0)))). 
     X=s(s(0)), Y=s(0).

Q5. We can represent multiplication using repeated addition. Write a predicate mult(X,Y,Z) using plus(X,Y,Z) where mult(X,Y,Z) represents X*Y=Z. Do not use built in arithmetic.

Expected:

    ?- mult(s(s(s(0))),s(s(0)),X).
     X = s(s(s(s(s(s(0)))))).
    ?- mult(s(s(s(0))),0,X).
     X = 0.

Q6. Write code to convert church numerals to integers.

Expected:

    ?- of_int(3,Y).
    Y = s(s(s(0))).

    ?- to_int(s(s(0)),X).
    X = 2.

Q7. Implement subtraction predicate sub(X,Y,Z) which holds if X-Y=Z where X,Y and Z are church numerals. Do not use built in arithmetic subtraction.

Expected:

    ?- of_int(5,X), of_int(4,Y), sub(X,Y,Z), to_int(Z,Result).
    Result = 1.
    ?- of_int(4,X), of_int(4,Y), sub(X,Y,Z), to_int(Z,Result), Result = 0.
    true.
    ?- of_int(4,X), of_int(5,Y), not(sub(X,Y,Z)).
    true.

Q8. Implement the predicate merge(X,Y,Z) where X and Y are sorted, and Z contains the same elements as U where append(X,Y,U) but Z is also additionally sorted.

    Expected.
    ?- merge([],[1],[1]).
     true.
    ?- merge([1],[],[1]).
     true.
    ?- merge([1,3,5],[2,4,6],X).
     X = [1,2,3,4,5,6].

Q9. Implement partition(L,P,S) such that

    P is the prefix of L and
    S is the suffix of L and
    append(P,S,L) holds
    If L is [], then P and S are [].
    If L is [H], then P is [H] and S is [].
    Otherwise,
        let length of L be N. Then length of P is div(N/2). Use Prolog's built-in integer division.
        length of S is N - div(N/2).

You may need to use/(re)define len,prefix,suffix,append predicates. You may also need to use Prolog comparison operator >= or > depending on how you write the predicate.


Expected:

    ?- partition([],[],[]).
     true.
    ?- partition([H],[H],[]).
     true.
    ?- partition([1,2,3],[1],X).
     X = [2,3].

Q10. Implement predicate mergesort(L,SL) where SL is the sorted version of L. Use the predicate to partition the list L into two, sort each on separately (using mergesort) and combine the individual sorted list using merge.


Expected:

    ?- mergesort([2,3,1],[1,2,3]), !.
     true.
    ?- mergesort([1,2,3],[1,2,3]), !.
     true.
    ?- mergesort([],[]), !.
     true.

Q11. Flatten a nested list structure.
    Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

Expected:

    ?- my_flatten([a, [b, [c, d], e]], X).
    X = [a, b, c, d, e].

Q12. Pack consecutive duplicates of list elements into sublists.
    If a list contains repeated elements they should be placed in separate sublists.

Expected:

    ?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
    X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]].

Q13. Run-length encoding of a list.
    Use the result of problem Q12 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as terms [N,E] where N is the number of duplicates of the element E.

Expected:

    ?- encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
    X = [[4,a],[1,b],[2,c],[2,a],[1,d][4,e]]

Q14. We suppose that a list (InList) contains elements that are lists themselves. The objective is to sort the elements of InList according to their length. E.g. short lists first, longer lists later, or vice versa.

Expected:

    ?- lsort([[a,b,c],[d,e],[f,g,h],[d,e],[i,j,k,l],[m,n],[o]],L).
    L = [[o], [d, e], [d, e], [m, n], [a, b, c], [f, g, h], [i, j, k, l]]

Q15. Determine the prime factors of a given positive integer.
    Construct a flat list containing the prime factors in ascending order.

Expected:

    ?- prime_factors(315, L).
    L = [3,3,5,7]