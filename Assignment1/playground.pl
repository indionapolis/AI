% the_earth(sphere).

%!  male(?God).
%!  female(?God).
%!  parent(?Parent, ?Child).
%
%   Basic Greek god family relations.

male(kronos).
male(zeus).
male(hades).
male(poseidon).
male(triton).

female(rhea).
female(hebe).
female(amphitrite).
female(hera).
female(rhode).

parent(kronos, hades).
parent(kronos, zeus).
parent(kronos, poseidon).
parent(rhea, hades).
parent(rhea, zeus).
parent(rhea, poseidon).
parent(zeus, hebe).
parent(hera, hebe).
parent(poseidon, triton).
parent(poseidon, rhode).
parent(amphitrite, triton).
parent(amphitrite, rhode).

daughter(Daugther, Parent) :-
    parent(Parent, Daugther),
    female(Daugther).


likes(john,mary).
likes(john,trains).
likes(peter,fast_cars). 

likes(Person1,Person2):-
    hobby(Person1,Hobby), 
    hobby(Person2,Hobby).

hobby(john,trainspotting). 
hobby(tim,sailing). 
hobby(helen,trainspotting). 
hobby(simon,sailing).


% Slides

member(H, [H | _]).
member(H, [_ | T]) :-
    member(X, T).

higher(X, Y, [X | T]) :-
    member(Y, T).
higher(X, Y, [_ | T]) :-
    higher(X, Y, T).

not_adjacent(X, Y, [X, Z | T]) :-
    Z \== Y,
    member(Y, T).
not_adjacent(X, Y, [Y, Z | T]) :-
    Z \== X,
    member(X, T).
not_adjacent(X, Y, [_ | T]) :-
    not_adjacent(X, Y, T).

puzzle(L) :-
    L = [Top, F4, F3, F2, Bottom],
    permutation(L, [adam, bill, claire, david, eric]),
    adam \== Top,
    bill \== Bottom,
    claire \== Top, claire \== Bottom,
    higher(david, bill, L),
    not_adjacent(eric, claire, L),
    not_adjacent(claire, bill, L).

main :-
    write('---'), nl,
    puzzle([A,B,C,D,E]),
    write(A), nl,
    write(B), nl,
    write(C), nl,
    write(D), nl,
    write(E), nl.

len([], 0).
len([_ | T], N) :-
    len(T, N1),
    N = N1 + 1.