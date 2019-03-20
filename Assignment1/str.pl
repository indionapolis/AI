count_to_ten(10) :- write(10), nl.

count_to_ten(X) :-
  write(X), nl,
  Y is X + 1,
  count_to_ten(Y).
