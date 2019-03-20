member(H, [H | _]).                      % a
member(H, [_ | T]) :-                    % b
    write(T),
    member(H, T).
