-module(part2).
-export([solve/1]).

solve(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Lines = string:tokens(binary_to_list(Data), "\n"),
    Results = [solve_machine(L) || L <- Lines],
    lists:sum(Results).


solve_machine(Line) ->
    {Target, Buttons} = parse(Line),
    NumEqs = length(Target),
    NumVars = length(Buttons),
    Matrix = build_matrix(Buttons, Target, NumEqs),
    {RREF, Pivots} = gaussian(Matrix, NumEqs, NumVars),
    PivotCols = [C || {_, C} <- Pivots],
    FreeCols = [C || C <- lists:seq(0, NumVars - 1), not lists:member(C, PivotCols)],

    MaxBound = lists:max(Target),

    case solve_search(FreeCols, #{}, RREF, Pivots, NumVars, MaxBound) of
        infinity -> 0;
        Val -> Val
    end.


solve_search([], Assigns, RREF, Pivots, NumVars, _) ->
    case resolve_pivots(RREF, Pivots, Assigns, NumVars) of
        {ok, FinalMap} ->
            Valid = maps:fold(fun(_, Val, Acc) -> 
                Acc andalso (Val >= 0) andalso (trunc(Val) == Val)
            end, true, FinalMap),
            
            if Valid -> maps:fold(fun(_, V, A) -> A + trunc(V) end, 0, FinalMap);
               true -> infinity
            end;
        error -> infinity
    end;

solve_search([Col | Rest], Assigns, RREF, Pivots, NumVars, MaxBound) ->
    lists:foldl(fun(K, CurrentMin) ->
        NewAssigns = maps:put(Col, K, Assigns),
        Res = solve_search(Rest, NewAssigns, RREF, Pivots, NumVars, MaxBound),
        min(CurrentMin, Res)
    end, infinity, lists:seq(0, MaxBound)).


gaussian(Matrix, Rows, Cols) ->
    gauss_step(Matrix, 0, 0, Rows, Cols, []).

gauss_step(M, R, C, Rows, Cols, Pivots) when R >= Rows; C >= Cols -> 
    {M, lists:reverse(Pivots)};
gauss_step(M, R, C, Rows, Cols, Pivots) ->
    case find_pivot(M, R, C) of
        none -> gauss_step(M, R, C + 1, Rows, Cols, Pivots);
        Idx ->
            M2 = swap(M, R, Idx),
            M3 = normalize(M2, R, C),
            M4 = eliminate(M3, R, C, Rows),
            gauss_step(M4, R + 1, C + 1, Rows, Cols, [{R, C}|Pivots])
    end.

find_pivot(M, R, C) -> find_p(lists:nthtail(R, M), C, R).
find_p([], _, _) -> none;
find_p([H|T], C, Idx) ->
    {N, _} = lists:nth(C + 1, H),
    if N /= 0 -> Idx; true -> find_p(T, C, Idx + 1) end.

swap(M, I, I) -> M;
swap(M, I, J) ->
    RowI = lists:nth(I + 1, M),
    RowJ = lists:nth(J + 1, M),
    set_nth(I + 1, set_nth(J + 1, M, RowI), RowJ).

normalize(M, R, C) ->
    Row = lists:nth(R + 1, M),
    Pivot = lists:nth(C + 1, Row),
    NewRow = [rat_div(X, Pivot) || X <- Row],
    set_nth(R + 1, M, NewRow).

eliminate(M, PR, PC, Rows) ->
    PRow = lists:nth(PR + 1, M),
    lists:map(fun({Row, I}) ->
        if I == PR -> Row;
        true ->
            Factor = lists:nth(PC + 1, Row),
            [rat_sub(X, rat_mul(Y, Factor)) || {X, Y} <- lists:zip(Row, PRow)]
        end
    end, lists:zip(M, lists:seq(0, Rows - 1))).

resolve_pivots(RREF, Pivots, Assigns, NumVars) ->
    try
        lists:foldl(fun({RIdx, CIdx}, Acc) ->
            Row = lists:nth(RIdx + 1, RREF),
            RHS = lists:last(Row),
            Sum = lists:foldl(fun(K, S) ->
                Coeff = lists:nth(K + 1, Row),
                Val = maps:get(K, Acc, 0),
                rat_add(S, rat_mul(Coeff, {Val, 1}))
            end, {0, 1}, lists:seq(0, NumVars - 1) -- [CIdx]),
            
            Diff = rat_sub(RHS, Sum),
            {N, D} = Diff,
            if D == 1, N >= 0 -> maps:put(CIdx, N, Acc);
               true -> throw(fail)
            end
        end, Assigns, lists:reverse(Pivots))
    of
        Map -> {ok, Map}
    catch
        throw:fail -> error
    end.



rat_add({N1, D1}, {N2, D2}) -> reduce(N1 * D2 + N2 * D1, D1 * D2).
rat_sub({N1, D1}, {N2, D2}) -> reduce(N1 * D2 - N2 * D1, D1 * D2).
rat_mul({N1, D1}, {N2, D2}) -> reduce(N1 * N2, D1 * D2).
rat_div({N1, D1}, {N2, D2}) -> reduce(N1 * D2, D1 * N2).
reduce(0, _) -> {0, 1};
reduce(N, D) when D < 0 -> reduce(-N, -D);
reduce(N, D) -> G = gcd(abs(N), D), {N div G, D div G}.
gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

set_nth(1, [_|T], New) -> [New|T];
set_nth(N, [H|T], New) -> [H|set_nth(N - 1, T, New)].

build_matrix(Buttons, Target, NumEqs) ->
    [ [check(Eq, B) || B <- Buttons] ++ [{V, 1}] 
      || {Eq, V} <- lists:zip(lists:seq(0, NumEqs - 1), Target) ].

check(Eq, Btn) -> case lists:member(Eq, Btn) of true -> {1, 1}; false -> {0, 1} end.

parse(Line) ->
    {_, [TStr]} = re:run(Line, "\\{([^}]+)\\}", [{capture, all_but_first, list}]),
    Target = [list_to_integer(X) || X <- string:tokens(TStr, ",")],
    [_, Rest] = string:split(Line, "]"),
    {ok, RE} = re:compile("\\(([^)]+)\\)"),
    {match, Matches} = re:run(Rest, RE, [global, {capture, all_but_first, list}]),
    Buttons = [[list_to_integer(N) || N <- string:tokens(hd(M), ",")] || M <- Matches],
    {Target, Buttons}.
