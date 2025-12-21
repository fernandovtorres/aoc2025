-module(part1).
-export([solve/1, test/0]).

solve(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Lines = string:tokens(binary_to_list(Data), "\n"),
    Solutions = [solve_machine(Line) || Line <- Lines],
    TotalPresses = lists:sum(Solutions),
    TotalPresses.

solve_machine(Line) ->
    {TargetState, Buttons} = parse_line(Line),
    NumLights = length(TargetState),
    NumButtons = length(Buttons),
    Matrix = build_matrix(NumLights, Buttons, TargetState),
    {RREF, PivotCols} = gaussian_elimination(Matrix, NumLights, NumButtons),
    find_min_presses(RREF, PivotCols, NumButtons).

build_matrix(NumLights, Buttons, TargetState) ->
    [
        begin
            ButtonRow = [check_button(R, Btn) || Btn <- Buttons],
            ButtonRow ++ [lists:nth(R + 1, TargetState)]
        end
        || R <- lists:seq(0, NumLights - 1)
    ].

check_button(LightIdx, ButtonIndices) ->
    case lists:member(LightIdx, ButtonIndices) of
        true -> 1;
        false -> 0
    end.

gaussian_elimination(Matrix, Rows, Cols) ->
    eliminate(Matrix, 0, 0, Rows, Cols, []).

eliminate(Matrix, _R, C, _Rows, Cols, Pivots) when C >= Cols ->
    {Matrix, lists:reverse(Pivots)};
eliminate(Matrix, R, _C, Rows, _Cols, Pivots) when R >= Rows ->
    {Matrix, lists:reverse(Pivots)};
eliminate(Matrix, R, C, Rows, Cols, Pivots) ->
    case find_pivot(Matrix, R, C) of
        none ->
            eliminate(Matrix, R, C + 1, Rows, Cols, Pivots);
        PivotRowIdx ->
            MatrixSwapped = swap_rows(Matrix, R, PivotRowIdx),
            MatrixReduced = [
                if 
                    Idx =:= R -> Row;
                    true -> 
                        case lists:nth(C + 1, Row) of
                            1 -> vec_xor(Row, lists:nth(R + 1, MatrixSwapped));
                            0 -> Row
                        end
                end
                || {Row, Idx} <- lists:zip(MatrixSwapped, lists:seq(0, Rows - 1))
            ],
            eliminate(MatrixReduced, R + 1, C + 1, Rows, Cols, [{R, C} | Pivots])
    end.

find_pivot(Matrix, StartRow, Col) ->
    RowsToCheck = lists:nthtail(StartRow, Matrix),
    case find_first_one(RowsToCheck, Col, 0) of
        none -> none;
        RelIdx -> StartRow + RelIdx
    end.

find_first_one([], _Col, _Idx) -> none;
find_first_one([Row | Rest], Col, Idx) ->
    case lists:nth(Col + 1, Row) of
        1 -> Idx;
        0 -> find_first_one(Rest, Col, Idx + 1)
    end.

swap_rows(Matrix, I, I) -> Matrix;
swap_rows(Matrix, I, J) ->
    RowI = lists:nth(I + 1, Matrix),
    RowJ = lists:nth(J + 1, Matrix),
    replace_nth(J + 1, replace_nth(I + 1, Matrix, RowJ), RowI).

replace_nth(N, List, NewElem) ->
    {Left, [_ | Right]} = lists:split(N - 1, List),
    Left ++ [NewElem] ++ Right.

vec_xor(L1, L2) ->
    [A bxor B || {A, B} <- lists:zip(L1, L2)].

find_min_presses(RREF, Pivots, NumButtons) ->
    PivotColIndices = [C || {_R, C} <- Pivots],
    FreeColIndices = [C || C <- lists:seq(0, NumButtons - 1), 
                      not lists:member(C, PivotColIndices)],

    NumFree = length(FreeColIndices),
    Combinations = trunc(math:pow(2, NumFree)),
    
    PressCounts = lists:filtermap(fun(Idx) -> 
        FreeVals = integer_to_bits(Idx, NumFree),
        FreeMap = maps:from_list(lists:zip(FreeColIndices, FreeVals)),
        try_solve(RREF, Pivots, FreeMap, NumButtons)
    end, lists:seq(0, Combinations - 1)),

    lists:min(PressCounts).

try_solve(RREF, Pivots, FreeMap, NumButtons) ->
    RevPivots = lists:reverse(Pivots),
    
    SolutionMap = lists:foldl(fun({RowIdx, ColIdx}, AccMap) ->
        Row = lists:nth(RowIdx + 1, RREF),
        TargetVal = lists:last(Row),
        
        SumRight = lists:foldl(fun(K, Sum) ->
            Coeff = lists:nth(K + 1, Row),
            Val = maps:get(K, AccMap, 0),
            Sum + (Coeff * Val)
        end, 0, lists:seq(ColIdx + 1, NumButtons - 1)),
        
        ValX = (TargetVal + SumRight) rem 2,
        maps:put(ColIdx, ValX, AccMap)
    end, FreeMap, RevPivots),

    Total = maps:fold(fun(_K, V, Acc) -> Acc + V end, 0, SolutionMap),
    {true, Total}.

integer_to_bits(_Int, 0) -> [];
integer_to_bits(Int, Size) ->
    Bits = [X - $0 || X <- integer_to_list(Int, 2)],
    PadLen = Size - length(Bits),
    lists:duplicate(PadLen, 0) ++ Bits.

parse_line(Line) ->
    [DiagramStr, Rest] = string:split(Line, "]"),
    "[" ++ DiagramContent = string:trim(DiagramStr),
    Target = [case C of $. -> 0; $# -> 1 end || C <- DiagramContent],

    {ok, RE} = re:compile("\\(([^)]+)\\)"),
    {match, Matches} = re:run(Rest, RE, [global, {capture, all_but_first, list}]),
    
    Buttons = [
        [list_to_integer(N) || N <- string:tokens(hd(M), ",")]
        || M <- Matches
    ],
    
    {Target, Buttons}.

test() ->
    L1 = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}",
    2 = solve_machine(L1),
    L2 = "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}",
    3 = solve_machine(L2),
    L3 = "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}",
    2 = solve_machine(L3),
    io:format("All tests passed!~n").
