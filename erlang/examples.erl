-module(examples).

-export([inventory/0,
         inventory/3]).

-export([trading/0,
         trading/5]).

%% Inventory example

inventory() ->
    inventory(50, 6, [{0, 0.7}, {1, 0.2}, {2, 0.1}]).

inventory(T, C, P) ->
    X = fun (F, A, _) -> lists:foldl(F, A, lists:seq(0, C)) end,
    W = fun (F, A, _) -> lists:foldl(F, A, P) end,
    F = fun (_, Inv, Ord, Dem) when Inv + Ord >= Dem, Inv + Ord - Dem =< C ->
                Inv + Ord - Dem;
            (_, _, _, _) ->
                invalid
        end,
    G = fun (_, Inv, Ord, _) -> 0.1 * Inv + case Ord of 0 -> 0; _ -> 1 end end,
    mdp:policy(T, {X, X, W, F, G}).

%% Trading example

trading() ->
    trading(250, -5, 15, 0.005, 20).

trading(T, Qmin, Qmax, Gamma, N) ->
    Transition = fun (K, Prices) ->
                         L = 2 * math:log(Prices:fetch(K)),
                         A = min(max(0.4 - L, 0.01), 0.98),
                         B = min(max(0.4 + L, 0.01), 0.98),
                         [{W, P} || {W, P} <- [{K + 1, A}, {K - 1, B}, {K, 1 - A - B}], abs(W) =< N]
                 end,
    Prices = dict:from_list([{K, math:pow(1 + Gamma, K)} || K <- lists:seq(-N, N)]),
    PTrans = dict:from_list([{K, Transition(K, Prices)} || K <- lists:seq(-N, N)]),
    States = [{Q, K} || Q <- lists:seq(Qmin, Qmax), K <- lists:seq(-N, N)],
    X = fun (F, A, _) -> lists:foldl(F, A, States) end,
    U = fun (F, A, {_, {Q, _}}) -> lists:foldl(F, A, lists:seq(Qmin - Q, Qmax - Q)) end,
    W = fun (F, A, {_, {_, K}, _}) -> lists:foldl(F, A, PTrans:fetch(K)) end,
    F = fun (Ti, {Q, _}, D, _) when Ti =:= T, Q + D =/= 0 ->
                invalid;
            (_, {Q, _}, D, L) ->
                {Q + D, L}
        end,
    G = fun (_, {_, K}, D, _) -> D * Prices:fetch(K) end,
    mdp:policy(T, {X, U, W, F, G}).
