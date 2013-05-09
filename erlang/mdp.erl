-module(mdp).

-export([policy/2]).

policy(T, MDP) ->
    policy(T, MDP, []).

policy(T, _, Policy) when T < 0 ->
    Policy;
policy(T, MDP, Policy) ->
    policy(T - 1, MDP, [{T, bellman(T, MDP, Policy)}|Policy]).

value(invalid, _) ->
    none;
value(_, []) ->
    0;
value(Xt, [{_, P}|_]) ->
    element(1, P:fetch(Xt)).

bellman(T, {X, U, W, F, G}, Policy) ->
    X(fun (Xt, A) ->
              A:store(Xt,
                      U(fun (Utx, Opt) ->
                                min({W(fun (_, none) ->
                                               none;
                                           ({Wtxu, P}, E) ->
                                               case value(F(T, Xt, Utx, Wtxu), Policy) of
                                                   none ->
                                                       none;
                                                   Vxu ->
                                                       E + (G(T, Xt, Utx, Wtxu) + Vxu) * P
                                               end
                                       end, 0, {T, Xt, Utx}), Utx}, Opt)
                        end, {none, none}, {T, Xt}))
      end, dict:new(), T).
