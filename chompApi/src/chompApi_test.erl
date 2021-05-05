%%%-------------------------------------------------------------------
%%% @author Ola
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. maj 2021 15:07
%%%-------------------------------------------------------------------
-module(chompApi_test).
-author("Ola").

-include_lib("eunit/include/eunit.hrl").

isFreePositionWhenIs_test() ->
    Game = chompApi:createGame(),
    [?assert(chompApi:isFreePosition({X, Y}, Game)) || X <- lists:seq(0, 4), Y <- lists:seq(0, 3)].

isFreePositionWhenIsNot_test() ->
    Game = chompApi:eatChocolates([{1, 2}, {0, 0}], chompApi:createGame()),
    ?assert(not chompApi:isFreePosition({1, 2}, Game)),
    ?assert(not chompApi:isFreePosition({0, 0}, Game)).

isProperPosition_test() ->
    [?assert(chompApi:isProperPosition({X, Y})) || X <- lists:seq(0, 4), Y <- lists:seq(0, 3)],
    ?assert(not chompApi:isProperPosition({-1, 0})),
    ?assert(not chompApi:isProperPosition({5, 4})).

countElements_test() ->
    ?assertMatch(0, chompApi:countElements([])),
    ?assertMatch(2, chompApi:countElements([{1, 1}, {0, 0}])).

eatChocolates_test() ->
    Game = chompApi:eatChocolates([{1, 2}, {0, 0}], chompApi:createGame()),
    ?assertMatch([{0, 0}, {1, 2}], chompApi:getEatenChocolates(Game)),
    Game2 = chompApi:eatChocolates([{3, 1}, {0, 1}], Game),
    ?assertMatch([{0, 1}, {3, 1}, {0, 0}, {1, 2}], chompApi:getEatenChocolates(Game2)).

eatChocolateFrom_test() ->
    Game = chompApi:eatChocolateFrom({3, 2}, chompApi:createGame()),

    ?assert(not chompApi:isFreePosition({3, 2}, Game)),
    ?assert(not chompApi:isFreePosition({4, 2}, Game)),
    ?assert(not chompApi:isFreePosition({4, 3}, Game)),
    ?assert(not chompApi:isFreePosition({3, 3}, Game)),
    ?assertMatch(4, chompApi:countElements(chompApi:getEatenChocolates(Game))),

    Game2 = chompApi:eatChocolateFrom({3, 0}, Game),

    ?assert(not chompApi:isFreePosition({4, 0}, Game2)),
    ?assert(not chompApi:isFreePosition({3, 1}, Game2)),
    ?assert(not chompApi:isFreePosition({4, 1}, Game2)),
    ?assert(not chompApi:isFreePosition({3, 2}, Game2)),
    ?assert(not chompApi:isFreePosition({4, 2}, Game2)),
    ?assert(not chompApi:isFreePosition({3, 3}, Game2)),
    ?assert(not chompApi:isFreePosition({4, 3}, Game2)),
    ?assertMatch(8, chompApi:countElements(chompApi:getEatenChocolates(Game2))).

takeTurn_test() ->
    Game = chompApi:createGame(),
    ?assert(chompApi:isFirstPlayerTurn(Game)),
    ?assert(not chompApi:isGameEnded(Game)),

    Game2 = chompApi:takeTurn({1, 0}, Game),
    ?assert(not chompApi:isFirstPlayerTurn(Game2)),
    ?assert(not chompApi:isGameEnded(Game2)),

    Game3 = chompApi:takeTurn({0, 1}, Game2),
    ?assert(chompApi:isFirstPlayerTurn(Game3)),
    ?assert(chompApi:isGameEnded(Game3)).
