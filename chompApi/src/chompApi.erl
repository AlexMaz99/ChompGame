%%%-------------------------------------------------------------------
%%% @author Ola
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. maj 2021 14:15
%%%-------------------------------------------------------------------
-module(chompApi).
-author("Ola").

%% API
-export([
  createGame/0,
  isFreePosition/2,
  isProperPosition/1,
  canEatChocolate/2,
  countElements/1,
  countElements/2,
  isGameEnded/1,
  eatChocolates/2,
  eatChocolateFrom/2,
  takeTurn/2,
  getEatenChocolates/1,
  isFirstPlayerTurn/1
]).

-record(game, {eatenChocolates, firstPlayerTurn, hasEnded}).

createGame() ->
  #game{eatenChocolates = [], firstPlayerTurn = true, hasEnded = false}.

isFreePosition(Position, Game) ->
  not lists:member(Position, Game#game.eatenChocolates).

isProperPosition({X, Y}) ->
  (X > -1) and (X < 5) and (Y > -1) and (Y < 4).

canEatChocolate(Position, Game) ->
  isProperPosition(Position) and isFreePosition(Position, Game).

countElements(List) -> countElements(0, List).
countElements(Acc, []) -> Acc;
countElements(Acc, [_ | Tail]) -> countElements(Acc + 1, Tail).

isGameEnded(Game) ->
  countElements(Game#game.eatenChocolates) > 18.

eatChocolates([], Game) -> Game;

eatChocolates([One | Rest], Game) ->
  case canEatChocolate(One, Game) of
    true -> eatChocolates(Rest, #game{eatenChocolates = [One | Game#game.eatenChocolates], firstPlayerTurn = Game#game.firstPlayerTurn, hasEnded = Game#game.hasEnded});
    false -> eatChocolates(Rest, Game)
  end.

eatChocolateFrom({X, Y}, Game) ->
  PositionsToAdd = [{NewX, NewY} || NewX <- lists:seq(X, 4), NewY <- lists:seq(Y, 3), canEatChocolate({NewX, NewY}, Game)],
  eatChocolates(PositionsToAdd, Game).

takeTurn(Position, Game) ->
  case canEatChocolate(Position, Game) of
    false -> {error, "Given chocolate has been already eaten"};
    true ->
      NewGame = eatChocolateFrom(Position, Game),
      GameHasEnded = isGameEnded(NewGame),
      #game{eatenChocolates = NewGame#game.eatenChocolates, firstPlayerTurn = not NewGame#game.firstPlayerTurn, hasEnded = GameHasEnded}
  end.

getEatenChocolates(Game) ->
  Game#game.eatenChocolates.

isFirstPlayerTurn(Game) ->
  Game#game.firstPlayerTurn.
