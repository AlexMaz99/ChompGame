%%%-------------------------------------------------------------------
%%% @author Ola
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. maj 2021 17:59
%%%-------------------------------------------------------------------
-module(chompApi_gen_server).
-author("Ola").
-behaviour(gen_server).

%% API
-export([start_link/0, init/1, terminate/2, handle_call/3, handle_info/2]).
-export([
  close/0,
  isFreePosition/1,
  isProperPosition/1,
  canEatChocolate/1,
  countElements/1,
  isGameEnded/0,
  eatChocolates/1,
  eatChocolateFrom/1,
  takeTurn/1,
  getEatenChocolates/0,
  isFirstPlayerTurn/0
]).

%% START %%
start_link() -> gen_server:start_link({local,?MODULE}, ?MODULE, [], []).
init(_) -> {ok, chompApi:createGame()}.

%% INTERFACE CLIENT -> SERVER %%
close() -> gen_server:call(?MODULE, terminate).
isFreePosition(Position) -> gen_server:call(?MODULE, {isFreePosition, Position}).
isProperPosition(Position) -> gen_server:call(?MODULE, {isProperPosition, Position}).
canEatChocolate(Position) -> gen_server:call(?MODULE, {canEatChocolate, Position}).
countElements(List) -> gen_server:call(?MODULE, {countElements, List}).
isGameEnded() -> gen_server:call(?MODULE, {isGameEnded}).
eatChocolates(List) -> gen_server:call(?MODULE, {eatChocolates, List}).
eatChocolateFrom(Position) -> gen_server:call(?MODULE, {eatChocolateFrom, Position}).
takeTurn(Position) -> gen_server:call(?MODULE, {takeTurn, Position}).
getEatenChocolates() -> gen_server:call(?MODULE, {getEatenChocolates}).
isFirstPlayerTurn() -> gen_server:call(?MODULE, {isFirstPlayerTurn}).

%% HANDLE MESSAGES %%
handle_call({isFreePosition, Position}, _From, Game) ->
  {reply, chompApi:isFreePosition(Position, Game), Game};
handle_call({isProperPosition, Position}, _From, Game) ->
  {reply, chompApi:isProperPosition(Position), Game};
handle_call({canEatChocolate, Position}, _From, Game) ->
  {reply, chompApi:canEatChocolate(Position, Game), Game};
handle_call({countElements, List}, _From, Game) ->
  {reply, chompApi:countElements(List), Game};
handle_call({isGameEnded}, _From, Game) ->
  {reply, chompApi:isGameEnded(Game), Game};
handle_call({eatChocolates, List}, _From, Game) ->
  {reply, chompApi:eatChocolates(List, Game), Game};
handle_call({eatChocolateFrom, Position}, _From, Game) ->
  {reply, chompApi:eatChocolateFrom(Position, Game), Game};
handle_call({takeTurn, Position}, _From, Game) ->
  {reply, chompApi:takeTurn(Position, Game), Game};
handle_call({getEatenChocolates}, _From, Game) ->
  {reply, chompApi:getEatenChocolates(Game), Game};
handle_call({isFirstPlayerTurn}, _From, Game) ->
  {reply, chompApi:isFirstPlayerTurn(Game), Game}.
handle_info(_, Game) -> {noreply, Game}.

terminate(normal, _) -> io:format("Normal terminate~n"), ok;
terminate(other, _) -> io:format("Other terminate~n"), ok;
terminate(kill, _) -> io:format("Kill terminate~n"), ok.
