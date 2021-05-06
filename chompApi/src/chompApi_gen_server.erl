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
  getGame/0,
  newGame/0,
  takeTurn/1
]).

%% START %%
start_link() -> gen_server:start_link({local,?MODULE}, ?MODULE, [], []).
init(_) -> {ok, chompApi:createGame()}.

%% INTERFACE CLIENT -> SERVER %%
close() -> gen_server:call(?MODULE, terminate).
getGame() -> gen_server:call(?MODULE, {get}).
newGame() -> gen_server:call(?MODULE, {new}).
takeTurn(Position) -> gen_server:call(?MODULE, {takeTurn, Position}).

%% HANDLE MESSAGES %%
handle_call({get}, _From, Game) ->
  {reply, Game, Game};

handle_call({new}, _From, _) ->
  Game = chompApi:createGame(),
  {reply, Game, Game};

handle_call({takeTurn, Position}, _From, Game) ->
  NewGame = chompApi:takeTurn(Position, Game),
  {reply, NewGame, NewGame}.

handle_info(_, Game) -> {noreply, Game}.

terminate(normal, _) -> io:format("Normal terminate~n"), ok;
terminate(other, _) -> io:format("Other terminate~n"), ok;
terminate(kill, _) -> io:format("Kill terminate~n"), ok.
