%% @author eric
%% @doc @todo Add description to start.
%% cd("D:/devel/erlang/cube/src").

-module(start).

-import(cube, [cube/1]).


%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).

%% ====================================================================
%% Macros
%% ====================================================================
-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.


start() ->
	C = cube:cube(1),
	C.