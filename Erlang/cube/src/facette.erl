%% @author eric
%% @doc @todo Add description to facette.

-module(facette).

-include_lib("../include/records.hrl"). 

-export([facette/2, facette/3]).


%% ====================================================================
%% API functions
%% ====================================================================
%% facette
%%
facette(X, Y) when X >= 0, Y >= 0 ->
	#facette{x=X, y=Y}.

facette(X, Y, D) when X >= 0, Y >= 0 ->
	#facette{x=X, y=Y, display=D}.