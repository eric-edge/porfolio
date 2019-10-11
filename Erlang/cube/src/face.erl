%% @author eric
%% @doc @todo Add description to face.

-module(face).

-include_lib("../include/records.hrl").

-export([face/1]).


%% ====================================================================
%% API functions
%% ====================================================================
%% face
%%
face(Fa) when Fa >= 0, Fa =< 6 ->
	#face{fa=Fa}.

