%% @author eric
%% @doc cufaxy

-module(cufaxy).

-include_lib("../include/records.hrl"). 

-import(face, [face/1]).
-import(facette, [facette/2]).


-export([cufaxy/1, cufaxy/4]).


%% ====================================================================
%% API functions
%% ====================================================================
%% cufaxy
%%

cufaxy(Cu) when is_integer(Cu) ->
	#cufaxy{cu=Cu}.

cufaxy(Cu, Fa, X, Y) when is_integer(Cu) and
						  is_integer(Fa) and
						  is_integer(X)  and
						  is_integer(Y) ->
	#cufaxy{cu=Cu, fa=Fa, x=X, y=Y}.
