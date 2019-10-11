%% @author eric
%% @doc @todo Add description to cube.

-module(cube).

-include_lib("../include/records.hrl"). 

-import(cufaxy, [cufaxy/1, cufaxy/4]).

-export([cube/1, cube/2, cube/4, cube/5, cube/7, cube/8, get_facette/4]).


%% ====================================================================
%% API functions
%% ====================================================================

%% cube
% cube/1
cube(Cu) 
  when is_integer(Cu) ->
	#cube{cufaxy=cufaxy(Cu)}.


% cube/2
cube(Parent, Cu) 
  when	is_integer(Cu) and
		is_record(Parent, cube) ->
	#cube{parent=Parent, cufaxy=cufaxy(Cu)}.


% cube/4
cube(Cu, Fa, X, Y)
  when	is_integer(Cu) and
		is_integer(Fa) and
		is_integer(X)  and
		is_integer(Y) ->
	#cube{cufaxy=cufaxy(Cu, Fa, X, Y)}.


% cube/5
cube(Parent, Cu, Fa, X, Y)
  when	is_integer(Cu) and
		is_integer(Fa) and
		is_integer(X)  and
		is_integer(Y)  and
		is_record(Parent, cube) ->
	#cube{parent=Parent, cufaxy=cufaxy(Cu, Fa, X, Y)}.


% cube/7
cube(Parent, S, T, Cu, Fa, X, Y)
  when	is_integer(Cu) and
		S >= 1         and
		element(T, {hard, soft}) and
		is_integer(Fa) and
		is_integer(X)  and
		is_integer(Y)  and
		is_record(Parent, cube) ->
	#cube{parent=Parent, size=S, type=T, cufaxy=cufaxy(Cu, Fa, X, Y)}.


% cube/8
cube(Parent, S, T, Cu, Fa, X, Y, D)
  when	is_integer(Cu) and
		S >= 1         and
		element(T, {hard, soft}) and
		is_integer(Fa) and
		is_integer(X)  and
		is_integer(Y)  and
		is_record(Parent, cube) ->
	#cube{parent=Parent, size=S, type=T, cufaxy=cufaxy(Cu, Fa, X, Y), data=D}.


%% get_facette
% gat_facette/4
get_facette({_, _, hard, _, [], _}, Fa, X, Y) ->
	cube(none, 1, soft, 0, Fa, X, Y);

get_facette({_, _, hard, _, [{_, _, soft, {_, Fa_, X_, Y_}, _, D} | _], _}, Fa, X, Y)
  when	Fa == Fa_, 
		X  == X_,
		Y  == Y_ ->
	cube(none, 1, soft, 0, Fa, X, Y, D);

get_facette({P, S, hard, C, [{_, _, soft, {_, _, _, _}, _, _} | Children], D}, Fa, X, Y) ->
	get_facette({P, S, hard, C, Children, D}, Fa, X, Y).

	


