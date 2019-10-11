%% @author eric
%% @doc SReader
%% cd("D:/devel/erlang/SReader/src").

-module('sreader').

-import(string, [to_lower/1, to_integer/1]).
-import(lists, [reverse/1]).

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
	
	
%% ====================================================================
%% Internal functions
%% ====================================================================

%% Tokenizer
lexme([], [], _ , _, _) ->
	[];

lexme([], T, Line , TCol, _) ->
	[{reverse(T), Line, TCol}] ;

lexme([$\s | S], [], Line, _, Col) ->
	lexme(S, [], Line, Col+1, Col+1);

lexme([$\t | S], [], Line, _, Col) ->
	lexme(S, [], Line, Col+1, Col+1);

lexme([$\r | [$\n | S]], [], Line, _, _) ->
	lexme(S, [], Line+1, 0, 0);

lexme([$\n | S], [], Line, _, _) ->
	lexme(S, [], Line+1, 0, 0);

lexme([$\s | S], T, Line, TCol, Col) ->
	[{reverse(T), Line, TCol} | lexme(S, [], Line, Col+1, Col+1)];

lexme([$\t | S], T, Line, TCol, Col) ->
	[{reverse(T), Line, TCol} | lexme(S, [], Line, Col+1, Col+1)];

lexme([$\r | [$\n | S]], T, Line, TCol, _) ->
	[{reverse(T), Line, TCol} | lexme(S, [], Line+1, 0, 0)];

lexme([$\n | S], T, Line, TCol, _) ->
	[{reverse(T), Line, TCol} | lexme(S, [], Line+1, 0, 0)];

lexme([$( | S], [], Line, _, Col) ->
	[{open_paren, Line, Col} | lexme(S, [], Line, Col+1, Col+1)];

lexme([$( | _], _, Line, TCol, _) ->
	erlang:error("Syntax error at (" ++ Line ++ ", " ++ TCol ++ ")");

lexme([$) | S], [], Line, TCol, Col) ->
	[{close_paren, Line, TCol} | lexme(S, [], Line, Col+1, Col+1)];

lexme([$) | S], T, Line, TCol, Col) ->
	[{reverse(T), Line, TCol} |  lexme([$) | S], [], Line, Col, Col)];

lexme([$" | S], [], Line, _, Col) ->
	lexme_a_string(S, [], Line, Col, Col+1);

lexme([C | S], T, Line, TCol, Col) ->
	lexme(S, [C | T], Line, TCol, Col+1).

lexme_a_string([$" | S], [], Line, TCol, Col) ->
	[{string, "", Line, TCol} | lexme(S, [], Line, Col+1, Col+1)];

lexme_a_string([$" | S], T, Line, TCol, Col) ->
	[{string, reverse(T), Line, TCol} | lexme(S, [], Line, Col+1, Col+1)];

lexme_a_string([C | S], T, Line, TCol, Col) ->
	lexme_a_string(S, [C | T], Line, TCol, Col+1).
	
readlexs([]) ->
	[];

readlexs([Lex | Rest]) ->
	[lex_to_value(Lex) | readlexs(Rest)].

%% Utils
multire([], _) ->
    nomatch;

multire([RE | RegExps], S) ->
    case re:run(S, RE, [{capture, none}]) of
    match ->
        RE;
    nomatch ->
        multire(RegExps, S)
    end.

lex_to_value({string, S, Line, Col}) ->
	{string, S, Line, Col};

lex_to_value({open_paren, Line, Col}) ->
	{open_paren, Line, Col};

lex_to_value({close_paren, Line, Col}) ->
	{close_paren, Line, Col};

lex_to_value({Lex, Line, Col}) ->
    test(multire(["^\-?[0-9]+$", "^[\"].*[\"]$"], Lex), Lex, Line, Col).

test("^\-?[0-9]+$", Lex, Line, Col) ->
	{V, []} = to_integer(Lex),
	{int, V, Line, Col};

test("^[\"].*[\"]$", Lex, Line, Col) ->
	[$\" | S1] = Lex,
	[$\" | S2] = reverse(S1),
	{str, reverse(S2), Line, Col};

test(nomatch, Lex, Line, Col) ->
	{sym, Lex, Line, Col}.

%% Parser
parse([]) ->
	[];

parse([{int, V, Line, Col} | Rest]) ->
	[{int, V, Line, Col} | parse(Rest)];

parse([{string, S, Line, Col} | Rest]) ->
	[{string, S, Line, Col} | parse(Rest)];

parse([{sym, S, Line, Col} | Rest]) ->
	[{sym, list_to_atom(S), Line, Col} | parse(Rest)];

parse([{open_paren, _, _} | Rest]) ->
	{L, R} = parse_list(Rest, []),
	[L | parse(R)];

parse([{close_paren, Line, Col} | _]) ->
	io:format("Parsing expression at line ~p, column ~p : ", [Line, Col]),
	erlang:error("Malformed expression").

parse_list([], _) ->
	erlang:error("Unexpected EOF");

parse_list([{close_paren, _, _} | Rest], List) ->
	{reverse(List), Rest};

parse_list([{open_paren, _, _} | Rest], List) ->
	{L, R} = parse_list(Rest, []),
	parse_list(R, [L | List]);

parse_list([{int, V, Line, Col} | Rest], List) ->
	parse_list(Rest, [{int, V, Line, Col} | List]);

parse_list([{string, S, Line, Col} | Rest], List) ->
	parse_list(Rest, [{string, S, Line, Col} | List]);

parse_list([{sym, S, Line, Col} | Rest], List) ->
	parse_list(Rest, [{sym, list_to_atom(S), Line, Col} | List]).

%% Reader
read_from_string("") ->
	[];

read_from_string(S) ->
	[E | _] = parse(readlexs(lexme(S, [], 0, 0, 0))),
	E.

%% Eval
eval_with_frame(_, []) ->
	eof;

eval_with_frame(_, {string, S, _, _}) ->
	S;

eval_with_frame(_, {int, V, _, _}) ->
	V;

eval_with_frame(_, [{sym, quote, _, _}, {_, E, _, _}]) ->
	E;

eval_with_frame(_, [{sym, quote, _, _}, L]) ->
	L;

eval_with_frame({none, []}, {sym, S, Line, Col}) ->
	io:format("Unbound symbol : ~p at ~p,~p~n", [S, Line, Col]),
	erlang:error("Eval argument error");

eval_with_frame({_, [{K, V} | _]}, {sym, S, _, _}) when K == S ->
	V;

eval_with_frame({Parent, [{_, _} | Rest]}, {sym, S, Line, Col}) ->
	eval_with_frame({Parent, Rest}, {sym, S, Line, Col});

eval_with_frame({{Parent, Frame}, []}, {sym, S, Line, Col}) ->
	eval_with_frame({Parent, Frame}, {sym, S, Line, Col}).

%% Start
start() ->
%% 	S = "   42 ( abc)\n
%%          -1234
%%          \"b\"
%%          \"some text\"   \"\"   ",
%% 	S = "(+ (* 2 (/ 12 6) 3) 4 \"z\") 42",
%% 	S = "(",
%% 	S = "
%% (+ (* 2 (/ 12 6) 3) 4)",
	S = "(quote (5))",
%1 3 5 7 901 3 5 7 901
%% 	L = lexme(S, [], 0, 0, 0),
%% 	io:fwrite(L),
%% 	io:fwrite("\n========\n"),
%% 	Lexs = readlexs(L),
	eval_with_frame({{none, [{version, 0.1}]}, [{a, 42}]}, read_from_string(S)).

