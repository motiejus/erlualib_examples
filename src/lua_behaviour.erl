%%% @doc Parse_transform which implements a given behaviour.
%%%
%%% What it does for the module:
%%% 1. Find out which behaviour it implements: {attribute,_,behaviour,Mod}
%%% 2. Call Mod:behaviour_info(callbacks)
%%% Latter function returns a list of functions/arities which must be
%%% implemented in this module. For example: [{add, 2}, {sin, 1}].
%%% 3. Implement every function in that behaviour, so it looks like this:
%%%
%%% add(Arg1, Arg2) ->
%%%     luam:one_call(FileName, "add", [Arg1, Arg2]).
%%% 
%%% FileName is the backing Lua module, in which the module is actually
%%% implemented. It is defined in -implemented_in(String) attribute.
%%%
%%% Assumptions about the parse_transformed module:
%%% 1. Has -behaviour(some_behaviour).
%%% 2. some_behaviour is compiled
%%%     (you can achieve this using erl_first_files in rebar.config)
%%% 3. Has -implemented_in(String).
%%% 4. String is a single Erlang expression (see below).
%%%
%%% Filename is written as abstract form in -implemented_in(String) attribute.
%%% In simple words, String must be 1 Erlang expression.
%%% These Strings are valid (wrapped in -implemented_in):
%%% * "\"crutas_calc.lua\".".
%%% * "code:priv_dir(crutas) ++ \"/crutas_calc.lua\"."
%%% * "fun() -> ok, code:priv_dir(crutas) ++ \"/crutas_calc.lua\" end()."
%%% Note that trailing dots are important.
-module(lua_behaviour).

-export([parse_transform/2]).

parse_transform(Ast, _Opts) ->
    {Ret, _Opt} = walk_ast(Ast, [], []),
    %io:format("Opt: ~p~nRet: ~p~n", [Opt, Ret]),
    Ret.

walk_ast([], AstOut, PL) ->
    {lists:reverse(AstOut), PL};

walk_ast([N={attribute, _, module, Mod}|Rest], AstOut, PL) ->
    walk_ast(Rest, [N|AstOut], [{module, Mod}|PL]);

walk_ast([N={attribute, _, behaviour, Mod}|Rest], AstOut, PL) ->
    walk_ast(Rest, [N|AstOut], [{behaviour, Mod}|PL]);

walk_ast([N={attribute, _, implemented_in, Eval}|Rest], AstOut, PL) ->
    LuaModABS = get_lua_mod_expr(get_value(module, PL), Eval),
    walk_ast(Rest, [N|AstOut], [{implemented_in, LuaModABS}|PL]);

walk_ast([{eof, L}|Rest], AstOut, PL) ->
    Funs = (get_value(behaviour, PL)):behaviour_info(callbacks),
    Export = {attribute,L,export,Funs},
    {EL, FunNodes} = make_fun_nodes(L, Funs, get_value(implemented_in, PL)),
    walk_ast(Rest, lists:append([[{eof, EL}], FunNodes, [Export], AstOut]), PL);

walk_ast([Node|Rest], AstOut, PL) ->
    walk_ast(Rest, [Node|AstOut], PL).

get_value(Key, PL) ->
    Ref = make_ref(),
    case proplists:get_value(Key, PL, Ref) of
        Ref -> error({keyerror, Key, PL});
        Ret -> Ret
    end.

make_fun_nodes(L, Funs, ImplementedIn) ->
    LinesFuns = lists:zip(lists:seq(L+1, L + length(Funs)*2, 2), Funs),
    ABSFuns = [make_fun(Line, Fun, Arity, ImplementedIn)
        || {Line, {Fun, Arity}} <- LinesFuns],

    %io:format("LinesFuns: ~p~n, Abstract funs: ~p~n", [LinesFuns, ABSFuns]),
    {
        L + length(Funs) * 2 + 1,
        lists:reverse(ABSFuns)
    }.

make_fun(BaseLine, Name, Arity, LuaModABS) ->
    Args = [list_to_atom("Arg"++integer_to_list(I)) || I <- lists:seq(1,Arity)],
    Header = [{var, BaseLine, Arg} || Arg <- Args],
    CallArgs = call_args(Args, BaseLine+1),
    {function,BaseLine,Name,Arity,
        [{clause,BaseLine,
                %[{var,8,'Arg1'},{var,8,'Arg2'}],
                Header,
                [],
                [{call,BaseLine+1,
                        {remote,BaseLine+1,
                            {atom,BaseLine+1,luam},{atom,BaseLine+1,one_call}
                        },
                        [
                            %{string,BaseLine+1,"crutas.lua"},
                            LuaModABS,
                            {string,BaseLine+1,atom_to_list(Name)},
                            %{cons,9,
                            %    {var,9,'Arg1'},
                            %    {cons,9,{var,9,'Arg2'},{nil,9}}}
                            CallArgs
                        ]}]}]}.

call_args([], Line) ->
    {nil, Line};
call_args([Arg|Args], Line) ->
    {cons, Line, {var, Line, Arg}, call_args(Args, Line)}.

get_lua_mod_expr(ModAtom, Eval) ->
    % Allow shortcut {priv, FileRelativeToPrivDir}
    Mod = atom_to_list(ModAtom),
    LuaModExpr = case Eval of
        {priv, File} -> 
            "filename:join(filename:dirname(filename:dirname(code:which("
            ++ Mod ++ "))), \"priv\") ++ \"" ++ File ++ "\".";
        _ when is_list(Eval) ->
            binary_to_list(iolist_to_binary(re:replace(Eval, "\\?MODULE", Mod)))
    end,

    %io:format("LuaModExpr: ~p~n", [LuaModExpr]),
    {ok, T, _} = erl_scan:string(LuaModExpr),
    case erl_parse:parse_exprs(T) of
        {ok, [M]} -> M;
        {ok, [_|_]} -> error(one_expression_allowed_in_implemented_in);
        {error, E} -> error({syntax_error_in_implemented_in_header, E})
    end.
