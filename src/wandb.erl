%% The MIT License (MIT)
%%
%% Copyright (c) 2015 Antonio SJ Musumeci <trapexit@spawn.link>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(wandb).

-export([compile_and_load/2,
         compile/2,
         forms/2,
         abstract/2]).

compile_and_load(Module,Terms)
  when is_atom(Module) ->
    case compile(Module,Terms) of
        {ok,Mod,Bin} ->
            Name = erlang:atom_to_list(Mod) ++ ".beam",
            case code:load_binary(Mod,Name,Bin) of
                {module,Mod} ->
                    {ok,Mod};
                {error,Reason} ->
                    {error,Reason}
            end;
        Else ->
            Else
    end.

compile(Module,Terms) ->
    Keys    = keys(Terms),
    Forms   = forms(Module,[Keys|Terms]),
    Options = [report_errors,export_all],
    compile:forms(Forms,Options).

forms(Module,Terms) ->
    Fun       = fun erl_syntax:revert/1,
    Abstracts = abstract(Module,Terms),
    lists:map(Fun,Abstracts).

abstract(Module,Funcs) ->
    abstract(Module,Funcs,[]).

abstract(Module,[{Func,Clauses}|Funcs],Acc) ->
    NewAcc = [func(Func,Clauses)|Acc],
    abstract(Module,Funcs,NewAcc);
abstract(Module,[],Acc) ->
    [module(Module)|Acc].

%% Private
module(Mod) ->
    ModuleAtom = erl_syntax:atom(module),
    ModAtom    = erl_syntax:atom(Mod),
    erl_syntax:attribute(ModuleAtom,[ModAtom]).

func(Func,Clauses) ->
    FuncAtom    = erl_syntax:atom(Func),
    ClausesTree = clauses(Clauses),
    erl_syntax:function(FuncAtom,ClausesTree).

clauses([{Key,Value}|KVs]) ->
    AbsKey   = erl_syntax:abstract(Key),
    RepKey   = replace_underscore(AbsKey),
    AbsValue = erl_syntax:abstract(Value),
    AbsKV    = erl_syntax:clause([RepKey],none,[AbsValue]),
    [AbsKV|clauses(KVs)];
clauses([]) ->
    [].

replace_underscore({tree,atom,{attr,0,[],none},'$_$'}) ->
    erl_syntax:underscore();
replace_underscore({tree,tuple,Data,List})
  when is_list(List) ->
    NewList = [replace_underscore(Elem) || Elem <- List],
    {tree,tuple,Data,NewList};
replace_underscore({tree,list,Data,{list,List,Extra}}) ->
    NewList = replace_underscore(List),
    {tree,list,Data,{list,NewList,Extra}};
replace_underscore(List)
  when is_list(List) ->
    [replace_underscore(Elem) || Elem <- List];
replace_underscore(Else) ->
    Else.

keys(FuncKVs) ->
    {'#keys','_keys'(FuncKVs)}.

'_keys'([{Func,KVs}|FuncKVs]) ->
    Keys = lists:map(fun({K,_}) -> K end,KVs),
    [{Func,Keys}|'_keys'(FuncKVs)];
'_keys'([]) ->
    [].
