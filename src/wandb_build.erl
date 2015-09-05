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

-module(wandb_build).

-export([new/1,
         insert/4,
         insert/3,
         remove/2,
         remove/3,
         commit/1,
         flatten/1]).

-record(wandb,{module,dict}).

new(Module)
  when is_atom(Module) ->
    Dict = dict:new(),
    #wandb{module=Module,dict=Dict}.

commit(#wandb{module=Module}=DB) ->
    Terms = flatten(DB),
    wandb:compile_and_load(Module,Terms).

flatten(#wandb{dict=Dict}) ->
    Fun = fun(Func,KVDict,Acc) ->
                  [{Func,dict:to_list(KVDict)}|Acc]
          end,
    dict:fold(Fun,[],Dict).

insert(#wandb{dict=Dict}=DB,Function,KVs)
  when is_atom(Function) ->
    NewDict = '_insert'(Dict,Function,KVs),
    DB#wandb{dict=NewDict}.

'_insert'(Dict,Function,KVs) ->
    Fun = fun({K,V},Acc) -> dict:store(K,V,Acc) end,
    case dict:find(Function,Dict) of
        {ok,KVDict} ->
            NewKVDict = lists:foldl(Fun,KVDict,KVs),
            dict:store(Function,NewKVDict,Dict);
        error ->
            NewKVDict = lists:foldl(Fun,dict:new(),KVs),
            dict:store(Function,NewKVDict,Dict)
    end.

insert(#wandb{dict=Dict}=DB,Function,Key,Value)
  when is_atom(Function) ->
    NewDict = '_insert'(Dict,Function,Key,Value),
    DB#wandb{dict=NewDict}.

'_insert'(Dict,Function,Key,Value) ->
    case dict:find(Function,Dict) of
        {ok,KVDict} ->
            NewKVDict = dict:store(Key,Value,KVDict),
            dict:store(Function,NewKVDict,Dict);
        error ->
            NewKVDict = dict:store(Key,Value,dict:new()),
            dict:store(Function,NewKVDict,Dict)
    end.

remove(#wandb{dict=Dict}=DB,Function,Key)
  when is_atom(Function) ->
    NewDict = '_remove'(Dict,Function,Key),
    DB#wandb{dict=NewDict}.

'_remove'(Dict,Function,Key) ->
    case dict:find(Function,Dict) of
        {ok,KVDict} ->
            NewKVDict = dict:erase(Key,KVDict),
            dict:store(Function,NewKVDict,Dict);
        error ->
            Dict
    end.

remove(#wandb{dict=Dict}=DB,Function)
  when is_atom(Function) ->
    NewDict = dict:erase(Function,Dict),
    DB#wandb{dict=NewDict}.
