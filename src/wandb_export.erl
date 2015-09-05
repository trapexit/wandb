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

-module(wandb_export).

-export([kv/1,
         dict/1]).

kv(Module) ->
    Funcs = functions(Module),
    [{Func,[{Key,Module:Func(Key)} ||
               Key <- Module:'#keys'(Func)]} ||
        Func <- Funcs].

dict(Module) ->
    Funcs = functions(Module),
    Fun = fun(Func,Dict) ->
                  Keys = Module:'#keys'(Func),
                  NewKVDict = kvdict(Module,Func,Keys),
                  dict:store(Func,NewKVDict,Dict)
          end,
    lists:foldl(Fun,dict:new(),Funcs).

%% Private
kvdict(Module,Func,Keys) ->
    Fun = fun(Key,KVDict) ->
                  Value = Module:Func(Key),
                  dict:store(Key,Value,KVDict)
          end,
    lists:foldl(Fun,dict:new(),Keys).

functions(Module) ->
    Funcs = erlang:get_module_info(Module,exports),
    filter_functions(Funcs).

filter_functions([{'#keys',_}|Funcs]) ->
    filter_functions(Funcs);
filter_functions([{'module_info',_}|Funcs]) ->
    filter_functions(Funcs);
filter_functions([{Mod,_}|Funcs]) ->
    [Mod|filter_functions(Funcs)];
filter_functions([]) ->
    [].
