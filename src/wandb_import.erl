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

-module(wandb_import).

-export([zip/2,
         zip/3,
         escript/3,
         files/4]).

zip(DB,Archive) ->
    MatchFun = fun(_) -> true end,
    Fun = zip_importer_fun(MatchFun),
    {ok,NewDB} = zip:foldl(Fun,DB,Archive),
    NewDB.

zip(DB,Archive,RE)
  when element(1,RE) =:= re_pattern ->
    MatchFun = fun(FilePath) ->
                       re:run(FilePath,RE) =/= nomatch
               end,
    Fun = zip_importer_fun(MatchFun),
    {ok,NewDB} = zip:foldl(Fun,DB,Archive),
    NewDB;
zip(DB,Archive,Prefix) ->
    MatchFun = fun(FilePath) -> prefixof(Prefix,FilePath) end,
    Fun = zip_importer_fun(MatchFun),
    {ok,NewDB} = zip:foldl(Fun,DB,{"",Archive}),
    NewDB.

escript(DB,Escript,Prefix)
  when is_list(Escript) ->
    {ok,Archive} = escript:parse_file(Escript),
    zip(DB,Archive,Prefix).

files(DB,Dir,RegExp,Recursive) ->
    Fun = files_importer_fun(Dir),
    filelib:fold_files(Dir,RegExp,Recursive,Fun,DB).

%% Private
files_importer_fun(Dir) ->
    fun(Filename,DB) ->
            Key = strip(Dir,Filename),
            FileInfo = file:read_file_info(Filename),
            NewDB = wandb_build:insert(DB,info,Key,FileInfo),
            {ok,Data} = file:read_file(Filename),
            wandb_build:insert(NewDB,data,Key,Data)
    end.

zip_importer_fun(MatchFun) ->
    fun(FilePath,GetInfo,GetBin,Acc) ->
            case MatchFun(FilePath) of
                true ->
                    NewDB = wandb_build:insert(Acc,info,FilePath,GetInfo()),
                    wandb_build:insert(NewDB,data,FilePath,GetBin());
                false ->
                    Acc
            end
    end.

strip([H|PreT],[H|StrT]) ->
    strip(PreT,StrT);
strip([],Str) ->
    Str;
strip(_,[]) ->
    [].

prefixof([H|PreT],[H|StrT]) ->
    prefixof(PreT,StrT);
prefixof([],_) ->
    true;
prefixof(_,_) ->
    false.
