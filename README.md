### Description
wandb (or Write Almost Never DataBase) is an abuse of Erlang's ability to create abstract forms from literal native data structures and to compile them at runtime. Using this ability wandb allows the user to create Erlang modules with functions which can take a literal as an argument and return another literal. Since data in modules are in effect read only and stored globally it can provide extremely fast and concurent access to data which is rarely or never modified after the original creation.

### Usage
```
1> DB0 = wandb_build:new(testdb).
2> DB1 = wandb_build:insert(DB0,get,[{key0,value0},{key1,"value1"},{<<"key2">>,10}]).
3> wandb_build:commit(DB1).
4> testdb:get(key0).
value0
5> testdb:get(<<"key2">>).
10
6> testdb:'#keys'(get).
[key0,key1,<<"key2">>]
```

If you want to provide an underscore to the key you need to use the special atom '$_$' which will be replaced with the proper form for the unused variable '_'.

* wandb.erl: core logic
* wandb_build.erl: provides a simple api to build an intermediate datastructure which can then be compiled into a module.
* wandb_import.erl: offers some convience functions for importing data from other formats into the builder format. Currently supporting only zip files.
* wandb_export.erl: gives some basic conversion from the module to dicts or KV lists.
