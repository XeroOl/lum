# Lum
Lum is a custom programming language made by me that compiles to lua.
It has two parts to it, a lua library, `lum.lua`, and a compiler, `lumc`.

I created lum because I had complaints about lua, namely a lack of `+=`, `-=`, and keywords making code long, like `end` and `function`.
Lum uses `{}` curly braces for blocks of code, and requires semicolons on each line.


To install, put `lum.lua` somewhere in your lua path, and put `lumc` somewhere in your executable path.

See [example.lum](./example.lum) for example lum code.

You can use lumc like this
```
# run a file
lumc filename.lum -r

# compile a file to lua
lumc filename.lum -o filename.lua

# preview generated lua code
lumc filename.lum
```

The lum library can be loaded from lua or lum using `require`.
Requiring the lum library gives you `lum.loadfile`, `lum.dofile`, `lum.loadstring`, `lum.dostring`, and the ability to require `.lum` files.

DEPENDENCIES:

`lum.lua` and `lumc` need to have `lpeglabel` installed. You can get lpeglabel with luarocks or something.
also, lumc is set up to run with luajit, but you can change it to whatever you want with the shebang.
I've only tested lum with 5.1 and luajit.
