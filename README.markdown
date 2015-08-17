Ordeal automates the following:

```shell
$ stack ghci myproject
...
ghci> import Main
ghci> Main.main
-- listening on localhost:3000
-- edit some files...
Ctrl-c
ghci> :reload
ghci> import Main
ghci> Main.main
-- repeat
```

Ordeal might be anagram of reload.

## Installation

```
$ git clone git@github.com:reiddraper/ordeal.git
$ cd ordeal
$ stack build && stack install
```

## Usage

To use Ordeal, you'll need a `.ordeal` file, which has the commands to run your
project.

```shell
$ cat .ordeal
import Main
Main.main

# examples
$ ordeal # defaults to `stack ghci`
$ ordeal stack ghci myproject
$ ordeal cabal repl
```
