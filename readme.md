# Ghoul

Gdscript Haskell Opinionated Unconventional Linter

This is a linting/style enforcing program written in Haskell, mainly for my own personal use, and therefore it is opinionated on how code should be written.

## Basic commands

```console
ghoul           | Runs linter based on rules.ghoul file
ghoul init      | Initialized rules.ghoul file
ghoul runAll    | Ignores rules.ghoul file and runs all rules
ghoul version   | Displays the current version of Ghoul
```

This program runs from working directory, therefore, it is possible (and even recomended) to add it to path, for more convenient usage.

## Enforced rules

| Name | Description | Additional information |
| ---- | ----------- | ---------------------- |
| [StaticTyping](https://github.com/ErnestasSku/Ghoul/blob/main/src/Rules/TypeChecker.hs) |Enforces static typing on functions and variables | [Godot docs](https://docs.godotengine.org/en/stable/tutorials/scripting/gdscript/static_typing.html?highlight=static%20typing)  about static typing |
| [ProperComments](https://github.com/ErnestasSku/Ghoul/blob/main/src/Rules/CommentChecker.hs) | Enforces a proper comment style (at the moment this function can't tell a difference apart from a poorly formatted comment and commented out code. (If you plan on commenting out a lot of code, it is recommended to disable this rule) | [Godot docs](https://docs.godotengine.org/en/stable/tutorials/scripting/gdscript/gdscript_styleguide.html#comment-spacing) on commenting style
| [ProperOrdering](https://github.com/ErnestasSku/Ghoul/blob/main/src/Rules/OrderChecker.hs) | Enforces a recommended ordering of code in a file. Note: it mostly works with elements 05 - 11 (see Godot docs for information about elements) | [Godot docs](https://docs.godotengine.org/en/stable/tutorials/scripting/gdscript/gdscript_styleguide.html#code-order) on recommended code ordering.

## How rules are defined

rules follow the below format. It gets an AST (from tokenizing) and it returns a list of integers and strings.

The integer value is the line of the code.

The string is the error message.

```Haskell
rule :: [AST] -> [(Int, String)]
```

## Known issues

At the moment I'm using Pretty library for colorful output, however, it doesn't not work on Windows command line, as it does not support ANSI escape color characters.