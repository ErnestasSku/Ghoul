module Rules.TypeChecker (typeCheck) where

import Utilities
import Data.Maybe (isJust)
import Parsing.AST



typeCheck :: [AST] -> [(Int, String)]
typeCheck (x:xs) = case x of
  Keyword s (line, col) ->
      if s == "var" || s == "const"
        then
        let
           opList = filter (<! line) xs
           boolOpList = map ((\x -> x == ":" || x == ":=") . fromASTtoString) opList
        in
            if or boolOpList
                then typeCheck xs
                else (line, "Variable is not statically typed") : typeCheck xs
        else
      if s == "func"
          then
            let
                opList = filter (<! line) xs
                boolOpList = map ((== "->") . fromASTtoString) opList
            in
                if or boolOpList then typeCheck xs
                else (line, "Function is not statically typed") : typeCheck xs
          else typeCheck xs
  _ -> typeCheck xs
typeCheck [] = []