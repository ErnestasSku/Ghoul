module Rules.CommentChecker (commentCheck) where

import Parsing.AST

commentCheck :: [AST] -> [(Int, String)]
commentCheck (x : xs) = case x of
  Comment s (line, col) ->
    -- Note: there could be more white space characters. Write another function which would deal with it
    if head s == ' ' || head s == '\t'
      then commentCheck xs
      else (line, "A proper comment should have a space at the beginning") : commentCheck xs
  _ -> commentCheck xs
commentCheck [] = []