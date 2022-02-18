module Rules.NoDeepNodesChecker (deepNode) where

import Parsing.AST

deepNode :: [AST] -> [(Int, String)]
deepNode s = deepNode' s False

deepNode' :: [AST] -> Bool -> [(Int, String)]
deepNode' (x : xs) deep = case x of
  Keyword "func" (line, pos) -> deepNode' xs True
  Identifier s (line, pos) ->
    if deep && head s == '$'
      then (line, "Get node should be at the top of the file") : deepNode' xs deep
      else deepNode' xs deep
  _ -> deepNode' xs deep
deepNode' [] _ = []