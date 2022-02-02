module Rules.Rules where

import Rules.TypeChecker (typeCheck)
import Rules.CommentChecker (commentCheck)
import qualified Parsing.AST

rulesList :: [[Parsing.AST.AST] -> [(Int, String)]]
rulesList = [typeCheck, commentCheck]