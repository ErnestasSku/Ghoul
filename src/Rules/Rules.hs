module Rules.Rules
  ( rulesList,
    RuleFunction,
    Rule (..),
    RuleQuestionnaire (..),
    fromRulesToFunc,
    fromRulesToStr,
  )
where

import Parsing.AST (AST)
import qualified Parsing.AST
import Rules.CommentChecker (commentCheck)
import Rules.TypeChecker (typeCheck)
import Rules.OrderChecker (properOrder)

type RuleFunction = [AST] -> [(Int, String)]

data Rule
  = StaticTypes
  | ProperComments
  | ProperOrdering
  deriving (Show)

data RuleQuestionnaire = RuleQ Rule Bool
  deriving (Show)

rulesList :: [RuleFunction]
rulesList = [typeCheck, commentCheck]

fromRulesToFunc :: [RuleQuestionnaire] -> [RuleFunction]
fromRulesToFunc (x : xs) = case x of
  RuleQ StaticTypes True -> typeCheck : fromRulesToFunc xs
  RuleQ ProperComments True -> commentCheck : fromRulesToFunc xs
  RuleQ ProperOrdering True -> properOrder : fromRulesToFunc xs
  _ -> fromRulesToFunc xs
fromRulesToFunc [] = []

fromRulesToStr :: [RuleQuestionnaire] -> [String]
fromRulesToStr (x : xs) = case x of
  RuleQ StaticTypes v -> ("\tStaticTyping = " ++ show v ++ "\n") : fromRulesToStr xs
  RuleQ ProperComments v -> ("\tProperComments = " ++ show v ++ "\n") : fromRulesToStr xs
  RuleQ ProperOrdering v -> ("\tProperOrdering = " ++ show v ++ "\n") : fromRulesToStr xs
fromRulesToStr [] = []
