module Rules.Rules
  ( rulesList,
    RuleFunction,
    Rule (..),
    RuleQuestionaire (..),
    fromRulesToFunc,
    fromRulesToStr,
  )
where

import Parsing.AST (AST)
import qualified Parsing.AST
import Rules.CommentChecker (commentCheck)
import Rules.TypeChecker (typeCheck)

type RuleFunction = [AST] -> [(Int, String)]

data Rule
  = StaticTypes
  | ProperComments
  deriving (Show)

data RuleQuestionaire = RuleQ Rule Bool
  deriving (Show)

rulesList :: [RuleFunction]
rulesList = [typeCheck, commentCheck]

fromRulesToFunc :: [RuleQuestionaire] -> [RuleFunction]
fromRulesToFunc (x : xs) = case x of
  RuleQ StaticTypes True -> typeCheck : fromRulesToFunc xs
  RuleQ ProperComments True -> commentCheck : fromRulesToFunc xs
  _ -> fromRulesToFunc xs
fromRulesToFunc [] = []

fromRulesToStr :: [RuleQuestionaire] -> [String]
fromRulesToStr (x : xs) = case x of
  RuleQ StaticTypes v -> ("\tStaticTyping = " ++ show v ++ "\n") : fromRulesToStr xs
  RuleQ ProperComments v -> ("\tProperComments = " ++ show v ++ "\n") : fromRulesToStr xs
fromRulesToStr [] = []
