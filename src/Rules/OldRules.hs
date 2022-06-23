{-
  Note used as a reference.
  Will be deleted later
-}

module Rules.OldRules
  ( 
    rulesList,
    RuleFunction,
    Rule (..),
    RuleQuestionnaire (..),
    fromRulesToFunc,
    fromRulesToStr,
    defaultRules
  )
where

import Parsing.AST (AST)
import qualified Parsing.AST
import Rules.CommentChecker (commentCheck)
import Rules.TypeChecker (typeCheck)
import Rules.OrderChecker (properOrder)
import Rules.NoDeepNodesChecker (deepNode)

type RuleFunction = [AST] -> [(Int, String)]

{-
  Possible to create new typeclass? Which would could map Rules names to rule descriptions.
  It would have two functions: normal, and detailed. Normal would print only the names of the functions
  Detailed would map function name to description and would print that.
-}

data Rule
  = StaticTypes
  | ProperComments
  | ProperOrdering
  | DeepNode
  deriving (Show)

data RuleQuestionnaire = RuleQ Rule Bool
  deriving (Show)

-- #TODO: this does not seem correct at the moment. 
rulesList :: [RuleFunction]
rulesList = [typeCheck, commentCheck, commentCheck, deepNode]

-- | Default rules for typechecking
defaultRules :: IO [RuleQuestionnaire]
defaultRules =
  return
    [ RuleQ StaticTypes True,
      RuleQ ProperComments True,
      RuleQ ProperOrdering True,
      RuleQ DeepNode True
    ]

fromRulesToFunc :: [RuleQuestionnaire] -> [RuleFunction]
fromRulesToFunc (x : xs) = case x of
  RuleQ StaticTypes True -> typeCheck : fromRulesToFunc xs
  RuleQ ProperComments True -> commentCheck : fromRulesToFunc xs
  RuleQ ProperOrdering True -> properOrder : fromRulesToFunc xs
  RuleQ DeepNode True -> deepNode : fromRulesToFunc xs
  _ -> fromRulesToFunc xs
fromRulesToFunc [] = []

fromRulesToStr :: [RuleQuestionnaire] -> [String]
fromRulesToStr (x : xs) = case x of
  RuleQ StaticTypes v -> ("\tStaticTyping = " ++ show v ++ "\n") : fromRulesToStr xs
  RuleQ ProperComments v -> ("\tProperComments = " ++ show v ++ "\n") : fromRulesToStr xs
  RuleQ ProperOrdering v -> ("\tProperOrdering = " ++ show v ++ "\n") : fromRulesToStr xs
  RuleQ DeepNode v -> ("\tDeepNode = " ++ show v ++ "\n") : fromRulesToStr xs
fromRulesToStr [] = []
