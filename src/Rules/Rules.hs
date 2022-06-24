{-# LANGUAGE BangPatterns #-}
module Rules.Rules
 (
      CompoundRule (..)
    , Rule (..)
    , RuleType (..)
 )
where

import Parsing.AST (AST)
import Rules.CommentChecker (commentCheck)
import Rules.TypeChecker (typeCheck)
import Rules.OrderChecker (properOrder)
import Rules.NoDeepNodesChecker (deepNode)

type RuleFunctionType = [AST] -> [(Int, String)]

class RuleType a where
  ruleDescription :: a -> String
  ruleList :: [a]
  fullRuleList :: [a]
  ruleFunction :: a -> Maybe RuleFunctionType
  ruleFunctions :: [a] -> [Maybe RuleFunctionType]
  ruleFunctions arr = map ruleFunction arr


data Rule
  = StaticTypes
  | ProperComments
  | ProperOrdering
  | DeepNode
  deriving (Show)

instance RuleType Rule where
  ruleList = [StaticTypes, ProperComments, ProperOrdering, DeepNode]
  fullRuleList = ruleList :: [Rule]

  ruleFunction StaticTypes = Just typeCheck
  ruleFunction ProperComments = Just commentCheck
  ruleFunction ProperOrdering = Just  properOrder
  ruleFunction DeepNode  = Just deepNode


  ruleDescription StaticTypes = "Checks for static typing in the code"
  ruleDescription ProperComments = "A proper code comment starts with a space. (While commented out code doesn't have a space, leaving commented out and unused code may be not the best practice)"
  ruleDescription ProperOrdering = "Checks for a proper order in the code. It should be as follows Tool (if used) -> Signals -> Enums -> Constants -> Export variables -> Variables -> OnReady Variables -> Functions"
  ruleDescription DeepNode  = "Checks if $Node is used somewhere in the code instead of as a reference at the top."

data CompoundRule = CRule Rule Bool

instance RuleType CompoundRule where
  ruleList = map (`CRule` True) (ruleList :: [Rule])
  fullRuleList = map (`CRule` True) (ruleList :: [Rule]) ++ map (`CRule` False) (ruleList :: [Rule])

  ruleFunction (CRule rule bool) = if bool then ruleFunction rule else Nothing

  ruleDescription (CRule rule _) = ruleDescription rule
instance Show CompoundRule where
  show (CRule rule True) = show rule
  show (CRule rule False) = "no" <> show rule

instance Read CompoundRule where
  readsPrec _ s = readCompoundRule s

readCompoundRule :: String -> [(CompoundRule, String)]
readCompoundRule = concat . ruleGenerator
  where
    ruleGenerator s = map (\x -> [(x, left) | (y', left) <- lex s, y' == show x]) (fullRuleList :: [CompoundRule])


