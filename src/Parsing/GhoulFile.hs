module Parsing.GhoulFile (parseRules) where

import Control.Monad
import Rules.Rules (Rule (..), RuleQuestionaire (..))
import Text.ParserCombinators.Parsec

parseRules :: String -> FilePath -> Either ParseError [RuleQuestionaire]
parseRules s f = parse p f s

p = parseSignature >> many (try parseRule)

parseSignature :: Parser String
parseSignature = do
  _ <- many space
  s <- string "[Rules]"
  notFollowedBy alphaNum
  return s

parseRule :: Parser RuleQuestionaire
parseRule = do
  _ <- many space
  s <- many alphaNum

  _ <- many space
  _ <- char '='
  _ <- many space
  bool <- parseFalse <|> parseTrue
  return $ RuleQ (fromStringToRule s) bool

parseTrue :: Parser Bool
parseTrue = do
  _ <- string "True"
  notFollowedBy alphaNum
  return True

parseFalse :: Parser Bool
parseFalse = do
  _ <- string "False"
  notFollowedBy alphaNum
  return False

fromStringToRule :: String -> Rule
fromStringToRule "StaticTyping" = StaticTypes
fromStringToRule "ProperComments" = ProperComments
fromStringToRule "ProperOrdering" = ProperOrdering