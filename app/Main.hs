{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM, forM_)
import Data.Foldable (concatMap)
import Data.List (intercalate, intersperse)
import qualified Data.Text.IO as T
import Parsing.Parser (parseAll)
import Rules.Rules (Rule (..), RuleFunction, RuleQuestionnaire (..), fromRulesToFunc, fromRulesToStr, rulesList, defaultRules)
import Rules.TypeChecker (typeCheck)
import System.Environment (getArgs)
import Text.Parsec (parse, putState)
import Utilities (createOutputString, (<->))
import System.Directory (getCurrentDirectory, doesFileExist)
import System.FilePath ((</>), takeExtension)
import FileUtilities (ghoulFile)


version :: String
version = "Ghoul 0.2.0"

--- ========== Main functions ==========
main :: IO ()
main = do
  args <- getArgs
  mainArgs args

-- #TODO I think this is not the best way of dealing with args. Rewrite later
mainArgs :: [String] -> IO ()
mainArgs ["init":args] = mainInit
mainArgs ["version":args] = args
mainArgs ["runAll":args] = defaultRun
mainArgs [args] = defaultRun
mainArgs [] = do
  rules <- ghoulFile
  print rules -- #TODO: remove later
  sophisticatedRun $ fromRulesToFunc rules

-- mainArgs [] = do
--   rules <- ghoulFile
--   print rules
--   sophisticatedRun $ fromRulesToFunc rules
-- mainArgs ["init"] = mainInit
-- mainArgs ["version"] = mainVersion
-- mainArgs ["runAll"] = defaultRun
-- mainArgs _ = defaultRun

-- | Initializes rules.ghoul file
mainInit :: IO ()
mainInit = do
  cwd <- getCurrentDirectory
  let ghoulFile = cwd </> "rules.ghoul"
  x <- doesFileExist ghoulFile

  if x
    then putStrLn "rules.ghoul file already exists"
    else do
      putStrLn "Generate a custom rule set? (Y/N)"
      c <- getYesNo
      rl <- if c then initQuestionnaire else defaultRules

      let msg = concat $ "[Rules]\n" : fromRulesToStr rl
      writeFile ghoulFile msg

-- | Prints current version of the program
mainVersion :: IO ()
mainVersion = do
  putStrLn version

-- | Runs with default rules
defaultRun :: IO ()
defaultRun = sophisticatedRun rulesList

-- | Runs with specific rule list
sophisticatedRun :: [RuleFunction] -> IO ()
sophisticatedRun definedRules = do
  cwd <- getCurrentDirectory
  files <- findGdFiles cwd

  result <- forM files $ \file -> do
    input <- readFile file
    let res = parse parseAll file input
    case res of
      Left err -> return (file, [])
      Right val -> return (file, val)

  output <- forM result $ \(file', ast) -> do
    let mapped = map ($ ast) definedRules
    let collapsed = concat mapped

    return $ createOutputString collapsed (cwd <-> file') ast

  let filtered = filter (not . null) output
  -- putStrLn $ intercalate "" $ concat output
  print filtered

--- ========== End of Main functions ==========


--- ========== Auxiliary functions for main functions ==========
-- | gets either Yes or No.
--  This is used to get around windows bug/requirement to press enter
-- After each char input (unlike behavior in linux)
getYesNo :: IO Bool
getYesNo = do
  c <- getChar
  case c of
    'Y' -> return True
    'y' -> return True
    'N' -> return False
    'n' -> return False
    _ -> getYesNo

{- #TODO: This is very imperative way of doing this. 
   There probably is a better way.

   [("Use static checking?", StaticTypes)] --Having a tuple of Strings and rules could be easier and more universal

-}
-- | Asks question for which rules to use
initQuestionnaire :: IO [RuleQuestionnaire]
initQuestionnaire = do
  putStrLn "Use static checking? (Y/N)"

  c <- getYesNo
  staticTypes <- if c then return $ RuleQ StaticTypes True else return $ RuleQ StaticTypes False

  putStrLn "Use proper comment checking? (Y/N)"
  c <- getYesNo
  comments <- if c then return $ RuleQ ProperComments True else return $ RuleQ ProperComments False

  return [staticTypes, comments]