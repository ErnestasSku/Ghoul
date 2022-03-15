{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM)
--import Data.Foldable (concatMap)
--import Data.List (intercalate, intersperse)
--import qualified Data.Text.IO as T
import Parsing.Parser (parseAll)
import Rules.Rules (Rule (..), RuleFunction, RuleQuestionnaire (..), fromRulesToFunc, fromRulesToStr, rulesList, defaultRules)
import System.Environment (getArgs)
import Text.Parsec (parse)
import Utilities (createOutputString, (<->))
import System.Directory (getCurrentDirectory, doesFileExist)
import System.FilePath ((</>))
import FileUtilities (ghoulFile, findGdFiles)
import PrettyPrint.Pretty (Output(..), Pretty (..), Color(..))
import PrettyPrint.Styles (applyStyle, defaultTheme1)


version :: String
version = "Ghoul 0.2.0"



--- ========== Main functions ==========
main :: IO ()
main = do
  args <- getArgs
  let currentMode = getMode args
  print currentMode
  mainArgs args currentMode

  where
    getMode (x:xs)
      | x == "Editor" = Editor ()
      | x == "Plain" = Plain ()
      | x == "Terminal" = Terminal ()
      | otherwise = getMode xs
    getMode [] = Terminal ()

-- #TODO I think this is not the best way of dealing with args. Rewrite later
mainArgs :: [String] -> Output () -> IO ()
mainArgs ("init":args) mode = mainInit mode args
mainArgs ("version":args) mode = mainVersion args mode
mainArgs ("runAll":_) mode = defaultRun mode
mainArgs [] mode = do
  rules <- ghoulFile
  flip sophisticatedRun mode $ fromRulesToFunc rules
mainArgs _ mode = defaultRun mode


-- | Initializes rules.ghoul file
mainInit :: Output () -> [String] -> IO ()
mainInit (Terminal _) _ = do
  cwd <- getCurrentDirectory
  let ghoulRulesFile = cwd </> "rules.ghoul"
  x <- doesFileExist ghoulRulesFile

  if x
    then putStrLn "rules.ghoul file already exists"
    else do
      putStrLn "Generate a custom rule set? (Y/N)"
      c <- getYesNo
      rl <- if c then initQuestionnaire else defaultRules

      let msg = concat $ "[Rules]\n" : fromRulesToStr rl
      writeFile ghoulRulesFile msg
mainInit _ _ = putStrLn "Not implemented for editor and plain"

-- | Prints current version of the program
mainVersion :: [String] -> Output () -> IO ()
mainVersion _ (Terminal _) = putStrLn $ color Yellow (Terminal version)
mainVersion _ (Editor _) = putStrLn $ color Yellow (Editor version)
mainVersion _ (Plain _) = putStrLn version

-- | Runs with default rules
defaultRun :: Output () -> IO ()
defaultRun = sophisticatedRun rulesList

-- | Runs with specific rule list
sophisticatedRun :: [RuleFunction] -> Output () -> IO ()
sophisticatedRun definedRules mode = do
  cwd <- getCurrentDirectory
  files <- findGdFiles cwd

  result <- forM files $ \file -> do
    input <- readFile file
    let res = parse parseAll file input
    case res of
      Left _ -> return (file, [])
      Right val -> return (file, val)

  output <- forM result $ \(file', ast) -> do
    let mapped = map ($ ast) definedRules
    let collapsed = concat mapped

    return $ createOutputString collapsed (cwd <-> file') ast

  let filtered = filter (not . null) output

  putStrLn $ unlines $ applyStyle (head filtered) style mode
  where
    style = defaultTheme1


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

{-- #TODO: This is very imperative way of doing this. 
   There probably is a better way.

   [("Use static checking?", StaticTypes)] --Having a tuple of Strings and rules could be easier and more universal

--}
-- | Asks question for which rules to use
initQuestionnaire :: IO [RuleQuestionnaire]
initQuestionnaire = do
  putStrLn "Use static checking? (Y/N)"

  c1 <- getYesNo
  staticTypes <- if c1 then return $ RuleQ StaticTypes True else return $ RuleQ StaticTypes False

  putStrLn "Use proper comment checking? (Y/N)"
  c2 <- getYesNo
  comments <- if c2 then return $ RuleQ ProperComments True else return $ RuleQ ProperComments False

  return [staticTypes, comments]
