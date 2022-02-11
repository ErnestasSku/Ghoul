{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM, forM_)
import Data.Foldable (concatMap)
import Data.List (intercalate, intersperse)
import qualified Data.Text.IO as T
import Parsing.GhoulFile (parseRules)
import Parsing.Parser (parseAll)
import Rules.Rules (Rule (..), RuleFunction, RuleQuestionaire (..), fromRulesToFunc, fromRulesToStr, rulesList)
import Rules.TypeChecker (typeCheck)
import System.Console.Pretty
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)
import System.Environment (getArgs)
import System.FilePath (takeExtension, (</>))
import Text.Parsec (parse, putState)
import Utilites (createOutputString, (<->))

version :: String
version = "Ghoul 0.2.0"

main :: IO ()
main = do
  args <- getArgs
  mainArgs args

-- #TODO I think this is not the best way of dealing with args. Rewrite later
mainArgs :: [String] -> IO ()
mainArgs [] = do
  rules <- ghoulFile
  print rules
  sophisitcatedRun $ fromRulesToFunc rules
mainArgs ["init"] = mainInit
mainArgs ["version"] = mainVersion
mainArgs ["runAll"] = defaultRun
mainArgs _ = defaultRun

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
      rl <- if c then initQuestioniare else defaultRules

      let msg = concat $ "[Rules]\n" : fromRulesToStr rl
      writeFile ghoulFile msg

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

-- | Default rules for typechecking
defaultRules =
  return
    [ RuleQ StaticTypes True,
      RuleQ ProperComments True,
      RuleQ ProperOrdering True
    ]

-- | Asks question for which rules to use
initQuestioniare :: IO [RuleQuestionaire]
initQuestioniare = do
  putStrLn "Use static checking? (Y/N)"

  c <- getYesNo
  staticTypes <- if c then return $ RuleQ StaticTypes True else return $ RuleQ StaticTypes False

  putStrLn "Use proper comment checking? (Y/N)"
  c <- getYesNo
  comments <- if c then return $ RuleQ ProperComments True else return $ RuleQ ProperComments False

  return [staticTypes, comments]

-- | Prints current version of the program
mainVersion :: IO ()
mainVersion = do
  colorSupport <- supportsPretty
  if colorSupport
    then putStrLn (color Yellow version)
    else putStrLn version

-- | Runs with default rules
defaultRun :: IO ()
defaultRun = sophisitcatedRun rulesList

-- | Runs with specific rule list
sophisitcatedRun :: [RuleFunction] -> IO ()
sophisitcatedRun definedRules = do
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
    return $ createOutputString collapsed (cwd <-> file') ast ++ ["\n"]

  putStrLn $ intercalate "" $ concat output

-- | Reads the rules.ghoul file
ghoulFile :: IO [RuleQuestionaire]
ghoulFile = do
  cwd <- getCurrentDirectory
  input <- readFile $ cwd </> "rules.ghoul"
  let rules = parseRules input "rules.ghoul"
  print rules
  case rules of 
    Left err -> return []
    Right val -> return val


-- | Recursively finds all files
getRecursivePaths :: FilePath -> IO [FilePath]
getRecursivePaths topPath = do
  names <- listDirectory topPath
  paths <- forM names $ \name -> do
    let path = topPath </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursivePaths path
      else return [path]
  return (concat paths)

predicateFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
predicateFind p path = do
  names <- getRecursivePaths path
  return $ filter p names

findGdFiles :: FilePath -> IO [FilePath]
findGdFiles = predicateFind (\p -> takeExtension p == ".gd")

findGdFiles' :: IO [FilePath]
findGdFiles' = do
  cwd <- getCurrentDirectory
  predicateFind (\p -> takeExtension p == ".gd") cwd