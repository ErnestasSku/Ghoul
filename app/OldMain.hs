{-
  Note used as a reference.
  Will be deleted later
-}

-- {-# LANGUAGE OverloadedStrings #-}

module OldMain () where

-- import Control.Monad (forM)
-- --import Data.Foldable (concatMap)
-- --import Data.List (intercalate, intersperse)
-- --import qualified Data.Text.IO as T
-- import Parsing.Parser (parseAll)
-- import Rules.OldRules (Rule (..), RuleFunction, RuleQuestionnaire (..), fromRulesToFunc, fromRulesToStr, rulesList, defaultRules)
-- import System.Environment (getArgs)
-- import Text.Parsec (parse)
-- import Utils.Utilities (createOutputString, (<->))
-- import System.Directory (getCurrentDirectory, doesFileExist)
-- import System.FilePath ((</>))
-- import Utils.FileUtilities (ghoulFile, findGdFiles)
-- import PrettyPrint.Pretty (Output(..), Pretty (..), Color(..))
-- import PrettyPrint.Styles (applyStyle, defaultTheme1)
-- import Parsing.GhoulFile (parseStyle)


-- version :: String
-- version = "Ghoul 0.2.0"



-- --- ========== Main functions ==========

-- data ArgCommands = Init | Version | Help | RunAll | DefaultRun
--   deriving (Show)

-- instance Read ArgCommands where
--   readsPrec _ s = readArgCommands s

-- -- Most likely not the best way of implementing read. 
-- readArgCommands :: String -> [(ArgCommands, String)]
-- readArgCommands s = commandList
--   where commandList = [(Init, x) | ("Init", x) <- lex s] ++ [(Init, x) | ("init", x) <- lex s]
--                    ++ [(Version, x) | ("Version", x) <- lex s] ++ [(Version, x) | ("version", x) <- lex s] ++ [(Version, x) | ("v", x) <- lex s]
--                    ++ [(Help, x) | ("Help", x) <- lex s] ++ [(Help, x) | ("help", x) <- lex s]
--                    ++ [(RunAll, x) | ("RunAll", x) <- lex s] ++ [(RunAll, x) | ("runAll", x) <- lex s] ++ [(RunAll, x) | ("runall", x) <- lex s]
--                   --  ++ [(DefaultRun, x) | ("", x) <- lex s]


-- data Arguments = Arguments 
--   {
--       argOutputMode :: Output ()
--     , argCommands :: [ArgCommands]
--   }
--   deriving (Show)


-- emptyArgs = Arguments{argOutputMode = Terminal (), argCommands = []}

-- parseArgs :: [String] -> Arguments -> Arguments
-- parseArgs (l:ls) acc = if null $ readArgCommands l
--             then parseArgs ls (acc{argOutputMode = getMode l}) 
--             else parseArgs ls (acc{argCommands = (read l :: ArgCommands) : argCommands acc })
--   where
--     getMode x 
--       | x == "Editor"   || x == "editor"   = Editor()
--       | x == "Plain"    || x == "plain"    = Plain ()
--       | x == "Terminal" || x == "terminal" = Terminal ()
--       | otherwise = error "Unknown command or mode"
-- parseArgs [] acc = acc 

-- main :: IO ()
-- main = do
--   unparsedArgs <- getArgs

--   let args = parseArgs unparsedArgs emptyArgs
--   let outputType = argOutputMode args
--   let executionCommands = argCommands args 

--   print "a"
--   -- _ <- map (runProgram outputType) executionCommands 

--   where
--     runProgram :: Output () -> ArgCommands -> IO ()
--     runProgram mode Init = mainInit mode 
--     runProgram ("init":args) mode = mainInit mode args
--     runProgram ("version":args) mode = mainVersion args mode
--     runProgram ("runAll":_) mode = defaultRun mode
--     runProgram (x:xs) mode = runProgram xs mode
--     runProgram [] mode = do
--       rules <- ghoulFile
--       flip sophisticatedRun mode $ fromRulesToFunc rules


-- -- | Initializes rules.ghoul file
-- mainInit :: Output () -> [String] -> IO ()
-- mainInit (Terminal _) _ = do
--   cwd <- getCurrentDirectory
--   let ghoulRulesFile = cwd </> "rules.ghoul"
--   x <- doesFileExist ghoulRulesFile

--   if x
--     then putStrLn "rules.ghoul file already exists"
--     else do
--       putStrLn "Generate a custom rule set? (Y/N)"
--       c <- getYesNo
--       rl <- if c then initQuestionnaire else defaultRules

--       let msg = concat $ "[Rules]\n" : fromRulesToStr rl
--       writeFile ghoulRulesFile msg
-- mainInit _ _ = putStrLn "Not implemented for editor and plain"

-- -- | Prints current version of the program
-- mainVersion :: [String] -> Output () -> IO ()
-- mainVersion _ (Terminal _) = putStrLn $ color Yellow (Terminal version)
-- mainVersion _ (Editor _) = putStrLn $ color Yellow (Editor version)
-- mainVersion _ (Plain _) = putStrLn version

-- -- | Runs with default rules
-- defaultRun :: Output () -> IO ()
-- defaultRun = sophisticatedRun rulesList

-- -- | Runs with specific rule list
-- sophisticatedRun :: [RuleFunction] -> Output () -> IO ()
-- sophisticatedRun definedRules mode = do
--   cwd <- getCurrentDirectory
--   files <- findGdFiles cwd

--   result <- forM files $ \file -> do
--     input <- readFile file
--     let res = parse parseAll file input
--     case res of
--       Left _ -> return (file, [])
--       Right val -> return (file, val)

--   output <- forM result $ \(file', ast) -> do
--     let mapped = map ($ ast) definedRules
--     let collapsed = concat mapped

--     return $ createOutputString collapsed (cwd <-> file') ast

--   let filtered = filter (not . null) output

--   st <- style

--   putStrLn $ unlines $ applyStyle (head filtered) st mode
--   where
--     style = do
--       cwd <- getCurrentDirectory
--       let ghoulRuleFile = cwd </> "rules.ghoul"
--       x <- doesFileExist ghoulRuleFile

--       if x 
--         then do
--           input <- readFile ghoulRuleFile
--           let style = parseStyle input "rules.ghoul"
--           case style of
--             Left err -> return defaultTheme1
--             Right val -> return val
--         else return defaultTheme1


-- --- ========== End of Main functions ==========


-- --- ========== Auxiliary functions for main functions ==========
-- -- | gets either Yes or No.
-- --  This is used to get around windows bug/requirement to press enter
-- -- After each char input (unlike behavior in linux)
-- getYesNo :: IO Bool
-- getYesNo = do
--   c <- getChar
--   case c of
--     'Y' -> return True
--     'y' -> return True
--     'N' -> return False
--     'n' -> return False
--     _ -> getYesNo

-- {-- #TODO: This is very imperative way of doing this. 
--    There probably is a better way.

--    [("Use static checking?", StaticTypes)] --Having a tuple of Strings and rules could be easier and more universal

-- --}
-- -- | Asks question for which rules to use
-- initQuestionnaire :: IO [RuleQuestionnaire]
-- initQuestionnaire = do
--   putStrLn "Use static checking? (Y/N)"

--   c1 <- getYesNo
--   staticTypes <- if c1 then return $ RuleQ StaticTypes True else return $ RuleQ StaticTypes False

--   putStrLn "Use proper comment checking? (Y/N)"
--   c2 <- getYesNo
--   comments <- if c2 then return $ RuleQ ProperComments True else return $ RuleQ ProperComments False

--   return [staticTypes, comments]
