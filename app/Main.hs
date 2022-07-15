{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import System.Environment (getArgs)
import PrettyPrint.Pretty (Output(..), Pretty (..), Color (..), colorList)
import PrettyPrint.Styles (defaultTheme1, listOfStyleFields, createStyle, applyStyle)
import Data.Maybe
import Data.List
import Utils.ArgUtils
import Control.Monad (forM, forM_, when, unless)
import Rules.Rules (RuleType(..), CompoundRule (..), Rule(..))
import Utils.Utilities (getYesNo, getNumberInput, getNumberInputRange, createOutputString, (<->))
import Utils.FileUtilities
import System.Directory (getCurrentDirectory)
import Text.Parsec
import Parsing.Parser (parseAll)
import Parsing.GhoulFile (parseStyle, parseRules)
import System.FilePath ((</>))


emptySpace = "    "

main :: IO ()
main = do
  cliArgs <- getArgs

  let argRecord = buildArguments cliArgs
  --Note debugging
  -- print argRecord
  if not (null (argCommands argRecord))
    then forM_ (argCommands argRecord) (runCommands argRecord)
    else defaultRun argRecord

  where
    runCommands :: Arguments -> ArgCommands -> IO ()
    runCommands args@Arguments {argOutputMode=mode, argSettings=settings} Init = initialization mode settings
    runCommands args@Arguments {argOutputMode=mode, argSettings=settings} Version = undefined
    runCommands args@Arguments {argOutputMode=mode, argSettings=settings} Help = helpCommand mode settings
    runCommands args@Arguments {argOutputMode=mode, argSettings=settings} RunAll = ruleRun mode ruleList

    defaultRun :: Arguments -> IO ()
    defaultRun args@Arguments {argOutputMode=mode} = do
      cwd <- getCurrentDirectory
      ghoulFileContent <- readFile (cwd </> ghoulFileNameExtension)
      let parsedRules = parseRules ghoulFileNameExtension ghoulFileContent  
      
      case parsedRules of
        Left err -> putStrLn ("Error happened during the rule reading " <> show err)
        Right rules -> ruleRun mode rules


ruleRun :: Output () -> [CompoundRule] -> IO ()
ruleRun mode rules = do
  files <- findGdFiles'

  result <- forM files $ \file -> do
    input <- readFile file
    let res = parse parseAll file input
    case res of
      Left _ -> return (file, [])
      Right val -> return (file, val)

  cwd <- getCurrentDirectory
  output <- forM result $ \(file', ast) -> do
    let mapped = concatMap (($ ast) . fromJust) (filter isJust (ruleFunctions rules))
    return $ createOutputString mapped (cwd <-> file') ast

  let filtered = filter (not . null) output
  ghoulFileContent <- readFile (cwd </> ghoulFileNameExtension)
  print (cwd </> ghoulFileNameExtension)
  print ghoulFileContent
  let parsedStyle = parseStyle ghoulFileNameExtension ghoulFileContent

  (style, successfulStyleParse) <- case parsedStyle of
    Left _ -> return (defaultTheme1, False)
    Right os -> return (os, True)

  print successfulStyleParse
  unless successfulStyleParse $ putStrLn "Failed to get style. Using default theme"

  putStrLn $ unlines $ applyStyle (head filtered) style mode



initialization :: Output () -> [ArgSettings] -> IO ()
{-
  Editor will use to access this function to initialize rules and styles.
  #TODO: 
-}
initialization (Editor ()) sett = undefined
{-
  Using when unless is better than duplicating the same exact code.
  However, it still be achieved more cleanly.
  #TODO: Improve.
-}
initialization mode sett = do
  alreadyExists <- ghoulFileExists
  if alreadyExists
    then do

      putStrLn $ termPlainColorOutput mode (Yellow ,ghoulFileNameExtension <> " file already exists")
      putStrLn $ termPlainColorOutput mode (Red, "Do you want to delete the while?")

      putStrLn "y - will delete the file (if force argument was used it will reinitialize without terminating)\nn - will terminate the program"
      delFile <- getYesNo
      when delFile $ do
          deleteGhoulFile
          when force $ initialization (Terminal ()) sett
    else do
    putStrLn $ termPlainColorOutput mode (Cyan, "Do you want to use custom rules? (y/n)\ny - will initialize rule questionnaire\nn - will use all rules as default")
    custRul <- getYesNo -- custRul = Custom Rules
    rules <- if custRul
      then mapM genFunctionInteractive (ruleList :: [Rule])
      else genFunctionPassive

    putStrLn $ termPlainColorOutput mode (Green, "Do you want to use a custom output style? (y/n)\ny - will initialize style questionnaire\nn - will use a default style")

    custStyl <- getYesNo -- custStyl = Custom Style
    style <- if custStyl
      then do
        putStrLn "Note that you can only chose built in colors during init via terminal"

        res1 <- forM (init listOfStyleFields) \x -> do
          putStrLn $ termPlainColorOutput mode (Yellow, "===== " <> x <> " =====")
          putStrLn colorOptions
          colorNum <- getNumberInputRange 1 ((fromIntegral . length) colorList)
          return (x, colorNum)
        -- Separator field is special
        res2 <- do
          putStrLn $ termPlainColorOutput mode (Yellow, "===== " <> last listOfStyleFields <> " =====")
          putStrLn colorOptions'
          colorNum <- getNumberInputRange 1 ((fromIntegral . length) colorList')
          return (last listOfStyleFields, colorNum)


        let result = res1 ++ [res2]
        print $ createStyle result
        return defaultTheme1
      else return defaultTheme1

    writeNewGhoulFile rules style

  where
    genFunctionInteractive :: Rule -> IO CompoundRule
    genFunctionInteractive rl = do
        putStrLn $ "Uses " <> show rl <> " rule? (y/n)"
        when verbose $ putStrLn (ruleDescription rl)
        CRule rl <$> getYesNo

    genFunctionPassive :: IO [CompoundRule]
    genFunctionPassive = return ruleList

    zpConv :: [(Int, Color)] -> String
    zpConv (x:xs) = (show . fst) x <> " - " <> (show . snd) x <> "\n" <> zpConv xs
    zpConv [] = ""

    colorOptions = zpConv (zip [1..] colorList)
    colorList' = colorList ++ [Default]
    colorOptions' = zpConv (zip [1..] colorList')

    verbose = Verbose `elem` sett
    force = Force `elem` sett
    colorful = colorful' mode
    colorful' (Terminal()) = True
    colorful' (Plain ()) = False
    colorful' _ = error "main - init - colorful'. Should not happen"
-- initialization (Plain ()) sett = undefined

helpCommand :: Output () -> [ArgSettings] -> IO ()
helpCommand (Terminal ()) args = do
  putStrLn $ termWrapper (Yellow, "Here are the main commands")
  forM_ (argList :: [ArgCommands]) \x -> do
    putStr $ emptySpace <> show x <> "\t\t" <> argDescription x
    putStr "\n"

  putStrLn $ termWrapper (Cyan, "Arguments which can be supplied")
  forM_ (argList :: [ArgSettings]) \x -> do
    putStr $ emptySpace <> show x <> "\t\t" <> argDescription x
    putStr "\n"

  putStrLn $ termWrapper (Yellow, "Arguments which can be supplied")
  forM_ (ruleList :: [Rule]) \x -> do
      putStr $ emptySpace <> show x <> "\t\t" <> ruleDescription x
      putStr "\n"

helpCommand (Editor ()) args = do
  -- putStrLn "Rules"
  forM_ (ruleList :: [Rule]) \x -> do
    putStrLn $ show x <> " | " <> ruleDescription x
  -- putStrLn "Style"
helpCommand (Plain ()) args = do
  putStrLn "Here are the main commands"
  forM_ (argList :: [ArgCommands]) \x -> do
    putStr $ emptySpace <> show x <> "\t\t" <> argDescription x
    putStr "\n"

  putStrLn "Arguments which can be supplied"
  forM_ (argList :: [ArgSettings]) \x -> do
    putStr $ emptySpace <> show x <> "\t\t" <> argDescription x
    putStr "\n"

  putStrLn "Arguments which can be supplied"
  forM_ (ruleList :: [Rule]) \x -> do
      putStr $ emptySpace <> show x <> "\t\t" <> ruleDescription x
      putStr "\n"


termWrapper :: (Color, String) -> String
termWrapper (col, str) = color col (Terminal str)

termPlainColorOutput :: Output () -> (Color, String) -> String
termPlainColorOutput (Terminal ()) (col, str) = color col (Terminal str)
termPlainColorOutput (Plain ()) (col, str) = color col (Plain str)