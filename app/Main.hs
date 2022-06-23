{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import System.Environment (getArgs)
import PrettyPrint.Pretty (Output(..), Pretty (..), Color (..), colorList)
import PrettyPrint.Styles (defaultTheme1, listOfStyleFields, createStyle)
import Data.Maybe
import Data.List
import Utils.ArgUtils
import Control.Monad (forM, forM_, when)
import Rules.Rules (RuleType(..), CompoundRule (..), Rule(..))
import Utils.Utilities (getYesNo, getNumberInput, getNumberInputRange)
import Utils.FileUtilities


emptySpace = "    "

main :: IO ()
main = do
  cliArgs <- getArgs

  let argRecord = buildArguments cliArgs
  --Note debugging
  print argRecord
  if not (null (argCommands argRecord))
    then forM_ (argCommands argRecord) (runCommands argRecord)
    else defaultRun argRecord

  where
    runCommands :: Arguments -> ArgCommands -> IO ()
    runCommands args@Arguments {argOutputMode=mode, argSettings=settings} Init = initialization mode settings
    runCommands args@Arguments {argOutputMode=mode, argSettings=settings} Version = undefined
    runCommands args@Arguments {argOutputMode=mode, argSettings=settings} Help = helpCommand mode settings
    runCommands args@Arguments {argOutputMode=mode, argSettings=settings} RunAll = undefined

    defaultRun :: Arguments -> IO ()
    defaultRun = undefined




initialization :: Output () -> [ArgSettings] -> IO ()
initialization (Terminal ()) sett = do
  alreadyExists <- ghoulFileExists
  if alreadyExists 
    then do
      putStrLn $ termWrapper (Yellow ,ghoulFileNameExtension <> " file already exists")
      putStrLn $ termWrapper (Red, "Do you want to delete the while?")
      putStrLn "y - will delete the file (if force argument was used it will reinitialize without terminating)\nn - will terminate the program"
      delFile <- getYesNo
      when delFile $ do 
          deleteGhoulFile
          when force $ initialization (Terminal ()) sett
    else do
    putStrLn $ termWrapper (Cyan, "Do you want to use custom rules? (y/n)\ny - will initialize rule questionnaire\nn - will use all rules as default")
    custRul <- getYesNo -- custRul = Custom Rules
    rules <- if custRul 
      then mapM genFunctionInteractive (ruleList :: [Rule]) 
      else genFunctionPassive

    putStrLn $ termWrapper (Green, "Do you want to use a custom output style? (y/n)\ny - will initialize style questionnaire\nn - will use a default style")
    custStyl <- getYesNo -- custStyl = Custom Style
    style <- if custStyl
      then do
        putStrLn "Note that you can only chose built in colors during init via terminal"
        
        res1 <- forM (init listOfStyleFields) \x -> do
          putStrLn $ termWrapper(Yellow, "===== " <> x <> " =====")
          putStrLn colorOptions
          colorNum <- getNumberInputRange 1 ((fromIntegral . length) colorList)
          return (x, colorNum)
        -- Separator field is special
        res2 <- do 
          putStrLn $ termWrapper(Yellow, "===== " <> last listOfStyleFields <> " =====")
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
initialization (Editor ()) sett = undefined
initialization (Plain ()) sett = do
  alreadyExists <- ghoulFileExists
  if alreadyExists 
    then do
      putStrLn $ termWrapper (Yellow ,ghoulFileNameExtension <> " file already exists")
      putStrLn $ termWrapper (Red, "Do you want to delete the while?")
      putStrLn "y - will delete the file (if force argument was used it will reinitialize without terminating)\nn - will terminate the program"
      delFile <- getYesNo
      when delFile $ do 
          deleteGhoulFile
          when force $ initialization (Terminal ()) sett
    else do
    putStrLn $ termWrapper (Cyan, "Do you want to use custom rules? (y/n)\ny - will initialize rule questionnaire\nn - will use all rules as default")
    custRul <- getYesNo -- custRul = Custom Rules
    rules <- if custRul 
      then mapM genFunctionInteractive (ruleList :: [Rule]) 
      else genFunctionPassive

    putStrLn $ termWrapper (Green, "Do you want to use a custom output style? (y/n)\ny - will initialize style questionnaire\nn - will use a default style")
    custStyl <- getYesNo -- custStyl = Custom Style
    style <- if custStyl
      then do
        putStrLn "Note that you can only chose built in colors during init via terminal"
        
        res1 <- forM (init listOfStyleFields) \x -> do
          putStrLn $ termWrapper(Yellow, "===== " <> x <> " =====")
          putStrLn colorOptions
          colorNum <- getNumberInputRange 1 ((fromIntegral . length) colorList)
          return (x, colorNum)
        -- Separator field is special
        res2 <- do 
          putStrLn $ termWrapper(Yellow, "===== " <> last listOfStyleFields <> " =====")
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
