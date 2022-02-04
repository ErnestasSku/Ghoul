{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Lib
-- import System.Console.Rainbow
import Control.Monad (forM, forM_)
import Data.Foldable (concatMap)
import Data.List (intersperse, intercalate)
import qualified Data.Text.IO as T
-- import Data.Version
import Parsing.Parser (parseAll)
-- import Paths_xmonad (version)
import Rules.Rules (rulesList)
import Rules.TypeChecker (typeCheck)
import System.Console.Pretty
import System.Directory ( getCurrentDirectory, doesDirectoryExist, listDirectory )
import System.Environment ( getArgs )
import System.FilePath ( takeExtension, (</>) )
import Text.Parsec (parse)
import Utilites (createOutputString, (<->))


version :: String
version = "Ghoul 0.2.0"

main :: IO ()
main = do
    args <- getArgs
    mainArgs args

-- #TODO I think this is not the best way of dealing with args. Rewrite later 
mainArgs :: [String] -> IO ()
mainArgs [] = defaultRun
mainArgs ["init"] = mainInit
mainArgs ["version"] = mainVersion
mainArgs _ = defaultRun

mainInit :: IO ()
mainInit = putStrLn "To be implemented"

mainVersion :: IO ()
mainVersion = do
  colorSupport <- supportsPretty
  if colorSupport
    then putStrLn (color Yellow version)
    else putStrLn version

defaultRun :: IO ()
defaultRun = do
  cwd <- getCurrentDirectory
  files <- findGdFiles cwd

  result <- forM files $ \file -> do
    input <- readFile file
    let res = parse parseAll file input
    case res of
      Left err -> return (file, [])
      Right val -> return (file, val)

  output <- forM result $ \(file', ast) -> do
    let mapped = map ($ ast) rulesList
    let collapsed = concat mapped
    return $ createOutputString collapsed (cwd <-> file') ast ++ ["\n"]

  putStrLn $ intercalate "" $ concat output


-- example
example :: IO ()
example = do
  -- simple style
  putStrLn ( style Underline "hello there!" )

  -- simple color
  putStrLn ( color Yellow "this lib was designed to be easy" )

  -- simple background
  putStrLn ( bgColor Blue "and the functions layer together easily" )

  -- combining
  putStrLn ( bgColor White . color Red . style Bold $ "like so!" )

  -- custom style & color
  let primary = bgColor Magenta . color Green . style Italic
  putStrLn ( primary "easily create your own helpers & styles" )

  -- with both unicode string types
  putStrLn ( color Cyan "String...")
  T.putStrLn (color Red "and Text")

  -- set styling to none
  putStrLn ( primary $ style Normal "or if you need to remove any styling..." )

  let a = color Green "asd"
  let b = color Yellow "bd"
  putStrLn $ a <> b

  putStrLn $ color Yellow "asd " <> color Red "a"

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