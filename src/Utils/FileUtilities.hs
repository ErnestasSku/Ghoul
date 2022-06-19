module Utils.FileUtilities
  ( ghoulFile,
    getRecursivePaths,
    predicateFind,
    findGdFiles,
    findGdFiles',
  )
where

import Rules.Rules (RuleQuestionnaire)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)
import System.FilePath (takeExtension, (</>))
import Control.Monad (forM)
import Parsing.GhoulFile (parseRules)


-- | Reads the rules.ghoul file
ghoulFile :: IO [RuleQuestionnaire]
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