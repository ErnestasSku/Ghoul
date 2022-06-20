{-# LANGUAGE ScopedTypeVariables #-}

module Utils.ArgUtils
  (
      ArgCommands(..)
    , ArgSettings(..)
    , Arguments(..)
    , buildArguments
  )
where

import Control.Applicative ((<|>))
import PrettyPrint.Pretty (Output(..))
import Data.Maybe ( fromJust, isJust )

class ArgDict a where
  argNames :: a -> [(String, a)]
  matchArg :: String -> Maybe a

data ArgCommands = Init | Version | Help | RunAll
  deriving (Show)

data ArgSettings = Verbose | EditorInput [String]
  deriving (Show, Eq)

-- FutureNote: Could be done more cleaner
instance ArgDict ArgCommands where
  argNames Init = [(x, Init) | x <- ["Init", "init"]]
  argNames Version = [(x, Version) | x <- ["Version", "version"]]
  argNames Help = [(x, Help) | x <- ["Help", "help"]]
  argNames RunAll = [(x, RunAll) | x <- ["RunAll", "runAll", "runall"]]

  matchArg s =
    let
      init' = lookup s $ argNames Init
      ver' = lookup s $ argNames Version
      help' = lookup s $ argNames Help
      run' = lookup s $ argNames RunAll
    in
      init' <|> ver' <|> help' <|> run'

instance ArgDict ArgSettings where
  argNames Verbose = [(x, Verbose) | x <- ["-v", "-verbose", "-Verbose"]]
  argNames (EditorInput _) = [(x, EditorInput []) | x <- ["-editorArgs"]]

  matchArg s =
    let
      verb' = lookup s $ argNames Verbose
      edit' = lookup s $ argNames (EditorInput [])
    in
      verb' <|> edit'

data Arguments = Arguments
  {
      argOutputMode :: Output ()
    , argCommands :: [ArgCommands]
    , argSettings :: [ArgSettings]
  }
  deriving (Show)

data ArgWrapper = Comm ArgCommands | Sett ArgSettings  | Outp (Output ())
  deriving (Show)

getArgType :: String -> ArgWrapper
getArgType s
    | isJust i = Comm $ fromJust i
    | isJust j = Sett $ fromJust j
    | isJust k = Outp $ fromJust k
    | otherwise = error ""
  where
    matchOutput :: String -> Maybe (Output ())
    matchOutput s
      | s == "Terminal" || s == "terminal" = Just $ Terminal ()
      | s == "Editor"  || s == "editor" = Just $ Editor ()
      | s == "Plain" || s == "plain" = Just $ Plain ()
      | otherwise = Nothing
    i = matchArg s :: Maybe ArgCommands
    j = matchArg s :: Maybe ArgSettings
    k = matchOutput s



emptyArgumentList :: Arguments
emptyArgumentList = Arguments{argOutputMode = Terminal (), argCommands = [], argSettings = []}

-- FutureNote: refactor records with record dot syntax which was introduced in Haskell 9.2 (after migration)
buildArguments :: [String] -> Arguments
buildArguments s = buildArgumentsHelper s emptyArgumentList
  where
    buildArgumentsHelper :: [String] -> Arguments -> Arguments
    buildArgumentsHelper (x:xs) args = case getArgType x of
      Comm ac -> buildArgumentsHelper xs args{argCommands = ac : argCommands args}
      Sett (EditorInput []) -> args{argSettings = EditorInput xs : argSettings args}
      Sett as -> buildArgumentsHelper xs args{argSettings = as : argSettings args}
      Outp out -> buildArgumentsHelper xs args{argOutputMode = out}
    buildArgumentsHelper [] args = args


