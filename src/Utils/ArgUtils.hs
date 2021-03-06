{-# LANGUAGE ScopedTypeVariables #-}

module Utils.ArgUtils
  (
      ArgCommands(..)
    , ArgSettings(..)
    , Arguments(..)
    , ArgDict (..)
    , buildArguments
  )
where

import Control.Applicative ((<|>))
import PrettyPrint.Pretty (Output(..))
import Data.Maybe ( fromJust, isJust )
import Data.List (intercalate)

class ArgDict a where
  argList :: [a]
  argDescription :: a -> String
  argKeywords :: a -> [String]
  argNames :: a -> [(String, a)]
  argNames x = [(s, x) | s <- argKeywords x]
  matchArg :: String -> Maybe a

data ArgCommands = Init | Version | Help | RunAll
  deriving (Show)


data ArgSettings = Verbose | Force | RuleInput [String] | StyleInput [String]
  deriving (Eq)

instance Show ArgSettings where
  show Verbose = "Verbose"
  show Force = "Force"
  show (RuleInput []) = "RuleInput"
  show (RuleInput x) = "RuleInput [" <> intercalate ", " (map show x) <> "]"
  show (StyleInput []) = "StyleInput"
  show (StyleInput x) = "StyleInput [" <> intercalate ", " (map show x) <> "]"

-- instance Eq ArgSettings where
--   Verbose == Verbose = True
--   Verbose == (RuleInput _) = False
--   Verbose == (StyleInput _) = False
--   (RuleInput _) == (RuleInput _) = True
--   (RuleInput _) == (StyleInput _) = False
--   (StyleInput _) == (StyleInput _) = True
  

instance ArgDict ArgCommands where
  argList = [Init, Version, Help, RunAll]

  argDescription Init = "Initializes ghoul configuration"
  argDescription Version = "Displays the current version of the program"
  argDescription Help = "Displays information about how the program is used"
  argDescription RunAll = "Runs all defined rules regardless of configuration"


  argKeywords Init = ["Init", "init"]
  argKeywords Version = ["Version", "version"]
  argKeywords Help = ["Help", "help"]
  argKeywords RunAll = ["RunAll", "runAll", "runall"]

  matchArg s =
    let
      init' = lookup s $ argNames Init
      ver' = lookup s $ argNames Version
      help' = lookup s $ argNames Help
      run' = lookup s $ argNames RunAll
    in
      init' <|> ver' <|> help' <|> run'

instance ArgDict ArgSettings where
  argList = [Verbose, Force, RuleInput [], StyleInput []]

  argDescription Verbose = "Verbose mode. Prints extra information during commands such as during initialization"
  argDescription Force = "Allows to continue initialization process without terminating"
  argDescription (RuleInput _) = "Input for rules. Specifies which rules to run. Also used for editor plugin"
  argDescription (StyleInput _) = "Custom style input. Mostly used by editor"

  argKeywords Verbose = ["-v", "-verbose", "-Verbose"]
  argKeywords Force = ["-force", "-f"]
  argKeywords (RuleInput _) = ["-rules"]
  argKeywords (StyleInput _) = ["-style"]

  matchArg s =
    let
      verb' = lookup s $ argNames Verbose
      rule' = lookup s $ argNames (RuleInput [])
      styl' = lookup s $ argNames (StyleInput [])
    in
      verb' <|> rule' <|> styl'

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
      Sett (RuleInput []) -> buildArgumentsHelper (dropInput xs) args{argSettings = RuleInput (collectInput xs) : argSettings args}
      Sett (StyleInput []) -> buildArgumentsHelper (dropInput xs) args{argSettings = StyleInput (collectInput xs) : argSettings args}
      Sett as -> buildArgumentsHelper xs args{argSettings = as : argSettings args}
      Outp out -> buildArgumentsHelper xs args{argOutputMode = out}
    buildArgumentsHelper [] args = args


    {-
      a function - maps a single string to a list of strings
      and compares them. Returns a result of bool

      Example: 
      - allSettingKeywords : list of compared strings. ex: ["a", "b", "c"]
      - s : string which is being compared. ex: "b"
      - @result : [False, True, False]
    -}
    a :: String -> [Bool]
    a s = map ((\x -> x s) . (\x -> (==x))) allSettingKeywords
    allSettingKeywords = argKeywords (RuleInput []) ++ argKeywords (StyleInput [])

    b :: [Bool] ->  Bool
    b = not . or

    collectInput :: [String] -> [String]
    collectInput = takeWhile (b . a)

    dropInput :: [String] -> [String]
    dropInput = dropWhile (b . a)
