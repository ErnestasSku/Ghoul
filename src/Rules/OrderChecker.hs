module Rules.OrderChecker (properOrder) where

import Parsing.AST

properOrder :: [AST] -> [(Int, String)]
properOrder s = properOrder' s Signal

-- properOrder s = properOrder' s Unknown

properOrder' :: [AST] -> Order -> [(Int, String)]
properOrder' (x : xs) state = case x of
  Keyword s (line, col) ->
    let ord = fromKeywordToOrder s
     in if s == "func"
          then []
          else
            if fromEnum ord == fromEnum state || fromEnum ord == fromEnum Unknown
              then properOrder' (filter (not . (<! line)) xs) state
              else
                if fromEnum ord > fromEnum state
                  then properOrder' (filter (not . (<! line)) xs) ord
                  else (line, "Line " ++ show line ++ " is not ordered correctly. \n\t\tThe correct order should follow signal -> enums -> consts -> exported variables -> public/private variables -> onready variables") : properOrder' (filter (not . (<! line)) xs) ord
  -- undefined
  _ -> properOrder' xs state
properOrder' [] _ = []

{-
    #FIXME:
    Note: Add Tool. It is rarely used in games, as Tool keyword
    lets the to execute code in the editor, it's mostly used with 
    Godot plugins 
-}
data Order
  = Unknown --
  | -- | Extends
    Signal
  | Enums
  | Constants
  | Exports
  | Variables
  | Onready
  | Funcs
  deriving (Show, Enum)

fromKeywordToOrder :: String -> Order
-- fromKeywordToOrder "extends" = Extends
fromKeywordToOrder "signal" = Signal
fromKeywordToOrder "enum" = Enums
fromKeywordToOrder "const" = Constants
fromKeywordToOrder "export" = Exports
fromKeywordToOrder "var" = Variables
fromKeywordToOrder "onready" = Onready
fromKeywordToOrder "func" = Funcs
fromKeywordToOrder _ = Unknown
