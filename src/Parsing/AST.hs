module Parsing.AST where

type LineRow = Int  
type LineCol = Int 
type LinePos = (LineRow, LineCol)
-- data LinePos = LinePos LineRow LineCol

data AST 
    = Identifier String LinePos 
    | Keyword String LinePos
    | Operator String LinePos
    | Comment String LinePos
    deriving (Show)

instance Eq AST where
    Identifier i p == Identifier i' p' = i == i' && p == p' 
    Keyword k _ == Keyword k' _  = k == k'
    Operator o _ == Operator o' _    = o == o'
    Comment c p == Comment c' p'  = c == c && p == p'
    _ == _ = False

-- | Checks if an element is in i-th row 
(<!) :: AST -- ^ Checked structure 
  -> Int -- ^ Row of the structure
  -> Bool
Identifier _ (r, _) <! i    = r == i
Keyword _ (r, _) <! i       = r == i
Operator _ (r, _) <! i      = r == i
Comment _ (r, _) <! i       = r == i