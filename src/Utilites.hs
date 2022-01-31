module Utilites (
    fromASTtoString
  
) 

where

import Parsing.Parser
import GHC.IO
import Text.Parsec (parse)
import Parsing.AST

-- | Extracts the String from AST type
fromASTtoString :: AST -> String
fromASTtoString (Identifier s _) = s
fromASTtoString (Keyword s _) = s
fromASTtoString (Operator s _) = s
fromASTtoString (Comment s _) = s
