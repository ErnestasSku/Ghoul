module PrettyPrint.Styles where

import PrettyPrint.Pretty
  ( Color (..),
    Output (..),
    Pretty (..),
  )

-- #TODO: This record doesn't have style
data OutputStyle = OutputStyle
  { osFileColor :: Color,
    osFilePathColor :: Color,
    osLineColor :: Color,
    osLineNumberColor :: Color,
    osCodeColor :: Color,
    osRuleColor :: Color,
    osSeparatorColor :: Maybe Color
  }

defaultTheme1 :: OutputStyle
defaultTheme1 = OutputStyle Cyan Cyan Green Red White Yellow Nothing

-- #TODO: Implement this function properly.
-- At the moment it returns just the only defined style.
getStyle :: OutputStyle
getStyle = defaultTheme1

applyStyle :: [(String, String, String, String)] -> OutputStyle -> Output () -> [String]
-- applyStyle ((filepath, line, code, warning) : xs) style output = [(fileNotation ++ fileName ++ " " ++ firstSep ++ " " ++ lineNotation ++ lineNumber ++ " " ++ codeExcerpt ++ " " ++ secondSep ++ " " ++ warningNotation ++ "\n")]
applyStyle ((filepath, line, code, warning) : xs) style output = (fileNotation ++ fileName ++ firstSep ++ lineNotation ++ lineNumber ++ " " ++ codeExcerpt ++ secondSep ++ warningNotation) : applyStyle xs style output
  where
    fileNotation = color (osFilePathColor style) $ lf output "File: " :: String
    fileName = color (osFilePathColor style) $ lf output filepath  :: String
    firstSep = color (sepCol1 $ osSeparatorColor style) $ lf output " | " :: String
    sepCol1 (Just a) = a
    sepCol1 Nothing = osFilePathColor style

    lineNotation = color (osLineColor style) $ lf output "line:" :: String
    lineNumber = color (osLineNumberColor style) $ lf output line :: String

    codeExcerpt = color (osCodeColor style) $ lf output code

    secondSep = color (sepCol2 $ osSeparatorColor style) $ lf output " | " :: String
    sepCol2 (Just a) = a
    sepCol2 Nothing = osRuleColor style

    warningNotation = color (osRuleColor style) $ lf output warning :: String
applyStyle [] _ _ = []

lf :: (Show a) => Output () -> a -> Output a
lf (Terminal _) a = Terminal a
lf (Editor _) a = Editor a
lf (Plain _) a = Plain a
