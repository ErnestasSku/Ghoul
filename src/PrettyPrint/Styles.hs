module PrettyPrint.Styles where

import PrettyPrint.Pretty
  ( Color (..),
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