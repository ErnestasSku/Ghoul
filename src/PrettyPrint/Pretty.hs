{-
  This library is inspired by https://github.com/loganmac/pretty-terminal
  and thus follows similar design patterns and implementations, however,
  it was not meeting my requirements, which are quite niche (mainly wrapping it in BB code)
-}
{-# LANGUAGE FlexibleInstances #-}

module PrettyPrint.Pretty
  ( Color (..),
    Pretty (..),
    Section (..),
    -- Style (..),
    Output (..),
    colorList,
    intColorCode,
  )
where

import qualified Data.Text as T
import Numeric (showHex)
import Text.Printf (printf)
import Data.List (sortBy, minimumBy)
import Data.Function (on)
import Text.Printf (printf)
import Text.Parsec (Reply(Error))

data Section = Foreground | Background

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | Default | Custom (Int, Int, Int)
  deriving (Read, Eq)

-- Note: Default is not added, since it is not clear what default means.
colorList :: [Color]
colorList = [Black, Red, Green, Yellow, Blue, Magenta, Cyan, White] 

instance Show Color where
  show x = case x of
    Black -> "Black"
    Red -> "Red"
    Green -> "Green"
    Yellow -> "Yellow"
    Blue -> "Blue"
    Magenta -> "Magenta"
    Cyan -> "Cyan"
    White -> "White"
    Default -> "Default"
    Custom (r, g, b) -> "(%d,%d,%d)" :: String

data Style = Normal | Bold | Faint | Italic | Underline | SlowBlink | ColoredNormal | Reverse
  deriving (Show, Enum)

-- As it is (Type a), It should be possible to implement a monad,
-- However not clear how useful it would be.
data Output a = Terminal a | Editor a | Plain a
  deriving(Show)

instance Functor Output where
  fmap f (Terminal a) = Terminal $ f a
  fmap f (Editor a) = Editor $ f a 
  fmap f (Plain a) = Plain $ f a

dropADT :: Output a -> a
dropADT (Terminal a) = a
dropADT (Editor a) = a
dropADT (Plain a) = a

{-
  It would be better if returning of String it would return
  an abstract type so the Text instance wouldn't need to unpack the Text into String
-}
class Pretty a where
  color :: Color -> a -> String
  color = colorize Foreground
  bgColor :: Color -> a -> String
  bgColor = colorize Background
  colorize :: Section -> Color -> a -> String

-- style :: Style -> a -> a

instance Pretty (Output String) where
  colorize section color (Terminal a) =
      "\x1b["
        <> sectionNum
        <> show (colorCodeInt color)
        <> "m"
        <> a
        <> "\x1b[0m" :: String
    where
      sectionNum :: String
      sectionNum = case section of
        Foreground -> "9"
        Background -> "4"
  colorize section color (Editor a) =
      "[color="
        <> colorCodeString color
        <> "]"
        <> a
        <> "[/color]"
  colorize _ _ (Plain a) = a


instance Pretty (Output T.Text) where
  colorize section color (Terminal a) = T.unpack $
      T.pack "\x1b["
        <> sectionNum
        <> T.pack (show (colorCodeInt color))
        <> T.pack "m"
        <> a
        <> T.pack "\x1b[0m"
    where
      sectionNum :: T.Text 
      sectionNum = case section of
        Foreground -> T.pack "9"
        Background -> T.pack "4"
  colorize section color (Editor a) = T.unpack $
      T.pack "[color="
        <> T.pack (colorCodeString color)
        <> T.pack "]"
        <> a
        <> T.pack "[/color]"
  colorize _ _ (Plain a) = T.unpack a

colorCodeInt :: Color -> Int
colorCodeInt Black = 0
colorCodeInt Red = 1
colorCodeInt Green = 2
colorCodeInt Yellow = 3
colorCodeInt Blue = 4
colorCodeInt Magenta = 5
colorCodeInt Cyan = 6
colorCodeInt White = 7
colorCodeInt Default = 8
colorCodeInt (Custom rgb) = findClosest rgb

intColorCode :: (Num a, Eq a) => a -> Color
intColorCode 0 = Black 
intColorCode 1 = Red 
intColorCode 2 = Green 
intColorCode 3 = Yellow 
intColorCode 4 = Blue 
intColorCode 5 = Magenta 
intColorCode 6 = Cyan 
intColorCode 7 = White 
intColorCode 8 = Default 
intColorCode _ = error "Exhausted all possible colors"

colorCodeString :: Color -> String
colorCodeString Black = "black"
colorCodeString Red = "red"
colorCodeString Green = "lime"
colorCodeString Yellow = "yellow"
colorCodeString Blue = "blue"
colorCodeString Magenta = "#FF00FF"
colorCodeString Cyan = "#00FFFF"
colorCodeString White = "white"
colorCodeString Default = "default"
colorCodeString (Custom (r, g, b)) = "#" <> hex r <> hex g <> hex b
  where
    hex :: Int -> String
    hex x
      | x > 255 = showHex 255 ""
      | x < 16 = "0" ++ showHex x ""
      | otherwise = showHex x ""

-- Finds the closest primary color for terminal output
-- findClosest :: (Int, Int, Int) -> Int
findClosest :: (Integral a, Integral b, Integral c) => (a, b, c) -> Int
findClosest (r, g, b) = colorCodeInt $ fst $ minimumBy (compare `on` snd) (map (f rgb') predefinedColors)
  where
    rgb' = (fromIntegral r, fromIntegral g, fromIntegral b)

f :: (Floating a) => (a, a, a) -> (Color, (a, a, a)) -> (Color, a)
f (r, g, b) (c, (r1, g1, b1)) = (c, fr + fg + fb)
  where
    fr = (r - r1) ** 2
    fg = (g - g1) ** 2
    fb = (b - b1) ** 2

predefinedColors :: [(Color, (Double, Double, Double))]
predefinedColors =
  [ (Black, (0, 0, 0)),
    (Red, (255, 0, 0)),
    (Green, (0, 255, 0)),
    (Yellow, (255, 255, 0)),
    (Blue, (0, 0, 255)),
    (Magenta, (255, 0, 255)),
    (Cyan, (0, 255, 255)),
    (White, (255, 255, 255)),
    (Default, (192, 192, 192))
  ]