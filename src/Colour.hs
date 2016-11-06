module Colour
  (Colour, white, silver, gray, black, red,
   maroon, yellow, olive, lime, green, aqua,
   teal, blue, navy, fuchsia, purple, custom
  ) where

import Data.Word (Word8)
import Text.Printf (printf)

-- The original 16 HTML colours as well as custom hex colours
data Colour = White
            | Silver
            | Gray
            | Black
            | Red
            | Maroon
            | Yellow
            | Olive
            | Lime
            | Green
            | Aqua
            | Teal
            | Blue
            | Navy
            | Fuchsia
            | Purple
            | Custom Word8 Word8 Word8

instance Show Colour where
  show White          = "white"
  show Silver         = "silver"
  show Gray           = "gray"
  show Black          = "black"
  show Red            = "red"
  show Maroon         = "maroon"
  show Yellow         = "yellow"
  show Olive          = "olive"
  show Lime           = "lime"
  show Green          = "green"
  show Aqua           = "aqua"
  show Teal           = "teal"
  show Blue           = "blue"
  show Navy           = "navy"
  show Fuchsia        = "fuchsia"
  show Purple         = "purple"
  show (Custom r g b) = printf "#%2x%2x%2x" r g b

white   = White
silver  = Silver
gray    = Gray
black   = Black
red     = Red
maroon  = Maroon
yellow  = Yellow
olive   = Olive
lime    = Lime
green   = Green
aqua    = Aqua
teal    = Teal
blue    = Blue
navy    = Navy
fuchsia = Fuchsia
purple  = Purple
custom  = Custom
