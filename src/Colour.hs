module Colour
  (Colour, white, silver, gray, black, red,
   maroon, yellow, olive, lime, green, aqua,
   teal, blue, navy, fuchsia, purple, custom
  ) where

import           Data.Aeson hiding (Success)
import           Data.Char (toLower)
import           Data.Text (unpack)
import           Data.Word (Word8)
import           Numeric (readHex)
import           Text.Printf (printf)
import           Text.Trifecta

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

instance FromJSON Colour where
  parseJSON = withText "colour" $ \str ->
    case str of
      "white"   -> pure White
      "silver"  -> pure Silver
      "gray"    -> pure Gray
      "black"   -> pure Black
      "red"     -> pure Red
      "maroon"  -> pure Maroon
      "yellow"  -> pure Yellow
      "olive"   -> pure Olive
      "lime"    -> pure Lime
      "green"   -> pure Green
      "aqua"    -> pure Aqua
      "teal"    -> pure Teal
      "blue"    -> pure Blue
      "navy"    -> pure Navy
      "fuchsia" -> pure Fuchsia
      "purple"  -> pure Purple
      str       -> case parseString (hexParser <* eof) mempty (unpack str) of
                     Success colour -> pure colour
                     Failure _  -> fail ("Invalid colour \"" ++ unpack str ++ "\"")

hexParser :: Parser Colour
hexParser = do
  char '#'
  r <- rgbParser
  g <- rgbParser
  b <- rgbParser
  pure $ Custom r g b

rgbParser :: Parser Word8
rgbParser = do
  first <- hexDigit
  second <- hexDigit
  pure $ hexToWord8 [first, second]
  where
    hexToWord8 = fst . head . readHex

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
