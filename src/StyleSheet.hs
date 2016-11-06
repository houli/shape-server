module StyleSheet
  (
    module Colour
  , StyleSheet(..)
  , (<:>)
  , defaultStyle
  , strokeWidth
  , stroke
  , fill
  ) where

import Colour

data StyleSheet = StyleSheet Double Colour Colour
                  deriving Show

defaultStyle :: StyleSheet
defaultStyle = StyleSheet 0 black black

(<:>) :: StyleSheet -> (StyleSheet -> StyleSheet) -> StyleSheet
(<:>) = flip ($)

strokeWidth :: Double -> (StyleSheet -> StyleSheet)
strokeWidth sw (StyleSheet _ sc fc) = StyleSheet sw sc fc

stroke :: Colour -> (StyleSheet -> StyleSheet)
stroke sc (StyleSheet sw _ fc) = StyleSheet sw sc fc

fill :: Colour -> (StyleSheet -> StyleSheet)
fill fc (StyleSheet sw sc _) = StyleSheet sw sc fc
