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

import Data.Aeson

import Colour

data StyleSheet = StyleSheet Double Colour Colour
                  deriving Show

instance FromJSON StyleSheet where
  parseJSON = withObject "stylesheet" $ \o -> do
    width <- o .:? "strokeWidth" .!= 0
    strokeColour <- o .:? "stroke" .!= black
    fillColour <-  o .:? "fill" .!= black
    pure $ StyleSheet width strokeColour fillColour

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
