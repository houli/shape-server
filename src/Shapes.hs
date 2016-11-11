 module Shapes (
  Shape(..), Drawing, Transform,
  empty, circle, square,
  identity, translate, rotate, scale, (<+>),
  transform
{-# LANGUAGE FlexibleInstances #-}
  ) where

import           Data.Aeson
import qualified Data.Matrix as M

import           StyleSheet (StyleSheet, defaultStyle)

data Shape = Empty
           | Circle
           | Square
           deriving Show

instance FromJSON Shape where
  parseJSON = withObject "circle or square" $ \o -> do
    shape <- o .: "shape"
    case shape of
      "circle" -> pure Circle
      "square" -> pure Square
      _        -> fail ("Unknown shape " ++ shape)

empty, circle, square :: Shape
empty = Empty
circle = Circle
square = Square

data Transform = Identity
               | Translate Double Double
               | Scale Double Double
               | Rotate Double
               | Compose Transform Transform
               deriving Show

instance FromJSON Transform where
  parseJSON = withObject "transform" $ \o -> do
    transformType <- o .: "type"
    case transformType of
      "translate" -> Translate <$> o .: "x" <*> o.: "y"
      "scale"     -> Scale <$> o .: "x" <*> o.: "y"
      "rotate"    -> Rotate <$> o .: "angle"
      _           -> fail ("Unknown transform " ++ transformType)

identity = Identity
translate = Translate
scale = Scale
rotate = Rotate
t0 <+> t1 = Compose t0 t1

-- Composition of transforms is matrix multiplication
transform :: Transform -> M.Matrix Double
transform Identity = M.identity 3
transform (Translate x y) = M.fromLists [[1, 0, x], [0, 1, y], [0, 0, 1]]
transform (Scale x y) = M.fromLists [[x, 0, 0], [0, y, 0], [0, 0, 1]]
transform (Rotate b) = let a = (b * pi) / 180 in
                         M.fromLists [[cos a, -(sin a), 0], [sin a, cos a, 0], [0, 0, 1]]
transform (Compose t1 t2) = transform t1 `M.multStd2` transform t2

type Drawing = [(Transform, Shape, StyleSheet)]

-- Define for a single triple and use the built-in list instance that
-- is defined for "instance FromJSON a => FromJSON [a]"
instance {-# OVERLAPPING #-} FromJSON (Transform, Shape, StyleSheet) where
  parseJSON = withObject "styled, transformed, shape" $ \o -> do
    shape <- parseJSON (Object o)
    styleSheet <- o .:? "stylesheet" .!= defaultStyle
    transforms <- o .:? "transform" .!= [identity]
    let composedTransforms = foldl1 (<+>) transforms
    pure (composedTransforms, shape, styleSheet)
