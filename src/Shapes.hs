 module Shapes (
  Shape(..), Drawing, Transform,
  empty, circle, square,
  identity, translate, rotate, scale, (<+>),
  transform
  ) where

import qualified Data.Matrix as M

data Shape = Empty
           | Circle
           | Square
           deriving Show

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

identity = Identity
translate = Translate
scale = Scale
rotate = Rotate
t0 <+> t1 = Compose t0 t1

transform :: Transform -> M.Matrix Double
transform Identity = M.identity 3
transform (Translate x y) = M.fromLists [[1, 0, x], [0, 1, y], [0, 0, 1]]
transform (Scale x y) = M.fromLists [[x, 0, 0], [0, y, 0], [0, 0, 1]]
transform (Rotate b) = let a = (b * pi) / 180 in M.fromLists [[cos a, -(sin a), 0], [sin a, cos a, 0], [0, 0, 1]]
transform (Compose t1 t2) = transform t1 `M.multStd2` transform t2

type Drawing = [(Transform, Shape)]
