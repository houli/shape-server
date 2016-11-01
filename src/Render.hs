{-# LANGUAGE OverloadedStrings #-}

module Render
  (render) where

import Data.Matrix (getElem)
import Text.Blaze.Svg11
import Text.Blaze.Svg11.Attributes

import qualified Shapes as S
import Shapes hiding (circle, transform)

svgHeader :: Svg -> Svg
svgHeader = docTypeSvg ! version "1.1" ! viewbox "-25 -25 50 50"

render :: Drawing -> Svg
render drawing = svgHeader $ renderDrawing drawing

renderDrawing :: Drawing -> Svg
renderDrawing [(trans, shape)] = renderShape shape ! renderTransform trans
renderDrawing ((trans, shape) : xs) = renderShape shape ! renderTransform trans >> renderDrawing xs

renderShape :: Shape -> Svg
renderShape Square = rect ! width "1" ! height "1"
renderShape Circle = circle ! r "1"

renderTransform :: Transform -> Attribute
renderTransform t = transform $ matrix a b c d e f
  where mat = S.transform t
        a = getElem 1 1 mat
        b = getElem 2 1 mat
        c = getElem 1 2 mat
        d = getElem 2 2 mat
        e = getElem 1 3 mat
        f = getElem 2 3 mat
