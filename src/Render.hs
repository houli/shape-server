module Render (render) where

import           Data.Matrix (submatrix, toLists)
import           Text.Blaze.Svg11
import           Text.Blaze.Svg11.Attributes

import qualified Shapes as S
import           Shapes (Drawing, Shape(..), Transform, empty)
import           StyleSheet (StyleSheet(..))

svgHeader :: Svg -> Svg
svgHeader = docTypeSvg ! version "1.1" ! viewbox "-25 -25 50 50"

render :: Drawing -> Svg
render drawing = svgHeader $ renderDrawing drawing

renderDrawing :: Drawing -> Svg
renderDrawing [] = renderShape empty -- Blank drawing if no shapes provided
renderDrawing [x] = renderTriple x
renderDrawing (x : xs) = renderTriple x >> renderDrawing xs

renderTriple :: (Transform, Shape, StyleSheet) -> Svg
renderTriple (tr, sh, StyleSheet sw sc fc) = renderShape sh !
                                             renderTransform tr !
                                             strokeWidth (stringAttr sw) !
                                             stroke (stringAttr sc) !
                                             fill (stringAttr fc) !
                                             customAttribute "vector-effect" "non-scaling-stroke"
  where
    stringAttr :: Show a => a -> AttributeValue
    stringAttr = stringValue . show

renderShape :: Shape -> Svg
renderShape Empty =  rect -- Nothing is drawn without specifying a width and height
renderShape Square = rect ! width "1" ! height "1"
renderShape Circle = circle ! r "1"

renderTransform :: Transform -> Attribute
renderTransform t = transform $ matrix a b c d e f
  where mat = S.transform t
        [[a, c, e], [b, d, f]] = toLists $ submatrix 1 2 1 3 mat
