module Main where

import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import Web.Scotty

import Render
import Shapes
import StyleSheet

svg drawing = do
  setHeader "Content-Type" "image/svg+xml"
  setHeader "Vary" "Accept-Encoding"
  raw $ renderSvg drawing

main :: IO ()
main = scotty 3000 $ do
  get "/" $ file "static/index.html"

  get "/demo" $ svg $ render [(skewX 30, circle, defaultStyle),
                              (translate 3 4 <+> scale 10 10 <+> rotate 10,
                               square,
                               defaultStyle <:> strokeWidth 2 <:> stroke green <:> fill (custom 250 124 124))]

  post "/draw" $ do
    drawing <- jsonData
    svg $ render drawing
