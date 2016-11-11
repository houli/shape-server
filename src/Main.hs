module Main where

import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import Web.Scotty

import Render

svg drawing = do
  setHeader "Content-Type" "image/svg+xml"
  setHeader "Vary" "Accept-Encoding"
  raw $ renderSvg drawing

main :: IO ()
main = scotty 3000 $ do
  get "/" $ file "static/index.html"

  post "/draw" $ do
    drawing <- jsonData
    svg $ render drawing
