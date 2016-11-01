{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import Web.Scotty

import Render
import Shapes

svg drawing = do
  setHeader "Content-Type" "image/svg+xml"
  setHeader "Vary" "Accept-Encoding"
  raw $ renderSvg drawing

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    svg $ render [(identity, circle), (((translate 3 4) <+> (scale 10 10) <+> (rotate 10)), square)]
