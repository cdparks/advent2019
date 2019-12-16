module Advent.Image
  ( scale
  )
where

import Advent.Prelude

import Codec.Picture

scale :: Int -> Image PixelRGB8 -> Image PixelRGB8
scale n image = generateImage
  pick
  (n * imageWidth image)
  (n * imageHeight image)
  where pick x y = pixelAt image (x `div` n) (y `div` n)
