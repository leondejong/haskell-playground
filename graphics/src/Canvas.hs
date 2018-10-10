-- {-# OPTIONS_GHC -Wall #-}

module Canvas where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

name :: String
name = "Canvas"

horizontal, vertical, width, height, rate :: Int
horizontal = 128
vertical = 256
width = 768
height = 576
rate = 60

offsetH, offsetV :: Float
offsetH = fromIntegral $ quot width 2
offsetV = fromIntegral $ quot height 2

background, foreground, content :: Color
background = makeColor 0.1 0.1 0.15 1
foreground = makeColor 0.4 0.4 1 1
content = makeColor 0.7 0.7 1 1

viewport :: ViewPort
-- viewport = ViewPort (negate offsetH, offsetV) 0 1
viewport = ViewPort (0, 0) 0 1

window :: Display
window = InWindow name (width, height) (horizontal, vertical)
