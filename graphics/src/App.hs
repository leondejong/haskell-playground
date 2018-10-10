-- {-# OPTIONS_GHC -Wall #-}

module App where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
-- import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.IO.Game

import Canvas
import Objects

type State = Float

stage :: Picture
stage = pictures
  [  drawCircle 48 48 32 foreground
  ,  drawRectangle 128 128 64 64 foreground
  ]

state :: State
state = 0

render :: Float -> Picture
render _ = drawCircle 64 64 32 foreground

input :: Event -> State -> State
input (EventKey (Char 's') Down _ _) _ = 0
input (EventKey (Char 'w') Down _ _) s = s + 1
input _ s = s

update :: Float -> State -> State
update t s = s + t

update' :: ViewPort -> Float -> State -> State
update' _ = update

main :: IO ()
-- main = display window background stage
-- main = animate window background render
-- main = simulate window background rate state render update'
main = play window background rate state render input update
