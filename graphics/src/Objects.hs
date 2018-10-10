-- {-# OPTIONS_GHC -Wall #-}

module Objects where

import Graphics.Gloss

import Canvas

type Object = [Float]

drawCircle :: Float -> Float -> Float -> Color -> Picture
drawCircle x y r c = translate x y $ color c $ circleSolid r

drawRectangle :: Float -> Float -> Float -> Float -> Color -> Picture
drawRectangle x y w h c = translate x y $ color c $ rectangleSolid w h

translateObject :: Object -> Object
translateObject [x, y, w, h] = [x - offsetH + (w / 2), (-y) + offsetV - (h / 2), w, h]
translateObject l = l

drawObject :: Color -> Object -> Picture
drawObject c [x, y, w, h] = drawRectangle x y w h c
drawObject c l = drawRectangle 0 0 0 0 c

object :: Color -> Object -> Picture
object c = (drawObject c) . translateObject
