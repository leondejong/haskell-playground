-- {-# OPTIONS_GHC -Wall #-}

module Platform where

import Data.Foldable

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.ViewPort

import Canvas
import Objects

type Player = Picture
type Level = [Picture]

data State = State
  { x :: Float
  , y :: Float
  , w :: Float
  , h :: Float
  , vx :: Float
  , vy :: Float
  , force :: Float
  , impulse :: Float
  , gravity :: Float
  , frame :: Float
  , c :: Color
  }

state :: State
state = State
  { x = 32
  , y = 32
  , w = 16
  , h = 16
  , vx = 0
  , vy = 0
  , force = 320
  , impulse = 720
  , gravity = 32
  , frame = 1 / fromIntegral rate
  , c = content
  }

objects :: [Object]
objects = -- border
  [ [0, 0, 768, 16]
  , [0, 560, 768, 16]
  , [0, 0, 16, 576]
  , [752, 0, 16, 576]
  -- floors
  , [336, 144, 16, 288]
  , [352, 144, 336, 16]
  , [418, 236, 336, 16]
  , [352, 326, 336, 16]
  , [464, 416, 112, 16]
  , [640, 416, 112, 16]
  , [576, 486, 64, 16]
  -- platforms
  , [80, 486, 64, 16]
  , [208, 416, 64, 16]
  , [80, 348, 64, 16]
  , [208, 280, 64, 16]
  , [80, 212, 64, 16]
  , [208, 144, 64, 16]
  -- stairs
  , [448, 432, 16, 16]
  , [432, 448, 16, 16]
  , [416, 464, 16, 16]
  , [400, 480, 16, 16]
  , [384, 496, 16, 16]
  , [368, 512, 16, 16]
  , [352, 528, 16, 16]
  , [336, 544, 16, 16]
  -- walls
  , [420, 80, 16, 64]
  , [588, 80, 16, 64]
  , [504, 16, 16, 64]
  ]

intersect :: Object -> Object -> Maybe Object
intersect a b
  | i = Just b
  | otherwise = Nothing
  where
    n = a !! 1 < b !! 1 + b !! 3
    s = a !! 1 + a !! 3 > b !! 1
    e = a !! 0 < b !! 0 + b !! 2
    w = a !! 0 + a !! 2 > b !! 0
    i = n && s && e && w

collision :: Object -> Maybe Object
collision o = asum $ fmap (intersect o) objects

forces :: State -> IO State
forces s = return s { vy = vy s + frame s * gravity s }

position :: State -> IO State
position s = return $ s { x = x s + vx s, y = y s + vy s }

left :: KeyState -> State -> IO State
left k s
  | k == Down = return $ s { vx = negate $ frame s * force s }
  | k == Up && vx s < 0  = return $ s { vx = 0 }
  | otherwise = return s

right :: KeyState -> State -> IO State
right k s
  | k == Down = return $ s { vx = frame s * force s }
  | k == Up && vx s > 0  = return $ s { vx = 0 }
  | otherwise = return s

jump :: State -> IO State
jump s
  | vy s == 0 = return $ s { vy = negate $ frame s * impulse s }
  | otherwise = return s

reviseX :: State -> Object -> IO State
reviseX s o =
  let r = vx s < 0
  in case r of
    True -> return s { vx = o !! 0 + o !! 2 - x s }
    False -> return s { vx = o !! 0 - x s - w s }

reviseY :: State -> Object -> IO State
reviseY s o =
  let r = vy s < 0
  in case r of
    True -> return s { vy = o !! 1 + o !! 3 - y s }
    False -> return s { vy = o !! 1 - y s - h s }

checkX :: State -> IO State
checkX s =
  case mo of
    Just o -> reviseX s o
    Nothing -> return s
  where
    mo = collision [x s + vx s, y s, w s, h s]

checkY :: State -> IO State
checkY s =
  case mo of
    Just o -> reviseY s o
    Nothing -> return s
  where
    mo = collision [x s, y s + vy s, w s, h s]

checkC :: State -> IO State
checkC s = do
  let mo = collision [x s + vx s, y s + vy s, w s, h s]
  -- print mo
  case mo of
    Just _ -> return $ s { c = content }
    Nothing -> return $ s { c = foreground }

player :: State -> Player
player s = object (c s) [x s, y s, w s, h s]

level :: State -> Level
level _ = fmap (object foreground) objects

render  :: State -> IO Picture
render s = return $ applyViewPortToPicture viewport $ pictures $ (++) (level s) [player s]

input :: Event -> State -> IO State
input e s
  | EventKey (Char 'a') k _ _ <- e = left k s
  | EventKey (Char 'd') k _ _ <- e = right k s
  | EventKey (Char 'w') Down _ _ <- e = jump s
  | otherwise = return s

update :: Float -> State -> IO State
update _ s = do
  s <- forces s
  -- s <- checkC s
  s <- checkX s
  s <- checkY s
  s <- position s
  return s

main :: IO ()
main = playIO window background rate state render input update
