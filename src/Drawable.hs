{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Drawable where

import Common

import UI.HSCurses.Curses (Window, wMove)
import UI.HSCurses.CursesHelper (drawLine)

import Data.Map (Map, mapWithKey, mapKeys)

class Drawable a where
  draw :: Window -> Pos -> a -> IO ()

newtype CharRep a = CharRep a
instance AsChar a => Drawable (CharRep a) where
  draw window (x,y) (CharRep a) =  do
    wMove window y x
    drawLine 1 $ [toChar a]

instance Drawable a => Drawable (Map Pos a) where
  draw window pos = sequence_ . mapWithKey (draw window) . mapKeys (+pos)

