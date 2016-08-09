module Tile where

import Common
import Drawable

import Data.Map (Map)

data Tile
  = Wall
  | Open
  | Water
  deriving (Show, Eq, Ord)

type Board = Map Pos Tile

instance AsChar Tile where
  fromChar '.' = Just Open
  fromChar '@' = Just Open
  fromChar 'B' = Just Open
  fromChar 'W' = Just Wall
  fromChar '~' = Just Water
  fromChar _   = Nothing

  toChar Open  = '.'
  toChar Wall  = 'W'
  toChar Water = '~'

instance Drawable Tile where
  draw window pos = draw window pos . CharRep

passable :: Maybe Tile -> Bool
passable (Just Open) = True
passable _           = False

opaque :: Tile -> Bool
opaque Wall = True
opaque _    = False
