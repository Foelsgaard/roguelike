{-# LANGUAGE RankNTypes #-}

module Lenses where

import Common
import Tile
import Entity
import GameState

import Control.Lens

import qualified Data.Map as M
import qualified Data.Bimap as BM

import System.Random

_entities :: Lens' GameState EntityData
_entities = lens gameEntityData $ \gs edata ->
  gs { gameEntityData = edata }

_board :: Lens' GameState Board
_board = lens gameBoard $ \gs b ->
  gs { gameBoard = b }

_bases :: Lens' EntityData (M.Map (ID Entity) Entity)
_bases = lens entBases $ \ed bs ->
  ed { entBases = bs }

_positions :: Lens' EntityData (BM.Bimap (ID Entity) Pos)
_positions = lens entPositions $ \ed ps ->
  ed { entPositions = ps }

_health :: Lens' Entity Health
_health = lens getter setter
  where getter e = case e of
          Player hp -> hp
          Baddie hp -> hp
        setter e hp = case e of
          Player _ -> Player hp
          Baddie _ -> Baddie hp

rng :: Lens' GameState StdGen
rng = lens gameRng $ \gs g ->
  gs { gameRng = g }

base :: ID Entity -> Lens' GameState Entity
base gid = lens getter setter
  where getter = views (_entities . _bases) (M.!gid)
        setter gs b = over (_entities . _bases) (M.insert gid b) gs

health :: ID Entity -> Lens' GameState Health
health gid = base gid . _health

position :: ID Entity -> Lens' GameState Pos
position gid = lens getter setter
  where getter = views (_entities . _positions) (BM.!gid)
        setter gs p = over (_entities . _positions) (BM.insert gid p) gs

entityAt :: Pos -> Getter GameState (Maybe (ID Entity))
entityAt pos = to $ views (_entities . _positions) (BM.lookupR pos)

tileAt :: Pos -> Getter GameState (Maybe Tile)
tileAt pos = to $ views _board (M.lookup pos)
