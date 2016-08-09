module GameState where

import Entity
import Tile
import Common
import Drawable
import Geometry

import UI.HSCurses.Curses
import System.Random

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Bimap as BM

data GameState =
  GameState { gameEntityData  :: EntityData
            , gameBoard       :: Board
            , gameRng         :: StdGen
            , gameVisionRange :: Int
            }

data GameConfig =
  GameConfig { configWindow   :: Window
             , configPosition :: Pos
             }

instance Drawable GameState where
  draw window pos gs = do
    let entityData = gameEntityData gs
        board      = gameBoard gs
        range      = gameVisionRange gs
        bases      = entBases $ entityData
        positions  = BM.toMap $ entPositions entityData
        positionsR = BM.toMapR $ entPositions entityData
        players    = M.filter isPlayer bases
        blocks     = M.keysSet $ M.filter opaque board

        visible = M.fromSet id $ S.unions $ do
          pos <- M.elems $ M.intersection positions players

          let unblocked =
                S.filter (S.null . S.intersection blocks . line' pos) $
                disk' range pos

          return unblocked

        visibletiles =
          M.intersection board visible
        visibleentities =
          M.mapMaybe (flip M.lookup bases) $
          M.intersection positionsR visible
    
    draw window pos visibletiles
    draw window pos visibleentities

    where isPlayer (Player _) = True
          isPlayer _          = False

          
