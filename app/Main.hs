module Main where

import Drawable
import Common
import Options
import Game
import GameState
import Entity

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper (start, end)

import System.Random

import Control.Monad (forever)
import Control.Monad.State (get)

import qualified Data.Map as M

main ::  IO ()
main = do
 
  opts     <- getOptions
  boardStr <- readFile $ boardFile opts
  window   <- initScr
  rng      <- getStdGen

  start

  cursSet CursorInvisible
  
  let gs = GameState { gameBoard       = make boardStr
                     , gameEntityData  = populate boardStr
                     , gameRng         = rng
                     , gameVisionRange = 15
                     } 
      gc = GameConfig window (10, 5)
  
  runGame game gc gs

  end

game :: Game ()
game = forever $ do

  refreshGame
  
  gs <- get

  sequence_
    $ M.mapWithKey (curry act)
    $ (entBases . gameEntityData) gs

