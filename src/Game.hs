{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Game where

import Drawable
import Tile
import Entity
import Common
import GameState
import Lenses

import UI.HSCurses.Curses

import Control.Lens
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus, mzero, liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State
  ( StateT
  , MonadState
  , execStateT
  , get
  , put
  , modify
  )
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Reader (ReaderT, MonadReader, ask, runReaderT)

import System.Random

import Prelude hiding (lookup)

import Data.Maybe (isNothing)
import Data.Map (notMember, (!), delete, insert, lookup)
import qualified Data.Bimap as BM

newtype Game a =
  Game {unGame :: MaybeT (ReaderT GameConfig (StateT GameState IO)) a}
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState GameState
           , MonadReader GameConfig
           , MonadIO
           , Alternative
           , MonadPlus
           )

act :: Specific Entity -> Game ()
act (player, Player _) = do

  key <- getInput

  pos <- use $ position player


  let movePlayer dir = tryMove player $ pos + dir

  case key of
    KeyUp       -> movePlayer ( 0, -1)
    KeyDown     -> movePlayer ( 0,  1)
    KeyLeft     -> movePlayer (-1,  0)
    KeyRight    -> movePlayer ( 1,  0)
    KeyChar 'q' -> quit
    _           -> return False

  return ()

act (baddie, Baddie _) = do

  pos <- use $ position baddie

  dir <- pick [ ( 0, -1)
              , ( 0,  1)
              , (-1,  0)
              , ( 1,  0)
              ]

  tryMove baddie (pos + dir)

  return ()

tryMove :: ID Entity -> Pos -> Game Bool
tryMove entity p = do

  psbl   <- uses (tileAt p) passable
  vacant <- uses (entityAt p) isNothing

  if psbl && vacant
    then moveEntity entity p >> return True
    else return False

refreshGame :: Game ()
refreshGame = do
  gs <- get
  gc <- ask

  let window = configWindow gc
      pos    = configPosition gc

  liftIO $ wclear window
  liftIO $ draw window pos gs
  liftIO $ refresh
  
getInput :: Game Key
getInput = liftIO getCh

rand :: Random a => Game a
rand = do
  (n, gen') <- uses rng random
  rng .= gen'
  return n

pick :: [a] -> Game a
pick xs = do
  (n, gen') <- uses rng (randomR (0, length xs - 1))
  rng .= gen'
  return $ xs !! n

moveEntity :: ID Entity -> Pos -> Game ()
moveEntity entity p = position entity .= p

quit :: Game a
quit = mzero

runGame :: Game a -> GameConfig -> GameState -> IO GameState
runGame = (execStateT .) . runReaderT . runMaybeT . unGame
