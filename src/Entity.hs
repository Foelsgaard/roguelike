module Entity where

import Drawable
import Common

import Data.Bimap (Bimap)
import qualified Data.Bimap as BM
import Data.Map (Map)
import qualified Data.Map as M

import Control.Arrow
import Control.Monad.State

data Entity
  = Player Health
  | Baddie Health
  deriving (Show, Eq, Ord)

type Health = Integer

data EntityData =
  EntityData
  { entBases     :: Map (ID Entity) Entity
  , entPositions :: Bimap (ID Entity) Pos
  }

instance AsChar Entity where
  fromChar '@' = Just $ Player 10
  fromChar 'B' = Just $ Baddie 10
  fromChar _   = Nothing

  toChar (Player _) = '@'
  toChar (Baddie _) = 'B'

instance Drawable Entity where
  draw window pos = draw window pos . CharRep

instance Drawable EntityData where
  draw window pos edata = draw window pos (BM.toMapR positions')
    where positions' =
            BM.map (\gid -> entBases edata M.! gid) $ entPositions edata

populate :: String -> EntityData
populate str =
  EntityData
  { entBases     = M.fromList $ zip ids bases
  , entPositions = BM.fromList $ zip ids ps
  }

  where (ps, bases) = unzip $ M.assocs $ make str
        ids = fst $ unzip $ zip (enumFrom (ID 0)) bases

