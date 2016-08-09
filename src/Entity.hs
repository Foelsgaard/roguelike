module Entity where

import Drawable
import Common

import qualified Data.Bimap as BM
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Arrow
import Control.Monad.State

data Entity
  = Player
  | Baddie
  deriving (Show, Eq, Ord)

type Health = Integer
type Speed  = Integer

data EntityData =
  EntityData
  { entBases     :: M.Map (ID Entity) Entity
  , entPositions :: BM.Bimap (ID Entity) Pos
  }

instance AsChar Entity where
  fromChar '@' = Just Player
  fromChar 'B' = Just Baddie
  fromChar _   = Nothing

  toChar Player = '@'
  toChar Baddie = 'B'

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

