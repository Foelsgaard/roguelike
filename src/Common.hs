{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Common where

import Control.Monad
import Control.Arrow

import Data.List (transpose, (\\))
import Data.Map (Map, fromList, mapMaybe)
import qualified Data.Set as S

type Pos = (Int, Int)
type Dir = Pos

instance Num Pos where
  (a, b) + (c, d) = (a + c, b + d)
  (a, b) * (c, d) = (a * c, b * d)
  abs (x,y) = (abs x, abs y)
  signum (x,y) = (signum x, signum y)
  fromInteger n = (fromInteger n, fromInteger n)
  negate (x,y) = (negate x, negate y)

newtype ID a =
  ID Integer
  deriving (Eq, Ord, Enum)

type Specific a = (ID a, a)

class AsChar a where
  toChar   :: a    -> Char
  fromChar :: Char -> Maybe a

indexify :: [[a]] -> [(Pos, a)]
indexify = concat . zipWith zip (zipWith zip nss (transpose nss))
  where nss = repeat [0..]

make :: AsChar a => String -> Map Pos a
make =
  mapMaybe id . fromList . indexify . (map . map) fromChar . lines



