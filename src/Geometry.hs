{-# LANGUAGE FlexibleInstances #-}

module Geometry where

import Common

import Control.Monad (guard)

import Data.Set (Set, mapMonotonic, empty, singleton, unions, fromList)

type Shape = Set Pos

disk' :: Int -> Pos -> Shape
disk' r o = translate o $ disk r

disk :: Int -> Shape
disk r = fromList $ do
  x <- [-r+1..r-1]
  y <- [-r+1..r-1]

  guard $ x*x + y*y < r*r

  return $ (x, y)

line' o p = translate o $ line (p - o)

line :: Pos -> Shape
line (x, y)
  | abs x <= 1 && abs y <= 1 = empty
  | x == 0 = fromList $ zip [0,0..] ((tail . init) [0, signum y..y])
  | y == 0 = fromList $ zip ((tail . init) [0, signum x..x]) [0,0..]
  | x == y = fromList $ zip
             ((tail . init) [0, signum x..x])
             ((tail . init) [0, signum y..y])
  | otherwise = unions [ line p'
                       , singleton p'
                       , translate p' $ line p''
                       ]

  where p'  = ( x `quot` 2
              , y `quot` 2
              )
        p'' = ( (x + signum x) `quot` 2
              , (y + signum y) `quot` 2
              )

translate :: Pos -> Shape -> Shape
translate dx = mapMonotonic (+dx)
