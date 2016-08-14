module GeometrySpec where

import Geometry
import Common

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Set

import Prelude hiding (map, filter)

spec :: Spec
spec = do
  parallel $ do

    context "disk" $ do

      prop (concat [ "filter ((==r + x(1|2)) . (fst|snd)) (disk r x) "
                   , "== empty"
                  ]) $ do
        \r x@(x1, _) -> filter ((==r + x1) . fst) (disk r x) == empty
        \r x@(_, x2) -> filter ((==r + x2) . snd) (disk r x) == empty

      prop "r /= 0 ==> length (disk r _) `quot` (r*r) <= 3" $ do
        \r x -> r /= 0 ==> length (disk r x) `quot` (r*r) <= 3

    context "line" $ do

      prop "line y y \\\\ fromList [y, y] == line x y" $ do
        \x y -> line x y \\ fromList [x, y] == line x y

      prop (concat [ "line x y /= empty ==> "
                   , "length (line x y) == "
                   , "max (abs (x1-y1)) (abs (x2-y2)) - 1"
                   ]) $ do
        \x@(x1,x2) y@(y1, y2) -> line x y /= empty ==> 
                  length (line x y) == max (abs (x1-y1)) (abs (x2-y2))-1


    context "translate" $ do
      
      prop "translate p empty == empty" $ do
        \p -> translate p empty == empty

      prop "translate p . translate -p == id" $ do
        \p s -> (translate p . translate (-p)) s == s

      prop "length . translate p == length" $ do
        \p s -> (length . translate p) s == length s

      prop "translate p == map (+p)" $ do
        \p s -> translate p s == map (+p) s

