module GeometrySpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Geometry
import qualified Data.Set as S
import Data.Set

spec :: Spec
spec = do
  modifyMaxSize (const 5000) $ parallel $ do
   describe "line" $ do
    
     it "returns the correct line to (5,5)" $ do
       line (5, 5) `shouldBe` fromList [(1,1), (2,2), (3,3), (4,4)]

     it "returns the empty set at zero" $ do
       line (0, 0) `shouldBe` empty

     prop "does not return zero" $ do
       \p -> (0, 0) `notMember` line p

     prop "does not return the end point" $ do
       \p -> p `notMember` line p

     prop "returns the shortest path to the end point" $ do
       \(x,y) -> (x, y) /= (0, 0) ==> length (line (x,y)) == max (abs x) (abs y) - 1

     prop "is symmetric about the origin" $ do
       \p -> line p == S.map negate (line (-p))

   describe "translate" $ do

     prop "translates the empty set to the empty set" $ do
       \p -> translate p empty == empty

     prop "cancels out an opposite translation" $ do
       \p s -> translate p (translate (-p) s) == s

     prop "preserves cardinality" $ do
       \p s -> length (translate p s) == length s

     prop "translates every point" $ do
       \p s -> all (flip member (translate p s) . (+p)) s

 --  describe "disk" $ do

 --    prop "
