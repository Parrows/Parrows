{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module BaseSpec.SkeletonCheckBase where

import Parrows.Definition
import Parrows.Skeletons.Topology
import Parrows.Skeletons.Map

import Parrows.Future

import Control.Arrow

import Test.Hspec
import Test.Hspec.QuickCheck

pipeSpec :: (ArrowLoopParallel (->) Int Int (),
  Future fut Int (),  ArrowLoopParallel (->) (fut Int) (fut Int) (),
  Future fut (([Int], [Int]) ,[Int]) (), ArrowLoopParallel (->) (fut (([Int], [Int]), [Int])) (fut (([Int], [Int]), [Int])) ()) => Spec
pipeSpec = describe "Pipe Test" $ do
    prop "Pipe 4 times (+1)" $ pipeTest
    prop "Pipe (Future) 4 times (+1)" $ pipeSimpleTest
      where
         replicated :: [Int -> Int]
         replicated = map (+) [1..4]

         expectedValue :: Int -> Int
         expectedValue x = foldl (flip ($)) x replicated

         pipeTest :: Int -> Bool
         pipeTest x = pipe () replicated x == expectedValue x

         pipeSimpleTest :: Int -> Bool
         pipeSimpleTest x = pipeSimple () replicated x == expectedValue x

         pipeCombinatorTest :: Int -> Bool
         pipeCombinatorTest x = (((+1) |>>>| (*2)) x) == (((+1) >>> (*2)) x)

ringSpec :: (Future fut Int (), ArrowLoopParallel (->) (Int, fut Int) (Int, fut Int) (),
              ArrowLoopParallel (->) Int Int ()) => Spec
ringSpec = describe "Ring Test" $ do
    prop "" $ ringTest
      where
        values :: Int -> [Int]
        values cnt = take cnt $ [1..]

        ringTest :: Int -> Bool
        ringTest cnt = (ring () (\(x,y) -> (y, x+1)) (values cnt) ) == (rightRotate $ map (+1) (values cnt))

        rightRotate    :: [a] -> [a]
        rightRotate [] =  []
        rightRotate xs =  last xs : init xs

mapSpec :: (ArrowParallel (->) Int Int (), ArrowParallel (->) [Int] [Int] ()) => Spec
mapSpec = describe "mapTest" $ do
    prop "parMap" $ parMapTest $ parMap ()
    prop "parMapStream" $ parMapTest $ parMapStream () 4
    prop "farm" $ parMapTest $ farm () 2
    prop "farmChunk" $ parMapTest $ farmChunk () 2 4
      where
          parMapTest :: ((Int -> Int) -> ([Int] -> [Int])) -> [Int] -> Bool
          parMapTest skel xs = skel (+1) xs == map (+1) xs

mapReduceSpec ::  (ArrowParallel (->) (Int, [Int]) Int ()) => Spec
mapReduceSpec = describe "parMapReduceDirect Test" $
    prop "Basic parMapReduceDirect Test" $ parMapReduceDirectTest
      where
        parMapReduceDirectTest :: [Int] -> Bool
        parMapReduceDirectTest xs = (parMapReduceDirect () 4 (+1) (uncurry (*)) (0, xs)) == (foldl (*) 0 $ map (+1) xs)
