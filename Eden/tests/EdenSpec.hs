-- EdenParallelSpec
module EdenSpec (spec) where

import Parrows.Definition
import Parrows.Future
import Parrows.Eden

import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    parEvalNSpec
    pipeSpec
    mapSpec

parEvalNSpec :: Spec
parEvalNSpec = describe "Basic Parrow Functionality Eden" $ do
    prop "Basic parEvalN" $ basicParEvalN
    prop "parEvalNLazy" $ parEvalNLazyInt
    prop "ParEvalNFut" $ parEvalNFutInt
     where
        basicParEvalN :: [Int] -> Bool
        basicParEvalN xs =  parEvalN () (repeat (+1)) xs == map (+1) xs

        parEvalNLazyInt :: [Int] -> Bool
        parEvalNLazyInt xs = parEvalNLazy () 4 (repeat (+1)) xs == map (+1) xs

        parEvalNFutInt :: [Int] -> Bool
        parEvalNFutInt xs = (map (get) $ parEvalNFut () (repeat (+1)) (map put xs)) == map (+1) xs

pipeSpec :: Spec
pipeSpec = describe "Pipe Test" $ do
    prop "Pipe 4 times (+1)" $ pipeTest
      where
         pipeTest :: Int -> Bool
         pipeTest x = (get (pipe () replicated (put x))) == x + 4
             where
                 replicated = replicate 4 (+1)

mapSpec :: Spec
mapSpec = describe "mapTest" $ do
    prop "parMap" $ parMapTest $ parMap ()
    prop "parMapStream" $ parMapTest $ parMapStream () 4
    prop "farm" $ parMapTest $ farm () 2
    prop "farmChunk" $ parMapTest $ farmChunk () 2 4
      where
          parMapTest :: ((Int -> Int) -> ([Int] -> [Int])) -> [Int] -> Bool
          parMapTest skel xs = skel (+1) xs == map (+1) xs
