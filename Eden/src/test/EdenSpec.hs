-- EdenParallelSpec
module EdenSpec (spec) where

import Parrows.Definition
import Parrows.Skeletons.Topology
import Parrows.Skeletons.Map
import Parrows.Eden()
import Parrows.Future

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
    prop "Pipe (Future) 4 times (+1)" $ pipeFutureTest
      where
         replicated :: [Int -> Int]
         replicated = map (+) [1..4]

         expectedValue :: Int -> Int
         expectedValue x = foldl (flip ($)) x replicated

         pipeTest :: Int -> Bool
         pipeTest x = pipe () replicated x == expectedValue x

         pipeFutureTest :: Int -> Bool
         pipeFutureTest x = (get (pipeFut () replicated (put x))) == expectedValue x

ringSpec :: Spec
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

mapSpec :: Spec
mapSpec = describe "mapTest" $ do
    prop "parMap" $ parMapTest $ parMap ()
    prop "parMapStream" $ parMapTest $ parMapStream () 4
    prop "farm" $ parMapTest $ farm () 2
    prop "farmChunk" $ parMapTest $ farmChunk () 2 4
      where
          parMapTest :: ((Int -> Int) -> ([Int] -> [Int])) -> [Int] -> Bool
          parMapTest skel xs = skel (+1) xs == map (+1) xs
