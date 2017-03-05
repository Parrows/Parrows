-- EdenParallelSpec
module EdenParallelSpec (spec) where

import Parrows.Definition
import Parrows.Future
import Parrows.Eden

import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    basicStuff
    pipeTest

basicStuff :: Spec
basicStuff = describe "Basic Parrow Functionality Eden" $ do
    prop "Basic parEvalN" $ basicParEvalN
    prop "ParEvalNFut" $ parEvalNFutInt
     where
        basicParEvalN :: [Int] -> Bool
        basicParEvalN xs =  parEvalN () (repeat (+1)) xs == map (+1) xs

        parEvalNFutInt :: [Int] -> Bool
        parEvalNFutInt xs = (map (get) $ parEvalNFut () (repeat (+1)) (map put xs)) == map (+1) xs

pipeTest :: Spec
pipeTest = describe "Pipe Test" $ do
    prop "Pipe 4 times (+1)" $ pipeTest
      where
         pipeTest :: Int -> Bool
         pipeTest x = (get (pipe () replicated (put x))) == x + 4
             where
                 replicated = replicate 4 (+1)
