{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module BaseSpec.BasicSpecBase (basicSpec) where

import Parrows.Definition
import Parrows.Future

import Control.Arrow()

import Test.Hspec
import Test.Hspec.QuickCheck

basicSpec :: (ArrowParallel (->) Int Int (),
  Future fut Int (), ArrowParallel (->) (fut Int) (fut Int) (),
  ArrowParallel (->) (Either Int Int) (Either Int Int) ()) => Spec
basicSpec = describe "Basic Functionality Check" $ do
    prop "Basic parEvalN" $ basicParEvalN
    prop "parEvalNLazy" $ parEvalNLazyInt
    prop "parEvalNFut" $ parEvalNFutInt
    prop "parEval2" $ parEval2Int
     where
        basicParEvalN :: [Int] -> Bool
        basicParEvalN xs = parEvalN () (repeat (+1)) xs == map (+1) xs

        parEvalNLazyInt :: [Int] -> Bool
        parEvalNLazyInt xs = parEvalNLazy () 4 (repeat (+1)) xs == map (+1) xs

        parEvalNFutInt :: [Int] -> Bool
        parEvalNFutInt xs = (map (get ()) (parEvalNFut () (repeat (+1)) (map (put ()) xs))) == map (+1) xs

        parEval2Int :: (Int, Int) -> Bool
        parEval2Int (x, y) = (parEval2 () (+1) (*2) (x, y)) == (x+1,y*2)
