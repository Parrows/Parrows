{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module BaseSpec.CompleteSpecBase(specBase) where

import BaseSpec.BasicSpecBase
import BaseSpec.SkeletonCheckBase
import BaseSpec.TorusSpecBase

import Parrows.Definition
import Parrows.Future

import Test.Hspec

specBase :: (
  Future fut Int (),
  Future fut [Matrix] (),
  Future fut (([Int], [Int]), [Int]) (),
  ArrowParallel (->) [Int] [Int] (),
  ArrowParallel (->) (Int, [Int]) Int (),
  ArrowParallel (->) (Either Int Int) (Either Int Int) (),
  ArrowLoopParallel (->) Int Int (),
  ArrowLoopParallel (->) [Matrix] [Matrix] (),
  ArrowLoopParallel (->) (fut Int) (fut Int) (),
  ArrowLoopParallel (->) (Int, fut Int) (Int, fut Int) (),
  ArrowLoopParallel (->) (fut (([Int], [Int]), [Int])) (fut (([Int], [Int]), [Int])) (),
  ArrowLoopParallel (->) ((Matrix, Matrix), fut [Matrix], fut [Matrix]) (Matrix, fut [Matrix], fut [Matrix]) ()) =>
  Spec
specBase = do
    basicSpec
    pipeSpec
    ringSpec
    mapSpec
    mapReduceSpec
    torusSpec
