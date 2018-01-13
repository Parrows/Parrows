module ParMonadSpec(spec) where

import Parrows.ParMonad.Simple()

import BaseSpec.CompleteSpecBase

import Test.Hspec

spec :: Spec
spec = describe "Par Monad PArrows Check" $ do
    specBase
