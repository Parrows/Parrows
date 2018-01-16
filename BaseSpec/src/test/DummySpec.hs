module DummySpec(spec) where

import Parrows.Dummy.Simple()

import BaseSpec.CompleteSpecBase

import Test.Hspec

spec :: Spec
spec = describe "Dummy PArrows Check" $ do
    specBase
