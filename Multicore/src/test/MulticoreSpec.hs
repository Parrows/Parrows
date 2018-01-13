module MulticoreSpec(spec) where

import Parrows.Multicore.Simple()

import BaseSpec.CompleteSpecBase

import Test.Hspec

spec :: Spec
spec = describe "Multicore PArrows Check" $ do
    specBase
