module EdenSpec(spec) where

import Parrows.Eden.Simple()

import BaseSpec.CompleteSpecBase

import Test.Hspec

spec :: Spec
spec = describe "Multicore PArrows Check" $ do
    specBase
