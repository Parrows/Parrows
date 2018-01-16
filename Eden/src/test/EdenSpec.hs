{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module EdenSpec(spec) where

import Parrows.Eden.Simple()

import BaseSpec.CompleteSpecBase

import Test.Hspec

spec :: Spec
spec = describe "Eden PArrows Check" $ do
    specBase
