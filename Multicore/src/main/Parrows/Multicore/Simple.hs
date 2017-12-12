{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Parrows.Multicore.Simple where

import Parrows.Multicore
import Control.Arrow

instance (NFData b, ArrowChoice arr, ArrowParallel arr a b (Conf b)) => ArrowParallel arr a b () where
    parEvalN _ fs = parEvalN (defaultConf fs) fs

instance (NFData b, ArrowChoice arr, ArrowParallel arr a b (), FutureEval arr a b (Conf b)) => FutureEval arr a b () where
    headStrictEvalN _ fs = headStrictEvalN (defaultConf fs) fs
    postHeadStrictEvalN _  fs = postHeadStrictEvalN (defaultConf fs) fs
