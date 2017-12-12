{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Parrows.Eden.Simple where

import Parrows.Eden
import Control.Arrow
import Control.Parallel.Eden (Trans)

instance (Trans b, ArrowChoice arr, ArrowParallel arr a b (Conf b)) => ArrowParallel arr a b () where
    parEvalN _ fs = parEvalN (defaultConf fs) fs

instance (Trans b, ArrowChoice arr, ArrowParallel arr a b (), FutureEval arr a b (Conf b)) => FutureEval arr a b () where
    headStrictEvalN _ fs = headStrictEvalN (defaultConf fs) fs
    postHeadStrictEvalN _  fs = postHeadStrictEvalN (defaultConf fs) fs

instance Future BasicFuture b () where
    put _ = put'
    get _ = get'
