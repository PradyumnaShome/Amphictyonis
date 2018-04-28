module Util where

import Control.Monad

whenM :: Monad m => m Bool -> m () -> m ()
whenM v a = v >>= (`when` a)

