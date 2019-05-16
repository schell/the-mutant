module Mutant.Prelude
  ( module Mutant.Prelude
  ) where

import           Data.Function      (fix)


while :: Monad m => m Bool -> m () -> m ()
while mPred mOther =
  fix $ \loop -> do
    p <- mPred
    if p
    then mOther >> loop
    else return ()
