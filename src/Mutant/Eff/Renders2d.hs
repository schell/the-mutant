{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}
module Mutant.Eff.Renders2d where

import Data.Vector.Storable (Vector)
import           Data.Kind (Type)
import           Polysemy  hiding (run)


data Renders2d m a where
  Clear    :: Renders2d m ()
  --DrawLine ::
  --CreateTexture
