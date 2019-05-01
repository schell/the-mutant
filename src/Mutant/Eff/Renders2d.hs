{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}
module Mutant.Eff.Renders2d where

import           Data.Kind            (Type)
--import           Data.Vector.Storable (Vector)
import           Linear               (V2)
import           Polysemy             hiding (run)


data Rect a
  = Rect
  { rectUpperLeft :: V2 a
  , rectExtents   :: V2 a
  }


data Renders2d (m :: Type -> Type) a where
  Clear         :: Renders2d m ()
  Present       :: Renders2d m ()
  GetDimensions :: Renders2d m (V2 Int)
  DrawLine      :: V2 Int -> V2 Int -> Renders2d m ()
  --DrawLines :: Vector (V2 Int) -> Renders2d m ()
  DrawRect      :: Rect Int -> Renders2d m ()
  --DrawRects :: [Rect Int] -> Renders2d m ()
  --CreateTexture
makeSem ''Renders2d
