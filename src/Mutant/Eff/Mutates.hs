{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Mutant.Eff.Mutates
  ( Mutates
  , Slot (..)
  , newSlot
  , withSlot
  , readSlot
  , updateSlot
  , runSlot
  ) where

import Control.Monad.IO.Class (liftIO)
import           Data.Kind (Type)
import           GHC.Conc  (TVar, atomically, newTVar, readTVar, writeTVar)
import           Polysemy


-- | A mutable, threadsafe variable.
newtype Slot a
  = Slot (TVar a)


data Mutates (m :: Type -> Type) a where
  NewSlot :: a -> Mutates m (Slot a)
  WithSlot :: Slot a -> (a -> a) -> Mutates m a
makeSem ''Mutates


readSlot
  :: Member Mutates r
  => Slot a
  -> Sem r a
readSlot = flip withSlot id


updateSlot
  :: Member Mutates r
  => Slot a
  -> a
  -> Sem r ()
updateSlot s a = do
  _ <- withSlot s $ const a
  return ()


runSlot
  :: Member (Lift IO) r
  => Sem (Mutates ': r) a
  -> Sem r a
runSlot = interpretH $ \case
  NewSlot a ->
    sendM (liftIO $ atomically $ newTVar a)
      >>= pureT . Slot
  WithSlot (Slot s) f -> do
    a <- sendM $ liftIO $ atomically $ do
      x <- readTVar s
      let x1 = f x
      writeTVar s x1
      return x1
    pureT a
