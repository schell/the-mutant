{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
module Mutant.Slot where

import           Control.Arrow          ((&&&))
import           Control.Monad.IO.Class (MonadIO (..))
import           GHC.Conc               (TVar, atomically, newTVar, readTVar,
                                         writeTVar)


-- | A mutable, threadsafe variable.
newtype Slot a
  = Slot { unSlot :: TVar a }


newSlot
  :: MonadIO m
  => a
  -> m (Slot a)
newSlot a =
  Slot
    <$> liftIO (atomically $ newTVar a)


stateSlot
  :: MonadIO m
  => Slot s
  -> (s -> (a, s))
  -> m a
stateSlot (Slot s) f = liftIO $ atomically $ do
  x <- readTVar s
  let (a, x1) = f x
  writeTVar s x1
  return a


withSlot
  :: MonadIO m
  => Slot a
  -> (a -> a)
  -> m a
withSlot s f = stateSlot s (f &&& f)


readSlot :: MonadIO m => Slot a -> m a
readSlot = liftIO . atomically . readTVar . unSlot


writeSlot :: MonadIO m => Slot a -> a -> m ()
writeSlot s a = stateSlot s $ const ((), a)
