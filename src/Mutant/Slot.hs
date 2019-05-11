{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
module Mutant.Slot where

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


withSlot
  :: MonadIO m => Slot a -> (a -> a) -> m a
withSlot (Slot s) f = liftIO $ atomically $ do
  x <- readTVar s
  let x1 = f x
  writeTVar s x1
  return x1


readSlot :: MonadIO m => Slot a -> m a
readSlot = liftIO . atomically . readTVar . unSlot


writeSlot :: MonadIO m => Slot a -> a -> m ()
writeSlot s a = withSlot s (const a) >> return ()
