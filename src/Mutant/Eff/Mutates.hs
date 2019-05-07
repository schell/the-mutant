{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
module Mutant.Eff.Mutates
  ( Mutates (..)
  , Slot (..)
  , mutatesTVar
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Kind              (Type)
import           GHC.Conc               (TVar, atomically, newTVar, readTVar,
                                         writeTVar)


-- | A mutable, threadsafe variable.
newtype Slot a
  = Slot { unSlot :: TVar a }




data Mutates (m :: Type -> Type)
  = Mutates
  { newSlot    :: forall a. a -> m (Slot a)
  , withSlot   :: forall a. Slot a -> (a -> a) -> m a
  , readSlot   :: forall a. Slot a -> m a
  , updateSlot :: forall a. Slot a -> a -> m ()
  }


mutatesTVar
  :: MonadIO m
  => Mutates m
mutatesTVar =
  let new a =
        Slot
          <$> liftIO (atomically $ newTVar a)
      with (Slot s) f =
        liftIO $ atomically $ do
          x <- readTVar s
          let x1 = f x
          writeTVar s x1
          return x1
      reed (Slot s) =
        liftIO $ atomically $ readTVar s

  in Mutates
     { newSlot = new
     , withSlot = with
     , readSlot = reed
     , updateSlot = \s a -> with s (const a) >> return ()
     }
