{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mutant.Control.Suspend where


import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Trans.Class (MonadTrans (..))


newtype Suspend m a
  = Suspend
  { runSuspend :: m (Either (Suspend m a) a) }


instance Functor m => Functor (Suspend m) where
  fmap f (Suspend ma) =
    Suspend
      $ either
          (Left . fmap f)
          (Right . f)
      <$> ma


instance Applicative m => Applicative (Suspend m) where
  pure = Suspend . pure . Right
  af <*> ax =
    Suspend
      $ go
      <$> runSuspend af
      <*> runSuspend ax
    where
      go
        :: Either (Suspend m (a -> b)) (a -> b)
        -> Either (Suspend m a) a
        -> Either (Suspend m b) b
      go (Right f) (Right x) =
          Right
          $ f x
      go (Right f) (Left s) =
          Left
          $ fmap f s
      go (Left sf) (Right x) =
          Left
          $ fmap ($ x) sf
      go (Left sf) (Left sx) =
          Left
          $ sf <*> sx


instance Monad m => Monad (Suspend m) where
  return = pure
  (>>=) ma amb =
    Suspend
      $ runSuspend ma >>= \case
          Left s -> return $ Left $ s >>= amb
          Right a -> runSuspend $ amb a


instance MonadTrans Suspend where
  lift = Suspend . fmap Right


instance MonadIO m => MonadIO (Suspend m) where
  liftIO = lift . liftIO


-- | Suspend until the next frame.
suspend
  :: Applicative m
  => Suspend m a
  -> Suspend m a
suspend = Suspend . pure . Left


-- | Be done with the computation.
done
  :: Applicative m
  => a
  -> Suspend m a
done = pure


-- | Wait some number of suspensions.
wait
  :: Applicative m
  => Int
  -> Suspend m ()
wait 0 = done ()
wait n = suspend
           $ wait
           $ n - 1


-- | Run both suspensions (interleaved left and then right) each frame and return
-- the first to complete.
race
  :: forall m a b
   . Applicative m
  => Suspend m a
  -> Suspend m b
  -> Suspend m (Either a b)
race ma mb =
  Suspend
    $ go
    <$> runSuspend ma
    <*> runSuspend mb
  where
    go
      :: Either (Suspend m a) a
      -> Either (Suspend m b) b
      -> Either (Suspend m (Either a b)) (Either a b)
    go (Right l) _         = Right $ Left l
    go _         (Right r) = Right $ Right r
    go (Left l)  (Left r)  = Left $ race l r
