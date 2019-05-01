{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}
module Mutant.Eff.Braided where

import           Data.Kind (Type)
import           Polysemy  hiding (run)


-- | An effect for braiding (interleaving) computations.
data Braided x y (m :: Type -> Type) a where
  Suspend :: x -> (y -> m a) -> Braided x y m a
makeSem_ ''Braided


-- | Suspend the computation, giving an interleaved action an 'x'.
-- When the interleaved action completes, it gives a 'y' which can
-- be used to continue.
suspend
  :: Member (Braided x y) r
  => x
  -> (y -> Sem r a)
  -> Sem r a

inlineRecursiveCalls [d|
  runBraided
    :: forall r x y a
    . (x -> Sem r y)
    -- ^ Kliesli arrow that handles any yielding.
    -- This is your interleaved coroutine.
    -> Sem ((Braided x y) ': r) a
    -> Sem r a
  runBraided karr = interpretH $ \(Suspend x m) ->
    raise (karr x)
      >>= runT . m
      >>= raise . runBraided karr
  |]
