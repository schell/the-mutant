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
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin         #-}
module Lib
    ( someFunc
    ) where

import           Control.Lens                     ((^.))
import           Control.Monad                    (void)
import           Data.Kind                        (Type)
import           Language.Javascript.JSaddle      (Function, JSContextRef, JSM,
                                                   JSVal, askJSM, function, js,
                                                   js1, js2, jsg, jss, runJSM)
import           Language.Javascript.JSaddle.Warp (run)
import           Polysemy                         hiding (run)
import           Polysemy.IO                      (runIO)



class HasUI ctx where
  type NodeType ctx
  type Callback ctx


newtype Node ctx
  = Node { unNode :: NodeType ctx }


data Event
  = EventClick


data Listener ctx
  = Listener
  { listenerEvent :: Event
  , listenerNode  :: Node ctx
  , listenerCall  :: Callback ctx
  }


data UI ctx m a where
  -- | Get a referenc to the root/stage node.
  GetStage    :: UI ctx m (Node ctx)
  -- | Append a child node to another node.
  --AppendChild :: Node ctx -> Node ctx -> next -> UICmd ctx next
  -- | Add some listener for an event to the node.
  AddListener :: Node ctx -> Event -> m () -> UI ctx m (Listener ctx)

makeSem_ ''UI


getStage
  :: Member (UI ctx) r
  => Sem r (Node ctx)


addListener
  :: Member (UI ctx) r
  => Node ctx
  -> Event
  -> Sem r ()
  -> Sem r (Listener ctx)


data Web

-- | JSM implementation
instance HasUI Web where
  type NodeType Web = JSVal
  type Callback Web = Function


runUI
  :: forall r a
   . Member (Lift JSM) r
  => (forall x. Sem r x -> JSM x)
  -> Sem (UI Web ': r) a
  -> Sem r a
runUI finish = interpretH $ \case
  GetStage ->
    sendM (Node @Web <$> jsg "document")
      >>= pureT
  AddListener node EventClick m -> do
    m1  <- runT m
    ref <- sendM askJSM

    let runIt
          :: Member (Lift JSM) r
          => Sem (UI Web ': r) x
          -> JSM x
        runIt = finish .@ runUI

    cb <- sendM
      $ function
      $ \_ _ _ -> void $ runIt m1

    sendM (void $ (unNode node) ^. js2 "addEventListener" "click" cb)

    pureT Listener{ listenerEvent = EventClick
                  , listenerNode  = node
                  , listenerCall  = cb
                  }


runJSMEff
  :: Member (Lift IO) r
  => Sem (Lift JSM ': r) a
  -> Sem r a
runJSMEff = undefined
  --interpret $ \(Lift jsm) -> sendM $ do
  --  run 8888 $ jsm >> return undefined
  --  return undefined


myApp
  :: forall ctx r
   . ( Member (UI ctx) r
     , Member (Lift IO) r
     )
  => Sem r ()
myApp = do
  stage <- getStage @ctx
  void
    $ addListener stage EventClick
    $ sendM
    $ putStrLn "clicked!"


myWebApp :: Semantic [UI Web, Lift JSM] ()
myWebApp = myApp

someFunc :: IO ()
someFunc = do
  putStrLn "starting the app at http://localhost:8888"
  runM
    $ runJSMEff
    $ runUI (runM . runJSMEff) myWebApp
