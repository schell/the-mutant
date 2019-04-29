{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Lib
    ( someFunc
    ) where

import           Control.Lens                     ((^.))
import           Control.Monad                    (void)
import           Data.Kind                        (Type)
import           Language.Javascript.JSaddle      (Function, JSM, JSVal,
                                                   function, js, js1, js2, jsg,
                                                   jss)
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
  => Sem (UI Web ': r) a
  -> Sem r a
runUI = interpretH $ \case
  GetStage -> sendM $ do
    node <- Node @Web <$> jsg "document"
    return node
  AddListener node EventClick m -> sendM $ do
    f   <- function $ \_ _ _ -> m
    void $ (unNode node) ^. js2 "addEventListener" "click" f
    return Listener{ listenerEvent = EventClick
                   , listenerNode  = node
                   , listenerCall  = f
                   }


runJSM
  :: Member (Lift IO) r
  => Sem (Lift JSM ': r) a
  -> Sem r a
runJSM = interpret $ \(Lift jsm) -> sendM $ do
  run 8888 $ jsm >> return undefined
  return undefined


myApp
  :: forall ctx r
   . ( Member (UI ctx) r
     , Member (Lift IO) r
     )
  => Sem r ()
myApp = do
  stage <- getStage @ctx
  addListener stage EventClick
    $ sendM
    $ putStrLn "clicked!"



someFunc :: IO ()
someFunc = do
  putStrLn "starting the app at http://localhost:8888"
  runM
    $ runJSM
    $ runUI
    $ myApp @Web
