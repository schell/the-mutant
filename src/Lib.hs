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
module Lib where

import           Control.Lens                     ((^.))
import           Control.Monad                    (void)
import           Control.Monad.IO.Class           (MonadIO (..))
import           Data.Kind                        (Type)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           GHC.Conc                         (TVar, atomically, newTVar,
                                                   readTVar, writeTVar)
import           Language.Javascript.JSaddle      (Function, JSM, JSVal,
                                                   function, js, js1, js2, jsg,
                                                   jss)
import           Language.Javascript.JSaddle.Warp (run)
import           Polysemy                         hiding (run)
import           Polysemy.IO                      (runIO)



class HasUI ctx where
  -- | The type of a UI node that can be added to a display graph.
  type NodeType ctx
  -- | An external(ish) callback.
  type Callback ctx


newtype Node ctx
  = Node { nodeValue :: NodeType ctx }


data Event
  = EventClick


data Listener ctx
  = Listener
  { listenerEvent :: Event
  , listenerNode  :: Node ctx
  , listenerCall  :: Callback ctx
  }


newtype Stage ctx
  = Stage
  { stageNode :: Node ctx }


newtype Label ctx
  = Label
  { labelNode :: Node ctx }


data UI ctx m a where
  -- | Get a referenc to the root/stage node.
  GetStage :: UI ctx m (Stage ctx)

  -- | Create a text label.
  NewLabel :: Text -> UI ctx m (Label ctx)

  -- | Change the text on a label.
  UpdateLabel :: Label ctx -> Text -> UI ctx m ()

  -- | Append a child node to another node.
  AppendChild :: Node ctx -> Node ctx -> UI ctx m ()

  -- | Add some listener for an event to the node.
  AddListener :: Node ctx -> Event -> m () -> UI ctx m (Listener ctx)

makeSem_ ''UI


getStage
  :: Member (UI ctx) r
  => Sem r (Stage ctx)


newLabel
  :: Member (UI ctx) r
  => Text
  -> Sem r (Label ctx)


updateLabel
  :: Member (UI ctx) r
  => Label ctx
  -> Text
  -> Sem r ()


addListener
  :: Member (UI ctx) r
  => Node ctx
  -> Event
  -> Sem r ()
  -> Sem r (Listener ctx)


appendChild
  :: Member (UI ctx) r
  => Node ctx
  -> Node ctx
  -> Sem r ()


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
  GetStage -> do
    body <- sendM $ do
      doc <- jsg "document"
      doc ^. js "body"
    pureT
      $ Stage
      $ Node body
  NewLabel txt -> do
    node <- sendM $ jsg "document" ^. js1 "createTextNode" txt
    pureT
      $ Label
      $ Node node
  UpdateLabel lbl txt -> do
    void $ sendM $ (nodeValue $ labelNode lbl) ^. jss "textContent" txt
    pureT ()
  AppendChild (Node parent) (Node child) -> do
    sendM $ void $ parent ^. js1 "appendChild" child
    pureT ()
  AddListener node EventClick m -> do
    m1  <- runT m

    let runIt
          :: Member (Lift JSM) r
          => Sem (UI Web ': r) x
          -> JSM x
        runIt = finish .@ runUI

    cb <- sendM
      $ function
      $ \_ _ _ -> void $ runIt m1

    sendM (void $ (nodeValue node) ^. js2 "addEventListener" "click" cb)

    pureT Listener{ listenerEvent = EventClick
                  , listenerNode  = node
                  , listenerCall  = cb
                  }




myApp
  :: ( Member (UI ctx) r
     , Member (Lift IO ) r
     )
  => Sem r ()
myApp = do
  stage <- getStage
  label <- newLabel $ T.pack "Count: ?"
  var   <- liftIO $ atomically $ newTVar (0 :: Int)
  appendChild (stageNode stage) (labelNode label)

  void
    $ addListener (stageNode stage) EventClick
    $ do
      clicks <- liftIO $ atomically $ do
        n <- readTVar var
        let n1 = succ n
        writeTVar var n1
        return n1
      updateLabel label
        $ T.pack
        $ unwords
          [ "Count: "
          , show clicks
          ]

  liftIO
    $ putStrLn "ready..."


someFunc :: IO ()
someFunc = do
  putStrLn "starting the app at http://localhost:8888"
  run 8888
    $ runM
    $ runIO @JSM
    $ runUI (runM . runIO @JSM) myApp
