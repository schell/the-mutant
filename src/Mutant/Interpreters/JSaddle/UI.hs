{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin         #-}
module Mutant.Interpreters.JSaddle.UI where

import           Control.Arrow                    ((&&&))
import           Control.Exception                (Exception, throwIO)
import           Control.Lens                     ((^.))
import           Control.Monad                    (void)
import           Control.Monad.IO.Class           (MonadIO (..))
import           Data.IntMap.Strict               (IntMap)
import qualified Data.IntMap.Strict               as IM
import qualified Data.Text                        as T
import           GHC.Conc                         (TVar, atomically, newTVar,
                                                   readTVar, writeTVar)
import           Language.Javascript.JSaddle      (JSM, JSVal, function, js,
                                                   js1, js2, jsg, jss)
import           Language.Javascript.JSaddle.Warp (run)
import           Polysemy                         hiding (run)
import           Polysemy.Error                   (Error, runError, throw)
import           Polysemy.IO                      (runIO)
import           Polysemy.State                   (State (..), gets, modify)

import           Mutant.Eff.Mutates
import           Mutant.Eff.UI


data BrowserData
  = BrowserData
  { browserDataNextK     :: Int
  , browserDataStage     :: Maybe Stage
  , browserDataNodes     :: IntMap JSVal
  , browserDataListeners :: IntMap JSVal
  }


initialBrowserData :: BrowserData
initialBrowserData =
  BrowserData
  { browserDataNextK     = 0
  , browserDataStage     = Nothing
  , browserDataNodes     = mempty
  , browserDataListeners = mempty
  }


data BrowserError
  = BrowserErrorDoesNotExist String
  deriving anyclass Exception
deriving instance Show BrowserError -- WTF GHC


freshK
  :: Member (State BrowserData) r
  => Sem r Int
freshK = do
  modify (\d -> d{ browserDataNextK = succ $ browserDataNextK d})
  gets browserDataNextK


getNode
  :: ( Member (State BrowserData) r
     , Member (Error BrowserError) r
     )
  => Node
  -> Sem r JSVal
getNode node =
  gets (IM.lookup (nodeId node) . browserDataNodes)
    >>= maybe (throw $ BrowserErrorDoesNotExist $ show node) return


getListener
  :: ( Member (State BrowserData) r
     , Member (Error BrowserError) r
     )
  => Listener
  -> Sem r (JSVal, JSVal)
getListener l = do
  n <- getNode $ Node $ listenerNodeId l
  f <- gets (IM.lookup (listenerId l) . browserDataListeners)
    >>= maybe (throw $ BrowserErrorDoesNotExist $ show l) return
  return (n, f)


eventToString
  :: Event
  -> String
eventToString = \case
  EventClick -> "click"



runUI
  :: forall r a
   . Member (Lift JSM) r
  => Member (State BrowserData) r
  => Member (Error BrowserError) r
  => (forall x. Sem r x -> JSM x)
  -> Sem (UI ': r) a
  -> Sem r a
runUI finish = interpretH $ \case
  GetStage -> do
    stage <-
      gets browserDataStage >>= \case
        Nothing -> do
          body <- sendM $ do
            doc <- jsg "document"
            doc ^. js "body"
          k <- freshK
          let stage = Stage $ Node k
          modify $ \dat ->
            dat{ browserDataNodes = IM.insert k body $ browserDataNodes dat
               , browserDataStage = Just stage
               }
          return stage
        Just stage -> return stage
    pureT stage

  NewLabel txt -> do
    jsnode <- sendM $ jsg "document" ^. js1 "createTextNode" txt
    k      <- freshK
    modify $ \dat ->
      dat{ browserDataNodes = IM.insert k jsnode $ browserDataNodes dat}
    pureT
      $ Label
      $ Node k

  UpdateLabel lbl txt -> do
    jsnode <- getNode (labelNode lbl)
    void $ sendM $ jsnode ^. jss "textContent" txt
    pureT ()

  AppendChild parent child -> do
    pv <- getNode parent
    cv <- getNode child
    sendM $ void $ pv ^. js1 "appendChild" cv
    pureT ()

  AddListener node ev m -> do
    val <- getNode node
    m1  <- runT m

    let runIt
          :: Member (Lift JSM) r
          => Sem (UI ': r) x
          -> JSM x
        runIt = finish .@ runUI

    cb <- sendM
      $ function
      $ \_ _ _ -> void $ runIt m1

    jslistener <- sendM (val ^. js2 "addEventListener" (eventToString ev) cb)

    (k, ls) <- gets (browserDataNextK &&& browserDataListeners)

    modify
      $ \dat ->
        dat{ browserDataNextK = succ k
           , browserDataListeners = IM.insert k jslistener ls
           }

    pureT Listener{ listenerId     = k
                  , listenerNodeId = nodeId node
                  , listenerEvent  = ev
                  }

  RemoveListener listener -> do
    (n, l) <- getListener listener
    let ev = eventToString $ listenerEvent listener
    sendM (void $ n ^. js2 "removeEventListener" ev l)
    pureT ()



myApp
  :: ( Member UI r
     , Member (Lift IO ) r
     , Member Mutates r
     )
  => Sem r ()
myApp = do
  stage <- getStage
  label <- newLabel $ T.pack "Count: 0"
  var   <- newSlot @Int 0
  appendChild (stageNode stage) (labelNode label)

  void
    $ addListener (stageNode stage) EventClick
    $ do
      clicks <- withSlot var succ
      updateLabel label
        $ T.pack
        $ unwords
          [ "Count: "
          , show clicks
          ]

  liftIO
    $ putStrLn "ready..."


runStateInTVar
  :: forall r x a
   . Member (Lift IO) r
  => TVar x
  -> Sem (State x ': r) a
  -> Sem r a
runStateInTVar var = interpretH $ \case
  Get ->
    sendM (liftIO $ atomically $ readTVar var)
      >>= pureT
  Put x ->
    sendM (liftIO $ atomically $ writeTVar var x)
      >> pureT ()


uiTest :: IO ()
uiTest = do
  putStrLn "starting the app at http://localhost:8888"
  var <- atomically $ newTVar initialBrowserData
  run 8888
    $ void
    $ runM
    $ runIO @JSM
    $ runError
    $ runSlot
    $ runStateInTVar var
    $ runUI
        ( handleEnd
        . runM
        . runIO @JSM
        . runError
        . runSlot
        . runStateInTVar var
        )
        myApp
  where
    handleEnd
      :: JSM (Either BrowserError x) -> JSM x
    handleEnd f =
      f >>=
        either
          (liftIO . throwIO)
          return
