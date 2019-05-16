{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Mutant.Backends.JSaddle
  ( getJSInstance
  , getJSRender2dAPI
  , getJSEventsAPI
  , getJSAnimationFrameAPI
  , runJS
  ) where

import           Control.Concurrent                     (threadDelay)
import           Control.Concurrent.Async               (async, race_)
import           Control.Concurrent.MVar                (MVar, newEmptyMVar,
                                                         putMVar, takeMVar,
                                                         tryTakeMVar)
import           Control.Lens                           ((^.))
import           Control.Monad                          (void)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           GHC.Clock                              (getMonotonicTime)
import           Language.Javascript.JSaddle            (Function,
                                                         JSException (..), JSM,
                                                         JSVal, MonadJSM,
                                                         askJSM, catch,
                                                         fromJSValUnchecked,
                                                         fun, function, js, js0,
                                                         js1, js2, js3, js4,
                                                         jsf, jsg, jss, liftJSM,
                                                         new,
                                                         nextAnimationFrame,
                                                         toJSVal, valToText)
import           Language.Javascript.JSaddle.WebSockets (jsaddleWithAppOr)
import           Linear                                 (V2 (..), V4 (..))
import           Network.Wai.Application.Static         (defaultWebAppSettings,
                                                         staticApp)
import           Network.Wai.Handler.Warp               (run)
import           Network.WebSockets.Connection          (defaultConnectionOptions)

import           Mutant.API.Events
import           Mutant.API.Render2d
import           Mutant.Backend
import           Mutant.Geom
import           Mutant.Slot


data Canvas
  = Canvas
  { canvasWindow      :: JSVal
  , canvasCtx         :: JSVal
  , canvasAssetPrefix :: String
  }


data instance Texture 'BackendJS = JSTexture JSVal


data instance Font 'BackendJS = JSFont String


data instance MutantInstance 'BackendJS = JSInstance Canvas


_catchAndPrint :: JSM () -> JSM ()
_catchAndPrint f =
  void $ f `catch` \(JSException e) -> valToText e >>= liftIO . print


getNewJSCanvas :: MonadJSM m => V2 Int -> String -> m Canvas
getNewJSCanvas (V2 w h) assetPrefix = liftJSM $ do
  doc  <- jsg "document"
  cvs  <- doc ^. js1 "createElement" "canvas"
  void $ cvs ^. jss "width" w
  void $ cvs ^. jss "height" h
  void $ cvs ^. jss "id" "the-mutant-root"
  ctx  <- cvs ^. js1 "getContext" "2d"
  return Canvas
         { canvasWindow = cvs
         , canvasCtx = ctx
         , canvasAssetPrefix = assetPrefix
         }


getDims :: MonadJSM m => Canvas -> m (V2 Int)
getDims canvas = liftJSM $ do
  w <- canvasWindow canvas ^. js "width"
  h <- canvasWindow canvas ^. js "height"
  V2
    <$> fromJSValUnchecked w
    <*> fromJSValUnchecked h


toCSSColor :: V4 Int -> String
toCSSColor (V4 r g b a) =
  unwords
    [ "rgba("
    , show r ++ ","
    , show g ++ ","
    , show b ++ ","
    , show (fromIntegral @_ @Double a/255.0)
    ]


initiateTexture :: JSVal -> JSVal -> JSVal -> Slot (LoadStatus (Texture 'BackendJS)) -> JSM ()
initiateTexture cvs img tex tvar = do
  s <- newSlot Nothing
  let removeListeners = do
        readSlot s >>= \case
          Nothing -> return () -- absurd
          Just (cb, errcb) -> do
            void $ img ^. js2 "removeEventListener" "load" (cb :: Function)
            void $ img ^. js2 "removeEventListener" "error" (errcb :: Function)
  zero <- toJSVal (0 :: Int)
  cb <-
    function $ \_ _ _ -> do
      width <- img ^. js "naturalWidth"
      height <- img ^. js "naturalHeight"
      w :: Int <- fromJSValUnchecked width
      h :: Int <- fromJSValUnchecked height
      void $ cvs ^. jss "width" w
      void $ cvs ^. jss "height" h
      let args = [zero, zero, width, height, zero, zero, width, height]
      void $ tex ^. jsf "drawImage" (img : args)
      void $ tex ^. jss "complete" True
      writeSlot tvar
        $ LoadStatusSuccess
        $ JSTexture tex
      removeListeners
  errcb <-
    function $ \_ _ _ -> do
      writeSlot tvar
        $ LoadStatusFailure "could not load texture"
      removeListeners
  void $ img ^. js2 "addEventListener" "load" cb
  void $ img ^. js2 "addEventListener" "error" errcb


-- https://developer.mozilla.org/en-US/docs/Web/API/FontFace/FontFace
initiateFont
  :: Slot (LoadStatus (Font 'BackendJS))
  -> FilePath
  -> String
  -> JSM ()
initiateFont fvar path fontName = do
  ff <- jsg "FontFace"
  fontFace <- new ff (fontName, "url(" ++ path ++ ")")
  promise <- fontFace ^. js0 "load"
  let onload = fun $ \_ _ _ -> do
        doc <- jsg "document"
        void $ doc ^. js "fonts" ^. js1 "add" fontFace
        writeSlot
          fvar
          $ LoadStatusSuccess
          $ JSFont fontName
      onfail = fun $ \_ _ [err] -> do
        msg <- fromJSValUnchecked =<< err ^. js "message"
        writeSlot
          fvar
          $ LoadStatusFailure msg
  void $ promise ^. js1 "then" onload
  void $ promise ^. js1 "catch" onfail


toCSSFontStr :: String -> V2 Int -> String
toCSSFontStr font (V2 w _) = unwords [show w ++ "px", font]


getJSRender2dAPI
  :: MonadJSM m
  => MutantInstance 'BackendJS
  -> m (Render2dAPI 'BackendJS m)
getJSRender2dAPI (JSInstance c) = do
  s <- newSlot c

  k <- newSlot (0 :: Int)
  return
    Render2dAPI
    { clear = do
        canvas <- readSlot s
        V2 w h <- getDims canvas
        void
          $ liftJSM
          $ canvasCtx canvas ^. js4 "clearRect" (0 :: Int) (0 :: Int) w h

    , present = return ()

    , getDimensions = readSlot s >>= getDims

    , setDrawColor = \v4Color -> do
        canvas <- readSlot s
        liftJSM $ do
          let color = toCSSColor v4Color
          void $ canvasCtx canvas ^. jss "fillStyle" color
          void $ canvasCtx canvas ^. jss "strokeStyle" color

    , strokeLine = \(Line (V2 x1 y1) (V2 x2 y2)) -> do
        canvas <- readSlot s
        liftJSM $ do
          void $ canvasCtx canvas ^. js0 "beginPath"
          void $ canvasCtx canvas ^. js2 "moveTo" x1 y1
          void $ canvasCtx canvas ^. js2 "lineTo" x2 y2
          void $ canvasCtx canvas ^. js0 "stroke"

    , strokeRect = \(Rect (V2 x y) (V2 w h)) -> do
        canvas <- readSlot s
        void
          $ liftJSM
          $ canvasCtx canvas ^. js4 "strokeRect" x y w h

    , fillRect = \(Rect (V2 x y) (V2 w h)) -> liftJSM $ do
        canvas <- readSlot s
        void $ canvasCtx canvas ^. js4 "fillRect" x y w h

    , fillTexture = \(JSTexture tex) source dest -> do
        canvas <- readSlot s
        liftJSM $ do
          let Rect (V2 sx sy) (V2 sw sh) = source
              Rect (V2 dx dy) (V2 dw dh) = dest
          cvs  <- tex ^. js "canvas"
          args <- traverse toJSVal [sx,sy,sw,sh,dx,dy,dw,dh]
          void $ canvasCtx canvas ^. jsf "drawImage" (cvs : args)

    , fillText = \(JSFont font) sz (V2 x y) txt -> liftJSM $ do
        let fontStr = toCSSFontStr font sz
        canvas <- readSlot s
        void $ canvasCtx canvas ^. jss "font" fontStr
        void $ canvasCtx canvas ^. jsf "fillText" [txt, show x, show y]

    , measureText = \(JSFont font) sz@(V2 _ h) str -> liftJSM $ do
        let fontStr = toCSSFontStr font sz
        canvas <- readSlot s
        void $ canvasCtx canvas ^. jss "font" fontStr
        metrics <- canvasCtx canvas ^. js1 "measureText" str
        width <- fromJSValUnchecked =<< metrics ^. js "width"
        return
          $ V2 width
          $ h * length (lines str)

    , texture = \fp -> do
        canvas <- readSlot s
        liftJSM $ do
          let imgPath = canvasAssetPrefix canvas ++ fp
          doc <- jsg "document"
          img <- doc ^. js1 "createElement" "img"
          cvs <- doc ^. js1 "createElement" "canvas"
          tex <- cvs ^. js1 "getContext" "2d"
          void $ tex ^. jss "complete" False
          void $ cvs ^. jss "width" (0 :: Int)
          void $ cvs ^. jss "height" (0 :: Int)
          tvar <- newSlot LoadStatusLoading
          initiateTexture cvs img tex tvar
          img ^. jss "src" imgPath
          return tvar

    , textureSize = \(JSTexture tex) -> do
        liftJSM $ do
          cvs <- tex ^. js "canvas"
          V2 <$> (cvs ^. js "width" >>= fromJSValUnchecked)
             <*> (cvs ^. js "height" >>= fromJSValUnchecked)

    , setRenderTarget = \case
        Nothing -> writeSlot s c
        Just (JSTexture tex) -> do
          canvas <- readSlot s
          cvs <- liftJSM $ tex ^. js "canvas"
          let newCanvas = Canvas cvs tex (canvasAssetPrefix canvas)
          writeSlot s newCanvas

    , font = \file -> liftJSM $ do
        fvar <- newSlot LoadStatusLoading
        n <- withSlot k succ
        canvas <- readSlot s
        let url = canvasAssetPrefix canvas ++ file
            nam = "font" ++ show n
        initiateFont fvar url nam
        return fvar
    }


getJSInstance :: V2 Int -> JSM (MutantInstance 'BackendJS)
getJSInstance sz = do
  cvs <- getNewJSCanvas sz "http://localhost:8888/"
  doc <- jsg "document"
  bdy <- doc ^. js "body"
  void $ bdy ^. js1 "appendChild" (canvasWindow cvs)
  return $ JSInstance cvs


data EventState
  = EventState
  { eventStateMousePos :: V2 Int
  } deriving (Show)


emptyEventState :: EventState
emptyEventState =
  EventState
  { eventStateMousePos = 0 }


mouseEventButtonsToButtons :: Int -> [MouseButton]
mouseEventButtonsToButtons = \case
  0 -> []
  1 -> [MouseButtonLeft]
  2 -> [MouseButtonRight]
  3 -> [MouseButtonLeft, MouseButtonRight]
  4 -> [MouseButtonMiddle]
  _ -> []


newEvent
  :: MonadIO m
  => EventPayload
  -> m Event
newEvent p = do
  Event
    <$> liftIO (round . (1000 *) <$> getMonotonicTime)
    <*> pure p


mouseMoveHandler
  :: MonadJSM m
  => Canvas
  -> Slot EventState
  -> Slot [Event]
  -> MVar ()
  -> m Function
mouseMoveHandler c sVar qVar mVar = liftJSM $ function $ \_ _ -> \case
  [] -> return () -- absurd
  event:_ -> do
    cvsX <-
      fromJSValUnchecked =<< canvasWindow c ^. js "offsetLeft"
    cvsY <-
      fromJSValUnchecked =<< canvasWindow c ^. js "offsetTop"
    clientX <-
      fromJSValUnchecked =<< event ^. js "clientX"
    clientY <-
      fromJSValUnchecked =<< event ^. js "clientY"
    relX <-
      fromJSValUnchecked =<< event ^. js "movementX"
    relY <-
      fromJSValUnchecked =<< event ^. js "movementY"
    btnNum <-
      fromJSValUnchecked =<< event ^. js "buttons"
    s <-
      withSlot sVar
        $ \s ->
            s{ eventStateMousePos = V2 (clientX - cvsX) (clientY - cvsY) }
    liftIO $ print s

    let
      btns = mouseEventButtonsToButtons btnNum
    ev <-
      newEvent
        $ EventMouseMotion
        $ MouseMotionEvent
            btns
            (eventStateMousePos s)
            (V2 relX relY)
    void $ withSlot qVar (++ [ev])
    -- trigger waitEvent
    liftIO $ putMVar mVar ()


getJSEventsAPI
  :: MonadJSM m
  => MutantInstance 'BackendJS
  -> m (EventsAPI 'BackendJS m)
getJSEventsAPI (JSInstance c) = do
  -- Keep a mutable var for event state
  sVar <- newSlot emptyEventState
  -- Keep a mutable var for our event queue
  qVar <- newSlot []
  -- Keep an MVar for syncing waitEvent
  mVar <- liftIO newEmptyMVar
  document <- liftJSM $ jsg "document"
  mousemove <- mouseMoveHandler c sVar qVar mVar
  void $ liftJSM $ document ^. js3 "addEventListener" "mousemove" mousemove False
  let
    getMayEvent =
      stateSlot qVar $ \case
        [] -> (Nothing, [])
        ev:evs -> (Just ev, evs)
    resetWait = void $ liftIO $ tryTakeMVar mVar
    pollEvents =
      readSlot qVar
      <* writeSlot qVar []
    waitEvent t = do
      -- check to see if there are already evs in the queue
      mev <- getMayEvent >>= \case
        Just ev -> do
          return $ Just ev
        -- if not, wait until there are, or there's a timeout
        Nothing -> do
          liftIO $ do
            let evTrigger = takeMVar mVar
                timeTrigger = threadDelay $ t * 1000
            race_ evTrigger timeTrigger
          getMayEvent
      -- reset the mvar if needed
      resetWait
      return mev
    beginTextInput = liftIO . putStrLn . ("beginTextInput: " ++) . show
    endTextInput = liftIO $ putStrLn "endTextInput"
  return
    EventsAPI{.. }


runJS
  :: String
  -> Int
  -> JSM ()
  -> IO ()
runJS assetsPrefix port ma = do
#ifndef ghcjs_HOST_OS
  putStrLn $ concat ["running at http://localhost:", show port]
  app <-
    jsaddleWithAppOr defaultConnectionOptions ma
      $ staticApp
      $ defaultWebAppSettings assetsPrefix
  run port app
#else
  app
#endif


getJSAnimationFrameAPI
  :: MutantInstance 'BackendJS
  -> JSM (AnimationFrameAPI 'BackendJS JSM)
getJSAnimationFrameAPI _ =
  return AnimationFrameAPI
    { requestAnimation = nextAnimationFrame . const }
