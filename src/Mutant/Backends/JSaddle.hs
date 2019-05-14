{-# LANGUAGE AllowAmbiguousTypes   #-}
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
module Mutant.Backends.JSaddle where

import           Control.Lens                           ((^.))
import           Control.Monad                          (void)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Language.Javascript.JSaddle            (Function,
                                                         JSException (..), JSM,
                                                         JSVal, MonadJSM, catch,
                                                         fromJSValUnchecked,
                                                         fun, function, js, js0,
                                                         js1, js2, js4, jsf,
                                                         jsg, jss, liftJSM, new,
                                                         toJSVal, valToText)
import           Language.Javascript.JSaddle.WebSockets (jsaddleWithAppOr)
import           Linear                                 (V2 (..), V4 (..))
import           Network.Wai.Application.Static         (defaultWebAppSettings,
                                                         staticApp)
import           Network.Wai.Handler.Warp               (run)
import           Network.WebSockets.Connection          (defaultConnectionOptions)

import           Mutant.API.Render2d
import           Mutant.Backend
import           Mutant.Geom
import           Mutant.Slot


type JSCanvas = Canvas JSVal JSVal String


type JSRender2d = Render2dAPI 'BackendJS


data instance Texture 'BackendJS = JSTexture JSVal
data instance Font 'BackendJS = JSFont String


catchAndPrint :: JSM () -> JSM ()
catchAndPrint f =
  void $ f `catch` \(JSException e) -> valToText e >>= liftIO . print


getNewJSCanvas :: MonadJSM m => V2 Int -> String -> m JSCanvas
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
         , canvasExtra = assetPrefix
         }


getDims :: MonadJSM m => JSCanvas -> m (V2 Int)
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


renderer2dJSaddle
  :: MonadJSM m
  => JSCanvas
  -> m (JSRender2d m)
renderer2dJSaddle c = do
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

    , texture = \fp -> do
        canvas <- readSlot s
        liftJSM $ do
          let imgPath = canvasExtra canvas ++ fp
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

    , withTexture = \(JSTexture tex) m -> do
        canvas <- readSlot s
        cvs <- liftJSM $ tex ^. js "canvas"
        let newCanvas = Canvas cvs tex (canvasExtra canvas)
        writeSlot s newCanvas
        a <- m
        writeSlot s canvas
        return a

    , font = \file -> liftJSM $ do
        fvar <- newSlot LoadStatusLoading
        n <- withSlot k succ
        canvas <- readSlot s
        let url = canvasExtra canvas ++ file
            nam = "font" ++ show n
        initiateFont fvar url nam
        return fvar
    }


myApp :: JSM ()
myApp = do
  cvs <- getNewJSCanvas (V2 640 480) "http://localhost:8888/"
  doc <- jsg "document"
  bdy <- doc ^. js "body"
  void $ bdy ^. js1 "appendChild" (canvasWindow cvs)
  renders2d <- renderer2dJSaddle cvs
  drawingStuff renders2d


renders2dTest :: IO ()
renders2dTest = do
  putStrLn "running at http://localhost:8888"
  app <-
    jsaddleWithAppOr defaultConnectionOptions myApp
      $ staticApp
      $ defaultWebAppSettings "assets/"
  run 8888 app
