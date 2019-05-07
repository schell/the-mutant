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
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Mutant.Interpreters.JSaddle.Render2d where

import           Control.Lens                           ((^.))
import           Control.Monad                          (void)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Language.Javascript.JSaddle            (Function, JSM, JSVal,
                                                         MonadJSM,
                                                         fromJSValUnchecked,
                                                         function, js, js0, js1,
                                                         js2, js4, jsf, jsg,
                                                         jss, liftJSM, toJSVal)
import           Language.Javascript.JSaddle.WebSockets (jsaddleWithAppOr)
import           Linear                                 (V2 (..), V4 (..))
import           Network.Wai.Application.Static         (defaultWebAppSettings,
                                                         staticApp)
import           Network.Wai.Handler.Warp               (run)
import           Network.WebSockets.Connection          (defaultConnectionOptions)

import           Mutant.Eff.Mutates                     (Mutates (..),
                                                         mutatesTVar)
import           Mutant.Eff.Render2d


type JSCanvas = Canvas JSVal JSVal String


type JSRender2d = Render2d 'Render2dJS


data instance Texture 'Render2dJS = JSTexture JSVal


getNewJSCanvas :: MonadJSM m => V2 Int -> String -> m JSCanvas
getNewJSCanvas (V2 w h) assetPrefix = liftJSM $ do
  doc  <- jsg "document"
  cvs  <- doc ^. js1 "createElement" "canvas"
  void $ cvs ^. jss "width" w
  void $ cvs ^. jss "height" h
  ctx  <- cvs ^. js1 "getContext" "2d"
  void $ ctx ^. jss "specialId" "root"
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


initiateTexture :: JSVal -> JSVal -> JSVal -> JSM Function
initiateTexture cvs img tex = do
  zero <- toJSVal (0 :: Int)
  rec cb <-
         function $ \_ _ _ -> do
           width <- img ^. js "naturalWidth"
           height <- img ^. js "naturalHeight"
           liftIO $ putStrLn "Image is loaded!"
           w :: Int <- fromJSValUnchecked width
           h :: Int <- fromJSValUnchecked height
           liftIO $ print (w,h)
           void $ cvs ^. jss "width" w
           void $ cvs ^. jss "height" h
           let args = [zero, zero, width, height, zero, zero, width, height]
           void $ tex ^. jsf "drawImage" (img : args)
           void $ tex ^. jss "complete" True
           void $ img ^. js2 "removeEventListener" "load" cb
  return cb


renderer2dJSaddle
  :: MonadJSM m
  => Mutates m
  -> JSCanvas
  -> m (JSRender2d m)
renderer2dJSaddle Mutates{..} c = do
  s <- newSlot c
  return
    Render2d
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
    , fillRect = \(Rect (V2 x y) (V2 w h)) -> do
        canvas <- readSlot s
        liftJSM $ do
          void $ canvasCtx canvas ^. js4 "fillRect" x y w h
          specialId :: String <- fromJSValUnchecked =<< canvasCtx canvas ^. js "specialId"
          liftIO $ putStrLn $ "Fill Rect: " ++ specialId
    , fillTexture = \(JSTexture tex) source dest -> do
        canvas <- readSlot s
        liftJSM $ do
          let Rect (V2 sx sy) (V2 sw sh) = source
              Rect (V2 dx dy) (V2 dw dh) = dest
          cvs  <- tex ^. js "canvas"
          args <- traverse toJSVal [sx,sy,sw,sh,dx,dy,dw,dh]
          void $ canvasCtx canvas ^. jsf "drawImage" (cvs : args)
    , textureLoad = \fp -> do
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
          cb <- initiateTexture cvs img tex
          void $ img ^. js2 "addEventListener" "load" cb
          img ^. jss "src" imgPath
          return $ Right $ JSTexture tex
    , textureSize = \(JSTexture tex) -> do
        liftJSM $ do
          cvs <- tex ^. js "canvas"
          V2 <$> (cvs ^. js "width" >>= fromJSValUnchecked)
            <*> (cvs ^. js "height" >>= fromJSValUnchecked)
    , textureIsLoaded = \(JSTexture tex) -> do
        liftJSM
          $ tex ^. js "complete" >>= fromJSValUnchecked
    , withTexture = \(JSTexture tex) m -> do
        canvas <- readSlot s
        cvs <- liftJSM $ tex ^. js "canvas"
        let newCanvas = Canvas cvs tex (canvasExtra canvas)
        updateSlot s newCanvas
        a <- m
        updateSlot s canvas
        return a
    }


myApp :: JSM ()
myApp = do
  cvs <- getNewJSCanvas (V2 640 480) "http://localhost:8888/"
  doc <- jsg "document"
  bdy <- doc ^. js "body"
  void $ bdy ^. js1 "appendChild" (canvasWindow cvs)
  renders2d <- renderer2dJSaddle mutatesTVar cvs
  drawingStuff renders2d


renders2dTest :: IO ()
renders2dTest = do
  putStrLn "running at http://localhost:8888"
  app <-
    jsaddleWithAppOr defaultConnectionOptions myApp
      $ staticApp
      $ defaultWebAppSettings "assets/"
  run 8888 app
