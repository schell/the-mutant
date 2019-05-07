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
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin         #-}
module Mutant.Interpreters.JSaddle.Renders2d where

import           Control.Lens                           ((^.))
import           Control.Monad                          (void)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Language.Javascript.JSaddle            (Function, JSM, JSVal,
                                                         fromJSValUnchecked,
                                                         function, js, js0, js1,
                                                         js2, js4, jsf, jsg,
                                                         jss, toJSVal)
import           Language.Javascript.JSaddle.WebSockets (jsaddleWithAppOr)
import           Linear                                 (V2 (..), V4 (..))
import           Network.Wai.Application.Static         (defaultWebAppSettings,
                                                         staticApp)
import           Network.Wai.Handler.Warp               (run)
import           Network.WebSockets.Connection          (defaultConnectionOptions)
import           Polysemy                               hiding (run)
import           Polysemy.IO                            (runIO)

import           Mutant.Eff.Renders2d


type JSCanvas = Canvas JSVal JSVal String


type JSRenders2d = Renders2d 'Renders2dJSaddle


data instance Texture 'Renders2dJSaddle = JSTexture JSVal


getNewJSCanvas :: V2 Int -> String -> JSM JSCanvas
getNewJSCanvas (V2 w h) assetPrefix = do
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


getDims :: JSCanvas -> JSM (V2 Int)
getDims canvas = do
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


runRenders2dInJSaddle
  :: Member (Lift JSM) r
  => JSCanvas
  -> Sem (JSRenders2d ': r) a
  -> Sem r a
runRenders2dInJSaddle canvas s = do
  ctxName :: String <-
    sendM
      (fromJSValUnchecked =<< canvasCtx canvas ^. js "specialId")
  sendM $ liftIO $ putStrLn $ "ctx: " ++ ctxName
  runRenders canvas s


runRenders
  :: Member (Lift JSM) r
  => JSCanvas
  -> Sem (JSRenders2d ': r) a
  -> Sem r a
runRenders canvas = interpretH $ \case
  Clear -> do
    sendM $ do
      V2 w h <- getDims canvas
      void $ canvasCtx canvas ^. js4 "clearRect" (0 :: Int) (0 :: Int) w h
    pureT ()
  Present -> pureT ()
  GetDimensions ->
    sendM (getDims canvas)
      >>= pureT
  SetDrawColor c -> do
    sendM $ do
      let color = toCSSColor c
      void $ canvasCtx canvas ^. jss "fillStyle" color
      void $ canvasCtx canvas ^. jss "strokeStyle" color
    pureT ()
  StrokeLine (Line (V2 x1 y1) (V2 x2 y2)) -> do
    sendM $ do
      void $ canvasCtx canvas ^. js0 "beginPath"
      void $ canvasCtx canvas ^. js2 "moveTo" x1 y1
      void $ canvasCtx canvas ^. js2 "lineTo" x2 y2
      void $ canvasCtx canvas ^. js0 "stroke"
    pureT ()
  StrokeRect (Rect (V2 x y) (V2 w h)) -> do
    sendM
      $ void
      $ canvasCtx canvas ^. js4 "strokeRect" x y w h
    pureT ()
  FillRect (Rect (V2 x y) (V2 w h)) -> do
    sendM
      $ void
      $ canvasCtx canvas ^. js4 "fillRect" x y w h
    sendM $ do
      specialId :: String <- fromJSValUnchecked =<< canvasCtx canvas ^. js "specialId"
      liftIO $ putStrLn $ "Fill Rect: " ++ specialId
    pureT ()
  FillTexture (JSTexture tex) source dest -> do
    let Rect (V2 sx sy) (V2 sw sh) = source
        Rect (V2 dx dy) (V2 dw dh) = dest
    sendM $ do
      cvs  <- tex ^. js "canvas"
      args <- traverse toJSVal [sx,sy,sw,sh,dx,dy,dw,dh]
      void $ canvasCtx canvas ^. jsf "drawImage" (cvs : args)
    pureT ()
  TextureLoad fp -> do
    et <- sendM $ do
      let imgPath = canvasExtra canvas ++ fp
      doc <- jsg "document"
      img <- doc ^. js1 "createElement" "img"
      cvs <- doc ^. js1 "createElement" "canvas"
      bdy <- doc ^. js "body"
      void $ bdy ^. js1 "appendChild" cvs
      tex <- cvs ^. js1 "getContext" "2d"
      void $ tex ^. jss "complete" False
      void $ tex ^. jss "specialId" fp
      void $ cvs ^. jss "width" (0 :: Int)
      void $ cvs ^. jss "height" (0 :: Int)
      cb <- initiateTexture cvs img tex
      void $ img ^. js2 "addEventListener" "load" cb
      img ^. jss "src" imgPath
      return $ Right $ JSTexture tex
    pureT et
  TextureSize (JSTexture tex) -> do
    sz <- sendM $ do
      cvs <- tex ^. js "canvas"
      V2 <$> (cvs ^. js "width" >>= fromJSValUnchecked)
         <*> (cvs ^. js "height" >>= fromJSValUnchecked)
    pureT sz
  TextureIsLoaded (JSTexture tex) -> do
    b <-
      sendM
      $ tex ^. js "complete" >>= fromJSValUnchecked
    pureT b
  WithTexture (JSTexture tex) m -> do
    sendM $ liftIO $ putStrLn "withTexture"
    m1 <- runT m
    cvs <- sendM $ tex ^. js "canvas"
    specialId <- sendM $ fromJSValUnchecked =<< tex ^. js "specialId"
    sendM $ liftIO $ putStrLn specialId
    sendM $ liftIO $ putStrLn "running withTexture"
    let newCanvas = Canvas cvs tex (canvasExtra canvas)
    raise
      $ runRenders2dInJSaddle newCanvas m1


myApp :: JSM ()
myApp = do
  cvs <- getNewJSCanvas (V2 640 480) "http://localhost:8888/"
  doc <- jsg "document"
  bdy <- doc ^. js "body"
  void $ bdy ^. js1 "appendChild" (canvasWindow cvs)
  runM
    $ runIO @JSM
    $ runRenders2dInJSaddle cvs drawingStuff


renders2dTest :: IO ()
renders2dTest = do
  putStrLn "running at http://localhost:8888"
  app <-
    jsaddleWithAppOr defaultConnectionOptions myApp
      $ staticApp
      $ defaultWebAppSettings "assets/"
  run 8888 app
