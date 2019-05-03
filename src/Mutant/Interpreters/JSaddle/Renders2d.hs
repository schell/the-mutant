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
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin         #-}
module Mutant.Interpreters.JSaddle.Renders2d where

import           Control.Lens                     ((^.))
import           Control.Monad                    (void)
import           Language.Javascript.JSaddle      (JSM, JSVal,
                                                   fromJSValUnchecked, js, js0,
                                                   js1, js2, js4, jsf, jsg, jss,
                                                   toJSVal)
import           Language.Javascript.JSaddle.Warp (run)
import           Linear                           (V2 (..), V4 (..))
import           Polysemy                         hiding (run)
import           Polysemy.IO                      (runIO)

import           Mutant.Eff.Renders2d


type JSCanvas = Canvas JSVal JSVal String


type JSRenders2d = Renders2d 'Renders2dJSaddle


data instance Texture 'Renders2dJSaddle = JSTexture JSVal


getNewJSCanvas :: String -> JSM JSCanvas
getNewJSCanvas assetPrefix = do
  doc  <- jsg "document"
  cvs  <- doc ^. js1 "createElement" "canvas"
  ctx  <- cvs ^. js1 "getContext" "2d"
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


runRenders2dInJSaddle
  :: Member (Lift JSM) r
  => JSCanvas
  -> Sem (JSRenders2d ': r) a
  -> Sem r a
runRenders2dInJSaddle canvas@(Canvas _ ctx pfx) = interpret $ \case
  Clear -> sendM $ do
    V2 w h <- getDims canvas
    void $ canvasCtx canvas ^. js4 "clearRect" (0 :: Int) (0 :: Int) w h
  Present -> return ()
  GetDimensions -> sendM $ getDims canvas
  SetDrawColor c -> sendM $ do
    let color = toCSSColor c
    void $ ctx ^. jss "fillStyle" color
    void $ ctx ^. jss "strokeStyle" color
  StrokeLine (Line (V2 x1 y1) (V2 x2 y2)) -> sendM $ do
    void $ ctx ^. js0 "beginPath"
    void $ ctx ^. js2 "moveTo" x1 y1
    void $ ctx ^. js2 "lineTo" x2 y2
    void $ ctx ^. js0 "stroke"
  StrokeRect (Rect (V2 x y) (V2 w h)) -> sendM $
    void $ ctx ^. js4 "strokeRect" x y w h
  FillRect (Rect (V2 x y) (V2 w h)) -> sendM $
    void $ ctx ^. js4 "fillRect" x y w h
  FillTexture (JSTexture img) source dest -> do
    let Rect (V2 sx sy) (V2 sw sh) = source
        Rect (V2 dx dy) (V2 dw dh) = dest
    sendM $ do
      args <- traverse toJSVal [sx,sy,sw,sh,dx,dy,dw,dh]
      void $ ctx ^. jsf "drawImage" (img : args)
  TextureLoad fp -> sendM $ do
    doc <- jsg "document"
    img <- doc ^. js1 "createElement" "img"
    img ^. jss "src" (pfx ++ fp)
    bdy <- doc ^. js "body"
    void $ bdy ^. js1 "appendChild" img
    return $ Right $ JSTexture img
  TextureSize (JSTexture img) ->
    sendM
      $ V2 <$> (img ^. js "naturalWidth" >>= fromJSValUnchecked)
           <*> (img ^. js "naturalHeight" >>= fromJSValUnchecked)
  TextureIsLoaded (JSTexture img) ->
    sendM
      $ img ^. js "complete" >>= fromJSValUnchecked


myApp :: JSM ()
myApp = do
  cvs <- getNewJSCanvas
  doc <- jsg "document"
  bdy <- doc ^. js "body"
  void $ bdy ^. js1 "appendChild" (canvasWindow cvs)
  runM
    $ runIO @JSM
    $ runRenders2dInJSaddle cvs drawingStuff


renders2dTest :: IO ()
renders2dTest = do
  putStrLn "running at http://localhost:8888"
  run 8888 myApp
