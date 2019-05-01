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
import           Control.Monad.IO.Class           (MonadIO (..))
import           Data.Foldable                    (for_)
import           Data.Function                    (fix)
import           GHC.Conc                         (threadDelay)
import           Language.Javascript.JSaddle      (JSM, JSVal,
                                                   fromJSValUnchecked, js, js0,
                                                   js1, js2, js4, jsg)
import           Language.Javascript.JSaddle.Warp (run)
import           Linear                           (V2 (..))
import           Polysemy                         hiding (run)
import           Polysemy.IO                      (runIO)

import           Mutant.Eff.Renders2d


data Canvas
  = Canvas
  { canvasVal :: JSVal
  , canvasCtx :: JSVal
  }


getNewCanvas :: JSM Canvas
getNewCanvas = do
  doc <- jsg "document"
  cvs <- doc ^. js1 "createElement" "canvas"
  ctx <- cvs ^. js1 "getContext" "2d"
  return $ Canvas cvs ctx


getDims :: Canvas -> JSM (V2 Int)
getDims canvas = do
  w <- canvasVal canvas ^. js "width"
  h <- canvasVal canvas ^. js "height"
  V2
    <$> fromJSValUnchecked w
    <*> fromJSValUnchecked h


runRenders2dInJSaddle
  :: Member (Lift JSM) r
  => Canvas
  -> Sem (Renders2d ': r) a
  -> Sem r a
runRenders2dInJSaddle canvas@(Canvas _ ctx) = interpret $ \case
  Clear -> sendM $ do
    V2 w h <- getDims canvas
    void $ canvasCtx canvas ^. js4 "clearRect" (0 :: Int) (0 :: Int) w h
  Present -> return ()
  GetDimensions -> sendM $ getDims canvas
  DrawLine (V2 x1 y1) (V2 x2 y2) -> sendM $ do
    void $ ctx ^. js0 "beginPath"
    void $ ctx ^. js2 "moveTo" x1 y1
    void $ ctx ^. js2 "lineTo" x2 y2
    void $ ctx ^. js0 "stroke"
  DrawRect (Rect (V2 x y) (V2 w h)) -> sendM $
    void $ ctx ^. js4 "fillRect" x y w h


drawingStuff
  :: ( Member Renders2d r
     , Member (Lift IO) r
     )
  => Sem r ()
drawingStuff = do
  V2 w h <- getDimensions
  let tl = 10
      tr = V2 (w - 10) 10
      br = V2 (w - 10) (h - 10)
      bl = V2 10 (h - 10)
      points = [tl, tr, br, bl]
      lnes = zip points $ drop 1 points ++ [tl]
  fix $ \loop -> do
    for_ lnes $ \line -> do
      clear
      uncurry drawLine line
      liftIO $ threadDelay 500000
    clear
    drawRect $ Rect tl (br - tl)
    liftIO $ threadDelay 500000
    clear
    liftIO $ threadDelay 500000
    loop


myApp :: JSM ()
myApp = do
  cvs <- getNewCanvas
  doc <- jsg "document"
  bdy <- doc ^. js "body"
  void $ bdy ^. js1 "appendChild" (canvasVal cvs)
  runM
    $ runIO @JSM
    $ runRenders2dInJSaddle cvs drawingStuff


renders2dTest :: IO ()
renders2dTest = do
  putStrLn "running at http://localhost:8888"
  run 8888 myApp
