{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Mutant.Eff.Renders2d where

import           Control.Monad.IO.Class (liftIO)
import           Data.Kind              (Type)
--import           Data.Vector.Storable (Vector)
import           Control.Monad          (unless)
import           Data.Foldable          (traverse_)
import           Data.Function          (fix)
import           Linear                 (V2 (..), V4 (..))
import           Polysemy               hiding (run)


data Rect a
  = Rect
  { rectUpperLeft :: V2 a
  , rectExtents   :: V2 a
  }


insetRect
  :: Num a
  => Rect a
  -> V2 a
  -> Rect a
insetRect r v =
  Rect
  { rectUpperLeft = rectUpperLeft r + v
  , rectExtents = rectExtents r - 2 * v
  }


data Line a
  = Line
  { lineStart :: V2 a
  , lineEnd   :: V2 a
  }


data Renders2dBackend
  = Renders2dJSaddle
  | Renders2dSDL


data family Texture (i :: Renders2dBackend)


data Renders2d (i :: Renders2dBackend) (m :: Type -> Type) a where
  Clear   :: Renders2d i m ()
  Present :: Renders2d i m ()

  GetDimensions :: Renders2d i m (V2 Int)
  SetDrawColor  :: V4 Int -> Renders2d i m ()

  StrokeLine :: Line Int -> Renders2d i m ()
  StrokeRect :: Rect Int -> Renders2d i m ()

  FillRect :: Rect Int -> Renders2d i m ()
  FillTexture :: Texture i -> Rect Int -> Rect Int -> Renders2d i m ()

  TextureLoad     :: String -> Renders2d i m (Either String (Texture i))
  TextureSize     :: Texture i -> Renders2d i m (V2 Int)
  TextureIsLoaded :: Texture i -> Renders2d i m Bool
makeSem ''Renders2d


-- | Helps with writing interpreters.
data Canvas window ctx a
  = Canvas
  { canvasWindow :: window
  , canvasCtx    :: ctx
  , canvasExtra  :: a
  }


-- | This is a test.
drawingStuff
  :: ( Member (Renders2d i) r
     , Member (Lift IO) r
     )
  => Sem r ()
drawingStuff = do
  wh@(V2 w h) <- getDimensions
  let tl = 10
      tr = V2 (w - 10) 10
      br = V2 (w - 10) (h - 10)
      bl = V2 10 (h - 10)
      lns = [Line tl br, Line tr bl]
      black = V4 0 0 0 255
      white = V4 255 255 255 255
      cyan = V4 0 255 255 255
  clear
  -- first draw the screen black
  setDrawColor black
  fillRect
    $ Rect 0 wh
  -- draw a white frame inset by 10 pixels
  setDrawColor white
  strokeRect
    $ insetRect (Rect 0 wh) 10
  -- then draw a cyan X
  setDrawColor cyan
  traverse_ strokeLine lns
  -- load an image
  tex <-
    textureLoad "assets/sot.png"
      >>= either
            fail
            return
  -- loop until it's actually loaded, `textureLoad` is synchronous on sdl and
  -- async in javascript
  fix $ \loop -> do
    loaded <- textureIsLoaded tex
    unless loaded loop
  tsz <- textureSize tex
  liftIO $ print tsz
  -- draw the texture to the screen
  let pos :: V2 Float = (fromIntegral <$> wh)/2.0 - (fromIntegral <$> tsz)/2.0
  fillTexture
    tex
    (Rect 0 tsz)
    (Rect (floor <$> pos) tsz)
  -- present it
  present
