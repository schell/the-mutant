{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
module Mutant.Eff.Render2d where

import           Control.Concurrent     (threadDelay)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Kind              (Type)
--import           Data.Vector.Storable (Vector)
import           Control.Monad          (unless)
import           Data.Foldable          (traverse_)
import           Data.Function          (fix)
import           Linear                 (V2 (..), V4 (..))


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


data Render2dBackend
  = Render2dJSaddle
  | Render2dSDL


data family Texture (i :: Render2dBackend)
data family Font (i :: Render2dBackend)


data Render2d (i :: Render2dBackend) (m :: Type -> Type)
  = Render2d
  { clear           :: m ()
  , present         :: m ()

  , getDimensions   :: m (V2 Int)
  , setDrawColor    :: V4 Int -> m ()

  , strokeLine      :: Line Int -> m ()
  , strokeRect      :: Rect Int -> m ()

  , fillRect        :: Rect Int -> m ()
  , fillTexture     :: Texture i -> Rect Int -> Rect Int -> m ()

  , textureLoad     :: String -> m (Either String (Texture i))
  , textureSize     :: Texture i -> m (V2 Int)
  , textureIsLoaded :: Texture i -> m Bool
  , withTexture     :: forall a. Texture i -> m a -> m a
  }


  --FontLoad :: String -> V2 Int -> Render2d i m (Font i)
  --FontIsLoaded :: Font i -> Render2d i m Bool
  -- TODO: Screenshot API


-- | Helps with writing interpreters.
data Canvas window ctx a
  = Canvas
  { canvasWindow :: window
  , canvasCtx    :: ctx
  , canvasExtra  :: a
  }


testTextureSize
  :: Monad m
  => Render2d i m
  -> FilePath
  -> m Bool
testTextureSize Render2d{..} fp = do
  tex <-
    textureLoad fp
      >>= either
            fail
            return
  fix $ \loop -> do
    loaded <- textureIsLoaded tex
    unless loaded loop
  tsz <- textureSize tex
  dim <- withTexture tex getDimensions
  return $ tsz == dim



-- | This is a test.
drawingStuff
  :: MonadIO m
  => Render2d i m
  -> m ()
drawingStuff Render2d{..} = do
  wh@(V2 w h) <- getDimensions
  liftIO $ putStrLn $ "Context has dimensions " ++ show wh
  let tl = 10
      tr = V2 (w - 10) 10
      br = V2 (w - 10) (h - 10)
      bl = V2 10 (h - 10)
      lns = [Line tl br, Line tr bl]
      black = V4 0 0 0 255
      white = V4 255 255 255 255
      cyan = V4 0 255 255 255
      canary = V4 255 255 0 255
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
    textureLoad "sot.png"
      >>= either
            fail
            return
  -- loop until it's actually loaded, `textureLoad` is synchronous on sdl and
  -- async in javascript
  fix $ \loop -> do
    loaded <- textureIsLoaded tex
    unless loaded loop
  tsz@(V2 _ th) <- textureSize tex
  liftIO $ putStrLn $ "Texture size is " ++ show tsz
  -- draw the texture to the screen
  let posf :: V2 Float
      posf = (fromIntegral <$> wh)/2.0 - (fromIntegral <$> tsz)/2.0
      pos = floor <$> posf
  fillTexture
    tex
    (Rect 0 tsz)
    (Rect pos tsz)
  -- do some higher-order drawing into the texture itself
  withTexture tex $ do
    texDims <- getDimensions
    liftIO $ putStrLn $ "textures dimensions are:" ++ show texDims
    setDrawColor canary
    fillRect (Rect 0 25)
  fillTexture
    tex
    (Rect 0 tsz)
    (Rect (pos + V2 0 th) tsz)
  -- present it
  fix $ \loop -> do
    present
    liftIO $ threadDelay 1000000
    loop
