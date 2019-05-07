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
module Mutant.Interpreters.SDL.Render2d where

import           Codec.Picture          (convertRGBA8, imageData, imageHeight,
                                         imageWidth, readImage)
import           Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Text              as T
import           Data.Traversable       (for)
import           Data.Vector.Storable   (thaw)
import           Linear                 (V2 (..), V4 (..))
import           SDL                    (Point (..), Rectangle (..), Renderer,
                                         Window, ($=))
import qualified SDL

import           Mutant.Eff.Render2d


type SDLCanvas = Canvas Window Renderer String


type SDLRender2d = Render2d 'Render2dSDL


data instance Texture 'Render2dSDL = SDLTexture SDL.Texture


getNewCanvas
  :: String
  -- ^ Window title
  -> V2 Int
  -- ^ Window size
  -> String
  -- ^ Asset prefix
  -> IO SDLCanvas
getNewCanvas title size pfx = do
  let wcfg = SDL.defaultWindow
         { SDL.windowInitialSize = fromIntegral <$> size }
      rcfg = SDL.defaultRenderer
         { SDL.rendererType = SDL.AcceleratedVSyncRenderer }
  w :: Window <- SDL.createWindow (T.pack title) wcfg
  Canvas w
    <$> SDL.createRenderer w (-1) rcfg
    <*> pure pfx


getDims :: SDLCanvas -> IO (V2 Int)
getDims canvas =
  fmap fromIntegral
    <$> SDL.glGetDrawableSize (canvasWindow canvas)


rect2Rectangle
  :: Integral i
  => Integral o
  => Rect i
  -> Rectangle o
rect2Rectangle (Rect tl wh) =
  Rectangle
    (P $ fromIntegral <$> tl)
    (fromIntegral <$> wh)


renders2dSDL
  :: MonadIO m
  => SDLCanvas
  -> SDLRender2d m
renders2dSDL canvas@(Canvas _ r pfx) =
  Render2d
  { clear = do
      SDL.clear r
      SDL.rendererDrawColor r $= V4 0 0 0 0
      SDL.fillRect r Nothing
  , present = SDL.present r
  , getDimensions = liftIO $ getDims canvas
  , setDrawColor = \c ->
      SDL.rendererDrawColor r $= (fromIntegral <$> c)
  , strokeLine = \(Line start end) -> do
    SDL.drawLine r
      (P $ fromIntegral <$> start)
      (P $ fromIntegral <$> end)
  , strokeRect = \rect -> do
    SDL.drawRect r (Just $ rect2Rectangle rect)
  , fillRect = \rect -> do
    SDL.fillRect r (Just $ rect2Rectangle rect)
  , fillTexture = \(SDLTexture tex) source dest -> do
    SDL.copy
      r
      tex
      (Just $ rect2Rectangle source)
      (Just $ rect2Rectangle dest)
  , textureLoad = \path -> do
    let file = pfx ++ path
    eDynImg <- liftIO $ readImage file
    for eDynImg $ \img -> do
      let rgba8Img = convertRGBA8 img
          size = fromIntegral <$> V2 (imageWidth rgba8Img) (imageHeight rgba8Img)
          pitch = fromIntegral $ imageWidth rgba8Img * 4
          bytes = imageData rgba8Img
      iovec <- liftIO $ thaw bytes
      -- i'm not sure why the pixel format is flipped here, but SDL.RGBA8888
      -- doesn't give us the correct colors.
      surface <- SDL.createRGBSurfaceFrom iovec size pitch SDL.ABGR8888
      tex <- SDL.createTextureFromSurface r surface
      SDL.freeSurface surface
      SDL.textureBlendMode tex $= SDL.BlendAlphaBlend
      ttex <- SDL.createTexture r SDL.RGBA8888 SDL.TextureAccessTarget size
      prev <- SDL.get $ SDL.rendererRenderTarget r
      SDL.rendererRenderTarget r $= Just ttex
      let source = rect2Rectangle $ Rect 0 size
      SDL.copy r tex (Just source) (Just source)
      SDL.rendererRenderTarget r $= prev
      SDL.destroyTexture tex
      return $ SDLTexture ttex
  , textureSize = \(SDLTexture tex) -> do
    info <- SDL.queryTexture tex
    return
      $ fromIntegral
          <$> V2 (SDL.textureWidth info) (SDL.textureHeight info)
  , textureIsLoaded = \_ -> return True
  , withTexture = \(SDLTexture tex) m -> do
    prev <-
      SDL.get
      $ SDL.rendererRenderTarget r
    SDL.rendererRenderTarget r $= Just tex
    a <- m
    SDL.rendererRenderTarget r $= prev
    return a
  }


renders2dTest :: IO ()
renders2dTest = do
  canvas <- getNewCanvas "SDL Render2d Test" (V2 640 480) "assets/"
  drawingStuff $ renders2dSDL canvas
