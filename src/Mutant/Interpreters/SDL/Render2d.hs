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
import           Control.Monad.Except   (ExceptT, runExceptT)
import           Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Text              as T
import           Data.Traversable       (for)
import           Data.Vector.Storable   (thaw)
import           Linear                 (V2 (..), V4 (..))
import           SDL                    (Point (..), Rectangle (..), Renderer,
                                         Window, ($=))
import qualified SDL
import           Typograffiti.SDL       as Typo

import           Mutant.API.Render2d
import           Mutant.Slot



type SDLCanvas = Canvas Window Renderer String


type SDLRender2d = Render2d 'Render2dSDL


data instance Texture 'Render2dSDL = SDLTexture SDL.Texture
data instance Font 'Render2dSDL = SDLFont FilePath


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


runOrFail
  :: Monad m
  => ExceptT String m b
  -> m b
runOrFail f = runExceptT f >>= either fail return


renders2dSDL
  :: forall m
   . MonadIO m
  => SDLCanvas
  -> m (SDLRender2d m)
renders2dSDL canvas@(Canvas _ r pfx) = do
  liftIO $ putStrLn "Creating the font store"
  fstore <- runOrFail $ Typo.newDefaultFontStore r
  liftIO $ putStrLn "Created the font store"

  return
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

    , fillText = \(SDLFont file) (V2 w h) clr pos str -> runOrFail $ do
        let v4clr = (\wrd -> fromIntegral wrd / (255.0 :: Float)) <$> clr
        RenderedGlyphs draw _ <-
            getTextRendering
              r
              fstore
              (pfx ++ file)
              (GlyphSizeInPixels w h)
              str
        draw [moveV2 $ fromIntegral <$> pos, colorV4 v4clr]

    , texture = \path -> do
        let file = pfx ++ path
        eDynImg <- liftIO $ readImage file
        e <- for eDynImg $ \img -> do
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
          return ttex
        newSlot
          $ either
              LoadStatusFailure
              (LoadStatusSuccess . SDLTexture)
              e

    , textureSize = \(SDLTexture tex) -> do
        info <- SDL.queryTexture tex
        return
          $ fromIntegral
              <$> V2 (SDL.textureWidth info) (SDL.textureHeight info)

    , withTexture = \(SDLTexture tex) m -> do
        prev <-
          SDL.get
          $ SDL.rendererRenderTarget r
        SDL.rendererRenderTarget r $= Just tex
        a <- m
        SDL.rendererRenderTarget r $= prev
        return a

    -- TODO: Do a filesystem check to see if fonts exist before returning success
    , font = \file -> newSlot (LoadStatusSuccess $ SDLFont file)
    }


renders2dTest :: IO ()
renders2dTest = do
  canvas <- getNewCanvas "SDL Render2d Test" (V2 640 480) "assets/"
  drawingStuff =<< renders2dSDL canvas
