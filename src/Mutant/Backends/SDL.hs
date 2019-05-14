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
module Mutant.Backends.SDL where

import           Codec.Picture          (convertRGBA8, imageData, imageHeight,
                                         imageWidth, readImage)
import           Control.Arrow          ((&&&))
import           Control.Monad.Except   (ExceptT, runExceptT)
import           Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Text              as T
import           Data.Traversable       (for)
import           Data.Vector.Storable   (thaw)
import           Linear                 (V2 (..), V4 (..))
import           SDL                    (Point (..), Rectangle (..), Renderer,
                                         Window, ($=))
import qualified SDL
import           System.Directory       (doesFileExist)
import           Typograffiti.SDL       as Typo

import           Mutant.API.Events
import           Mutant.API.Render2d
import           Mutant.Backend
import           Mutant.Geom
import           Mutant.Slot


type SDLCanvas = Canvas Window Renderer String


type SDLRender2dAPI = Render2dAPI 'BackendSDL


type SDLEventsAPI = EventsAPI 'BackendSDL


data instance Texture 'BackendSDL = SDLTexture (Slot SDL.Texture)


data instance Font 'BackendSDL = SDLFont FilePath


data instance MutantInstance 'BackendSDL = SDLInstance Window


getSDLInstance
  :: MonadIO m
  => String
  -- ^ Window title
  -> V2 Int
  -- ^ Window Size
  -> m (MutantInstance 'BackendSDL)
getSDLInstance title size = do
  SDL.initialize $ Just SDL.InitVideo
  let wcfg = SDL.defaultWindow
         { SDL.windowInitialSize = fromIntegral <$> size }
  SDLInstance
    <$> SDL.createWindow (T.pack title) wcfg


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


getSDLRender2dAPI
  :: forall m
   . MonadIO m
  => MutantInstance 'BackendSDL
  -- ^ The mutant instance. See getSDLInstance.
  -> String
  -> m (SDLRender2dAPI m)
getSDLRender2dAPI (SDLInstance win) pfx = do
  let rcfg = SDL.defaultRenderer
               { SDL.rendererType = SDL.AcceleratedVSyncRenderer }
  r <- SDL.createRenderer win (-1) rcfg
  fstore <- runOrFail $ Typo.newDefaultFontStore r
  return
    Render2dAPI
    { clear = do
        SDL.clear r
        SDL.rendererDrawColor r $= V4 0 0 0 0
        SDL.fillRect r Nothing
    , present = SDL.present r
    , getDimensions =
        fmap fromIntegral
          <$> SDL.glGetDrawableSize win
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

    , fillTexture = \(SDLTexture s) source dest -> do
        tex <- readSlot s
        SDL.copy
          r
          tex
          (Just $ rect2Rectangle source)
          (Just $ rect2Rectangle dest)

    , fillText = \(SDLFont file) (V2 w h) pos str -> runOrFail $ do
        clr <- SDL.get $ SDL.rendererDrawColor r
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
          return tex
        newSlot
          =<< either
                (return . LoadStatusFailure)
                (fmap (LoadStatusSuccess . SDLTexture) . newSlot)
                e

    , textureSize = \(SDLTexture s) -> do
        info <- SDL.queryTexture =<< readSlot s
        return
          $ fromIntegral
              <$> V2 (SDL.textureWidth info) (SDL.textureHeight info)

    , withTexture = \(SDLTexture s) m -> do
        -- read the tex and get its size
        tex <- readSlot s
        size <-
          fmap fromIntegral
          . uncurry V2
          . (SDL.textureWidth &&& SDL.textureHeight)
          <$> SDL.queryTexture tex
        -- make a texture to draw into
        ttex <- SDL.createTexture r SDL.RGBA8888 SDL.TextureAccessTarget size
        SDL.textureBlendMode ttex $= SDL.BlendAlphaBlend
        -- then set that as the render target
        prev <- SDL.get $ SDL.rendererRenderTarget r
        SDL.rendererRenderTarget r $= Just ttex
        -- draw the original texture into it
        let source = rect2Rectangle $ Rect 0 size
        SDL.copy r tex (Just source) (Just source)
        -- do the rest of the drawing
        a <- m
        -- destroy the old texture
        SDL.destroyTexture tex
        -- unbind the render target
        SDL.rendererRenderTarget r $= prev
        -- update the input texture slot
        writeSlot s ttex
        return a

    , font = \file -> liftIO (doesFileExist (pfx ++ file)) >>= newSlot . \case
        True -> LoadStatusSuccess $ SDLFont file
        False ->
          LoadStatusFailure
            $ unwords
              [ show file
              , "does not exist."
              ]
    }


renders2dTest :: IO ()
renders2dTest = do
  i <- getSDLInstance "SDL Render2d Test" (V2 640 480)
  r <- getSDLRender2dAPI i "assets/"
  drawingStuff r
