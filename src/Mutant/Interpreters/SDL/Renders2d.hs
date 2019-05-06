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
module Mutant.Interpreters.SDL.Renders2d where

import           Codec.Picture          (convertRGBA8, imageData, imageHeight,
                                         imageWidth, readImage)
import           Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Text              as T
import           Data.Traversable       (for)
import           Data.Vector.Storable   (thaw)
import           Linear                 (V2 (..), V4 (..))
import           Polysemy               hiding (run)
import           SDL                    (Point (..), Rectangle (..), Renderer,
                                         Window, ($=))
import qualified SDL

import           Mutant.Eff.Renders2d


type SDLCanvas = Canvas Window Renderer String


type SDLRenders2d = Renders2d 'Renders2dSDL


data instance Texture 'Renders2dSDL = SDLTexture SDL.Texture


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


runRenders2dInSDL
  :: Member (Lift IO) r
  => SDLCanvas
  -> Sem (SDLRenders2d ': r) a
  -> Sem r a
runRenders2dInSDL canvas@(Canvas _ r pfx) = interpret $ \case
  Clear -> do
    SDL.clear r
    SDL.rendererDrawColor r $= V4 0 0 0 0
    SDL.fillRect r Nothing
  Present -> SDL.present r
  GetDimensions -> sendM $ getDims canvas
  SetDrawColor c -> SDL.rendererDrawColor r $= (fromIntegral <$> c)
  StrokeLine (Line start end) ->
    SDL.drawLine r
      (P $ fromIntegral <$> start)
      (P $ fromIntegral <$> end)
  StrokeRect rect -> SDL.drawRect r (Just $ rect2Rectangle rect)
  FillRect rect -> SDL.fillRect r (Just $ rect2Rectangle rect)
  FillTexture (SDLTexture tex) source dest -> do
    liftIO $ putStrLn "Filling texture"
    SDL.copy
      r
      tex
      (Just $ rect2Rectangle source)
      (Just $ rect2Rectangle dest)
    liftIO $ putStrLn "Filled texture"
  TextureLoad path -> do
    let file = pfx ++ path
    eDynImg <- sendM $ readImage file
    for eDynImg $ \img -> do
      let rgba8Img = convertRGBA8 img
          size = fromIntegral <$> V2 (imageWidth rgba8Img) (imageHeight rgba8Img)
          pitch = fromIntegral $ imageWidth rgba8Img * 4
          bytes = imageData rgba8Img
      iovec <- sendM $ thaw bytes
      -- i'm not sure why the pixel format is flipped here, but SDL.RGBA8888
      -- doesn't give us the correct colors.
      surface <- SDL.createRGBSurfaceFrom iovec size pitch SDL.ABGR8888
      tex <- SDL.createTextureFromSurface r surface
      SDL.freeSurface surface
      SDL.textureBlendMode tex $= SDL.BlendAlphaBlend
      return $ SDLTexture tex
  TextureSize (SDLTexture tex) -> do
    info <- SDL.queryTexture tex
    return
      $ fromIntegral
      <$> V2 (SDL.textureWidth info) (SDL.textureHeight info)
  TextureIsLoaded _ -> return True


renders2dTest :: IO ()
renders2dTest =
  getNewCanvas "SDL Renders2d Test" (V2 640 480) "assets/"
    >>= runM . (`runRenders2dInSDL` drawingStuff)
