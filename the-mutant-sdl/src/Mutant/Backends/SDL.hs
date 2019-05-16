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
module Mutant.Backends.SDL
  ( getSDLInstance
  , getSDLRender2dAPI
  , getSDLEventsAPI
  , getSDLAnimationFrameAPI
  ) where

import           Codec.Picture          (convertRGBA8, imageData, imageHeight,
                                         imageWidth, readImage)
import           Control.Arrow          ((&&&))
import           Control.Monad          (join)
import           Control.Monad.Except   (ExceptT, runExceptT)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Maybe             (mapMaybe)
import qualified Data.Text              as T
import           Data.Traversable       (for)
import           Data.Vector.Storable   (thaw)
import           Linear                 (V2 (..), V4 (..))
import           SDL                    (Point (..), Rectangle (..), Window,
                                         ($=))
import qualified SDL
import qualified SDL.Raw.Types          as SDL (Rect (..))
import           System.Directory       (doesFileExist)
import           Typograffiti.SDL       as Typo

import           Mutant.API.Events
import           Mutant.API.Render2d
import           Mutant.Backend
import           Mutant.Geom
import           Mutant.KeyboardCodes
import           Mutant.Slot


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


rect2RawRectangle
  :: Integral i
  => Rect i
  -> SDL.Rect
rect2RawRectangle (Rect (V2 x y) (V2 w h)) =
  SDL.Rect
    (fromIntegral x)
    (fromIntegral y)
    (fromIntegral w)
    (fromIntegral h)


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
  -> m (Render2dAPI 'BackendSDL m)
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

    , measureText = \(SDLFont file) (V2 w h) str -> runOrFail $ do
        RenderedGlyphs _ sz <-
            getTextRendering
              r
              fstore
              (pfx ++ file)
              (GlyphSizeInPixels w h)
              str
        return sz

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

    , setRenderTarget = \case
        Nothing ->
          SDL.rendererRenderTarget r $= Nothing
        Just (SDLTexture s) -> do
          -- read the tex and get its size
          tex <- readSlot s
          info <- SDL.queryTexture tex
          case SDL.textureAccess info of
            SDL.TextureAccessTarget ->
              SDL.rendererRenderTarget r $= Just tex
            _ -> do
              size <-
                fmap fromIntegral
                . uncurry V2
                . (SDL.textureWidth &&& SDL.textureHeight)
                <$> SDL.queryTexture tex
              -- make a texture to draw into
              ttex <- SDL.createTexture r SDL.RGBA8888 SDL.TextureAccessTarget size
              SDL.textureBlendMode ttex $= SDL.BlendAlphaBlend
              SDL.rendererRenderTarget r $= Just ttex
              -- draw the original texture into it
              let source = rect2Rectangle $ Rect 0 size
              SDL.copy r tex (Just source) (Just source)
              -- update the input texture slot
              writeSlot s ttex
              -- destroy the old texture
              SDL.destroyTexture tex

    , font = \file -> liftIO (doesFileExist (pfx ++ file)) >>= newSlot . \case
        True -> LoadStatusSuccess $ SDLFont file
        False ->
          LoadStatusFailure
            $ unwords
              [ show file
              , "does not exist."
              ]
    }


fromSDLButton :: SDL.MouseButton -> Maybe MouseButton
fromSDLButton = \case
  SDL.ButtonLeft -> Just MouseButtonLeft
  SDL.ButtonMiddle -> Just MouseButtonMiddle
  SDL.ButtonRight -> Just MouseButtonRight
  _ -> Nothing


fromSDLMotion :: SDL.InputMotion -> InputMotion
fromSDLMotion SDL.Released = Released
fromSDLMotion SDL.Pressed  = Pressed


fromSDLKeysym :: SDL.Keysym -> Keysym
fromSDLKeysym (SDL.Keysym s k md) =
  Keysym
    (Scancode $ SDL.unwrapScancode s)
    (Keycode $ SDL.unwrapKeycode k)
    $ KeyModifier
      { keyModifierLeftShift  = SDL.keyModifierLeftShift md
      , keyModifierRightShift = SDL.keyModifierRightShift md
      , keyModifierLeftCtrl   = SDL.keyModifierLeftCtrl md
      , keyModifierRightCtrl  = SDL.keyModifierRightCtrl md
      , keyModifierLeftAlt    = SDL.keyModifierLeftAlt md
      , keyModifierRightAlt   = SDL.keyModifierRightAlt md
      , keyModifierLeftGUI    = SDL.keyModifierLeftGUI md
      , keyModifierRightGUI   = SDL.keyModifierRightGUI md
      , keyModifierNumLock    = SDL.keyModifierNumLock md
      , keyModifierCapsLock   = SDL.keyModifierCapsLock md
      , keyModifierAltGr      = SDL.keyModifierAltGr md
      }


fromSDLPayload :: SDL.EventPayload -> Maybe EventPayload
fromSDLPayload = \case
  SDL.MouseMotionEvent (SDL.MouseMotionEventData _ _ btns (SDL.P pos) rel) ->
    Just
      $ EventMouseMotion
      $ MouseMotionEvent
          (mapMaybe fromSDLButton btns)
          (fromIntegral <$> pos)
          (fromIntegral <$> rel)
  SDL.MouseButtonEvent (SDL.MouseButtonEventData _ mot _ sdlBtn clks (SDL.P pos)) ->
    flip fmap (fromSDLButton sdlBtn) $ \btn ->
      EventMouseButton
        $ MouseButtonEvent
            (fromIntegral <$> pos)
            btn
            (fromSDLMotion mot)
            (fromIntegral clks)
  SDL.KeyboardEvent (SDL.KeyboardEventData _ mot rep ksym) ->
    Just
      $ EventKeyboard
          $ KeyboardEvent
              (fromSDLMotion mot)
              rep
              (fromSDLKeysym ksym)
  SDL.TextInputEvent (SDL.TextInputEventData _ txt) ->
    Just
      $ EventTextInput
          $ TextEvent txt
  SDL.QuitEvent ->
    Just
      $ EventQuit
  _ -> Nothing


fromSDLEvent :: SDL.Event -> Maybe Event
fromSDLEvent (SDL.Event millis payload) =
  Event millis
    <$> fromSDLPayload payload


getSDLEventsAPI
  :: MonadIO m
  => MutantInstance 'BackendSDL
  -> m (EventsAPI 'BackendSDL m)
getSDLEventsAPI _ = do
  SDL.initialize $ Just SDL.InitEvents
  return
    EventsAPI
    { pollEvents =
        mapMaybe fromSDLEvent
          <$> SDL.pollEvents
    , waitEvent =
        (join . fmap fromSDLEvent <$>)
        . SDL.waitEventTimeout
        . fromIntegral
    --, injectEvent =
    , beginTextInput =
        SDL.startTextInput
        . rect2RawRectangle
    , endTextInput =
        SDL.stopTextInput
    }


getSDLAnimationFrameAPI
  :: Monad m
  => MutantInstance 'BackendSDL
  -> m (AnimationFrameAPI 'BackendSDL m)
getSDLAnimationFrameAPI _ =
  return
    AnimationFrameAPI
    { requestAnimation = \f -> f}
