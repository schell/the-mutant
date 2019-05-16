{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
module Mutant.API.Render2d where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Function          (fix)
import           Data.Kind              (Type)
import           Linear                 (V2 (..), V4 (..))

import           Mutant.Backend         (Backend (..))
import           Mutant.Geom            (Line (..), Rect (..))
import           Mutant.Slot            (Slot, readSlot)


data LoadStatus a
  = LoadStatusLoading
  | LoadStatusSuccess a
  | LoadStatusFailure String
  deriving (Eq)


onLoadFailure
  :: Monad m
  => (String -> m ())
  -> LoadStatus a
  -> m ()
onLoadFailure f s =
  case s of
    LoadStatusFailure err -> f err
    _                     -> return ()

await
  :: MonadIO m
  => Slot (LoadStatus a)
  -> m a
await t =
  fix $ \loop -> readSlot t >>= \case
    LoadStatusLoading -> loop
    LoadStatusFailure err -> fail err
    LoadStatusSuccess a -> return a


-- | Represents a texture maintained by the target backend.
data family Texture (i :: Backend)


-- | Represents a font maintained by the target backend.
data family Font (i :: Backend)


data Render2dAPI (i :: Backend) (m :: Type -> Type)
  = Render2dAPI
  { -- | Clear the rendering context.
    clear :: m ()

    -- | Present the rendering context to display the frame.
  , present :: m ()

    -- | Return the dimensions of the rendering context.
  , getDimensions   :: m (V2 Int)

    -- | Set the color of future drawing operations in the current context.
  , setDrawColor
      :: V4 Int
         -- ^ The color, eg @let white = V4 255 255 255 255 in setDrawColor white@
      -> m ()

    -- | Draw a stroked line.
  , strokeLine :: Line Int -> m ()

    -- | Draw a stroked rectangle.
  , strokeRect :: Rect Int -> m ()

  -- | Draw a filled rectangle.
  , fillRect :: Rect Int -> m ()

  -- | Draw some portion of the given texture to the current rendering context.
  , fillTexture
    :: Texture i
    -- ^ The texture to draw from.
    -> Rect Int
    -- ^ The source rectangle within the texture.
    -> Rect Int
    -- ^ The destination rectangle in the current context where the drawing
    -- should go. The texture will be stretched or squished to fit.
    -> m ()

    -- | Draw some text.
    -- Uses the current draw color as the text color.
  , fillText
      :: Font i
      -- ^ The font to use
      -> V2 Int
      -- ^ The glyph size
      -> V2 Int
      -- ^ The position of the first line of text.
      -> String
      -- ^ The text to draw
      -> m ()

    -- | Measure the size of the text.
  , measureText
      :: Font i
      -- ^ The font
      -> V2 Int
      -- ^ The glyph size
      -> String
      -- ^ The string to measure
      -> m (V2 Int)

    -- | Load a texture
  , texture
      :: FilePath
      -> m (Slot (LoadStatus (Texture i)))

    -- | Return the size of the texture
  , textureSize
      :: Texture i
      -> m (V2 Int)

    -- | Bind a texture as the rendering target or reset the target to the main
    -- drawing context.
  , setRenderTarget
      :: Maybe (Texture i)
      -- ^ The texture to draw into or @Nothing@ to reset to the main drawing
      -- context.
      -> m ()

    -- | Load a new font face.
  , font
      :: FilePath
      -> m (Slot (LoadStatus (Font i)))

  }


-- | Perform a series of rendering commands into the given texture.
withTexture
  :: Monad m
  => Render2dAPI i m
  -> Texture i
  -- ^ The texture to draw into
  -> m a
  -- ^ The draw calls to draw into the texture
  -> m a
withTexture Render2dAPI{..} t ma = do
  setRenderTarget $ Just t
  a <- ma
  setRenderTarget Nothing
  return a


testTextureSize
  :: MonadIO m
  => Render2dAPI i m
  -> FilePath
  -> m Bool
testTextureSize r@Render2dAPI{..} fp = do
  tex <- await =<< texture fp
  tsz <- textureSize tex
  dim <- withTexture r tex getDimensions
  return $ tsz == dim


raiseRender2d
  :: Render2dAPI i m
  -- ^ the API
  -> (forall a. m a -> t m a)
  -- ^ A function for raising API computations. ie (ReaderT . const)
  -> Render2dAPI i (t m)
raiseRender2d r f =
  Render2dAPI
  { clear = f $ clear r
  , present = f $ present r
  , getDimensions = f $ getDimensions r
  , setDrawColor = f . setDrawColor r
  , strokeLine = f . strokeLine r
  , strokeRect = f . strokeRect r
  , fillRect = f . fillRect r
  , fillTexture = \t s d -> f $ fillTexture r t s d
  , fillText = \fnt sz p str -> f $ fillText r fnt sz p str
  , measureText = \fnt sz str -> f $ measureText r fnt sz str
  , texture = f . texture r
  , textureSize = f . textureSize r
  , setRenderTarget = f . setRenderTarget r
  , font = f . font r
  }


data AnimationFrameAPI (i :: Backend) (m :: Type -> Type)
  = AnimationFrameAPI
  { -- | Request that a computation be called next frame.
    -- You should call this method whenever you're ready to update your animation
    -- on screen. This ensures smooth animation on all target platforms.
    requestAnimation
      :: m ()
      -> m ()
  }
