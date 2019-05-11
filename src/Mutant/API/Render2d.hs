{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
module Mutant.API.Render2d where

import           Control.Concurrent     (threadDelay)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Foldable          (traverse_)
import           Data.Function          (fix)
import           Data.Kind              (Type)
import           Linear                 (V2 (..), V4 (..))

import           Mutant.Slot


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


data Render2dBackend
  = Render2dJS
  | Render2dSDL


data family Texture (i :: Render2dBackend)
data family Font (i :: Render2dBackend)


data Render2d (i :: Render2dBackend) (m :: Type -> Type)
  = Render2d
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

    -- | Draw some text
  , fillText
    :: Font i
    -- ^ The font to use
    -> V2 Int
    -- ^ The glyph size
    -> V4 Int
    -- ^ The text color
    -> V2 Int
    -- ^ The position of the first line of text.
    -> String
    -- ^ The text to draw
    -> m ()

    -- | Load a texture
  , texture
      :: FilePath
      -> m (Slot (LoadStatus (Texture i)))

    -- | Return the size of the texture
  , textureSize
      :: Texture i
      -> m (V2 Int)

    -- | Perform a series of rendering commands into the given texture.
  , withTexture
      :: forall a
       . Texture i
      -- ^ The texture to draw into
      -> m a
      -- ^ The draw calls to draw into the texture
      -> m a

    -- | Load a new font face.
  , font
      :: FilePath
      -> m (Slot (LoadStatus (Font i)))
  }

-- | Helps with writing interpreters.
data Canvas window ctx a
  = Canvas
  { canvasWindow :: window
  , canvasCtx    :: ctx
  , canvasExtra  :: a
  }


testTextureSize
  :: MonadIO m
  => Render2d i m
  -> FilePath
  -> m Bool
testTextureSize Render2d{..} fp = do
  tex <- await =<< texture fp
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
      black = V4 174 174 174 255
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
  tex <- await =<< texture "sot.png"
  -- loop until it's actually loaded, `textureLoad` is synchronous on sdl and
  -- async in javascript
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

  -- play with fonts and text
  komika <- await =<< font "komika.ttf"

  fillText
    komika
    (V2 16 16)
    (V4 255 255 255 255)
    (V2 100 100)
    "Here is some text..."

  -- present the window
  present
  -- loop so the window won't close
  fix $ \loop -> do
    liftIO $ threadDelay 1000000
    loop
