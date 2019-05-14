{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE LambdaCase      #-}
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

import           Mutant.Backend         (Backend (..))
import           Mutant.Geom            (Line (..), Rect (..), insetRect)
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

-- | Helps with writing backends.
data Canvas window ctx a
  = Canvas
  { canvasWindow :: window
  , canvasCtx    :: ctx
  , canvasExtra  :: a
  }


testTextureSize
  :: MonadIO m
  => Render2dAPI i m
  -> FilePath
  -> m Bool
testTextureSize Render2dAPI{..} fp = do
  tex <- await =<< texture fp
  tsz <- textureSize tex
  dim <- withTexture tex getDimensions
  return $ tsz == dim


-- | This is a test.
drawingStuff
  :: MonadIO m
  => Render2dAPI i m
  -> m ()
drawingStuff Render2dAPI{..} = do
  wh@(V2 w h) <- getDimensions
  liftIO $ putStrLn $ "Context has dimensions " ++ show wh
  let tl = 10
      tr = V2 (w - 10) 10
      br = V2 (w - 10) (h - 10)
      bl = V2 10 (h - 10)
      lns = [Line tl br, Line tr bl]
      grey = V4 174 174 174 255
      black = V4 255 255 255 255
      white = V4 255 255 255 255
      cyan = V4 0 255 255 255
      canary = V4 255 255 0 255

  clear

  -- first draw the screen grey
  setDrawColor grey
  fillRect
    $ Rect 0 wh

  -- draw a white frame inset by 10 pixels
  setDrawColor white
  strokeRect
    $ insetRect (Rect 0 wh) 10

  -- then draw a cyan X
  setDrawColor cyan
  traverse_ strokeLine lns

  -- load an image, get its size
  tex <- await =<< texture "sot.png"
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
  setDrawColor black
  komika <- await =<< font "komika.ttf"
  fillText
    komika
    (V2 16 16)
    (V2 100 100)
    "Here is some text..."

  -- present the window
  present

  -- loop so the window won't close on desktop
  fix $ \loop -> do
    liftIO $ threadDelay 1000000
    loop
