{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
module MutantExample
    ( mainLoop
    ) where

import           Control.Monad          (unless)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader (..), ReaderT (..))
import           Data.Foldable          (foldl')
import           Data.Function          (fix)
import           GHC.Clock              (getMonotonicTime)
import           Linear                 (V2 (..), V4 (..))

import           Mutant.API.Events      (Event (..), EventPayload (..),
                                         EventsAPI (..))
import           Mutant.API.Render2d    (AnimationFrameAPI (..), Font,
                                         Render2dAPI (..), await, raiseRender2d)
--import           Mutant.Control.Suspend (done, race, runSuspend, suspend, wait)
import           Mutant.Geom            (Rect (..))
import           Mutant.Slot


data AppState
  = AppState
  { appStateMousePos :: V2 Int }


data ButtonState
  = ButtonStateUp
  | ButtonStateOver
  | ButtonStateDown
  deriving (Eq)


-- | Renders a button and returns the state of the button this frame.
_button
  :: Monad m
  => Render2dAPI i m
  -- ^ The rendering API
  -> Font i
  -- ^ Font to display
  -> V2 Int
  -- ^ Font glyph size
  -> V2 Int
  -- ^ Button position
  -> String
  -- ^ Button text
  -> m ButtonState
_button Render2dAPI{..} fnt sz _pos str = do
  _txtsz <- measureText fnt sz str
  undefined


app
  :: MonadReader AppState m
  => Render2dAPI i m
  -> Font i
  -> Int
  -> Int
  -> m ()
app Render2dAPI{..} fnt rate count = do
  setDrawColor
    $ V4 50 50 50 255
  fillRect
    . Rect 0
    =<< getDimensions
  setDrawColor
    $ V4 255 255 0 255
  fillText
    fnt
    32
    32
    $ show (count, rate)


foldEvent
  :: (Bool, AppState)
  -> Event
  -> (Bool, AppState)
foldEvent (True      , s) _  = (True, s)
foldEvent (shouldQuit, s) (Event _ p) = case p of
  EventQuit -> (True, s)
  _         -> (shouldQuit, s)


calculateFPS
  :: MonadIO m
  => Slot Double
  -> Slot [Double]
  -> m Int
calculateFPS lastTime frames = do
  now <- liftIO getMonotonicTime
  prev <- stateSlot lastTime (,now)
  fs <- withSlot frames
    $ \fs -> take 100 $ now - prev : fs
  return
    $ round
    $ 100.0 / sum fs


mainLoop
  :: MonadIO m
  => Render2dAPI i m
  -> AnimationFrameAPI i m
  -> EventsAPI i m
  -> m ()
mainLoop
  r@Render2dAPI{..}
  AnimationFrameAPI{..}
  _e@EventsAPI{..} = do

  -- load the font
  komika <- await =<< font "komika.ttf"
  lastTime <- newSlot =<< liftIO getMonotonicTime
  frames <- newSlot $ replicate 100 0
  count <- newSlot 0
  sVar <-
    newSlot
      $ AppState
        { appStateMousePos = V2 0 0 }

  fix $ \loop -> do
    newCount <- withSlot count succ
    fps <- calculateFPS lastTime frames
    s <- readSlot sVar
    -- run our events
    (shouldQuit, s1) <-
      foldl' foldEvent (False, s)
        <$> pollEvents
    writeSlot sVar s1
    -- clear the screen, render by running
    -- the app, present and loop
    clear
    let r1 = raiseRender2d r (ReaderT . const)
    runReaderT (app r1 komika fps newCount) s1
    present
    liftIO $ putStrLn $ show newCount
    unless shouldQuit
      $ requestAnimation loop
