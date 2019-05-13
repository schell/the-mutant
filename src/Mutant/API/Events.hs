{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
module Mutant.API.Events where

import           Data.Int       (Int32)
import           Data.Kind      (Type)
import           Data.Text      (Text)
import           Data.Word      (Word32, Word64)
import           Linear         (V2)

import           Mutant.Backend


-- | Buttons on a mouse.
data MouseButton
  = MouseButtonLeft
  | MouseButtonMiddle
  | MouseButtonRight
  deriving (Show, Eq)


-- | The direction of some input motion.
data InputMotion
  = Released
  | Pressed
  deriving (Show, Eq)


-- | A mouse or pointer device was moved.
data MouseMotionEvent
  = MouseMotionEvent
  { -- | The buttons held down during this motion.
    mouseMotionEventButtons  :: ![MouseButton]
    -- | The new absolute position of the mouse.
  , mouseMotionEventPosition :: !(V2 Int)
    -- | The relative motion of this event.
  , mouseMotionEventMovement :: !(V2 Int)
  } deriving (Show, Eq)


-- | A mouse or pointer device changed button state (clicked).
data MouseButtonEvent
  = MouseButtonEvent
  { -- | The absolute position of the mouse at the time of the click.
    mouseButtonEventPosition :: !(V2 Int)
    -- | The button that changed state
  , mouseButtonEventButton   :: !MouseButton
    -- | The button motion
  , mouseButtonEventMotion   :: !InputMotion
    -- | The number of clicks
  , mouseButtonEventClicks   :: !Int
  } deriving (Show, Eq)


data KeyModifier
  = KeyModifier
  { keyModifierLeftShift  :: Bool
  , keyModifierRightShift :: Bool
  , keyModifierLeftCtrl   :: Bool
  , keyModifierRightCtrl  :: Bool
  , keyModifierLeftAlt    :: Bool
  , keyModifierRightAlt   :: Bool
  , keyModifierLeftGUI    :: Bool
  , keyModifierRightGUI   :: Bool
  , keyModifierNumLock    :: Bool
  , keyModifierCapsLock   :: Bool
  , keyModifierAltGr      :: Bool
  } deriving (Show, Eq)


data Keysym
  = Keysym
  { keysymScancode :: Word32
  , keysymKeycode  :: Int32
  , keysymModifier :: KeyModifier
  } deriving (Show, Eq)


data KeyboardEvent
  = KeyboardEvent
  { keyboardEventMotion :: !InputMotion
  , keyboardEventRepeat :: !Bool
  , keyboardEventKeysym :: !Keysym
  } deriving (Show, Eq)


newtype TextEvent
  = TextEvent
  { textEventText :: Text }
  deriving (Show, Eq)


data EventPayload
  = EventMouseMotion !MouseMotionEvent
  | EventMouseButton !MouseButtonEvent
  | EventKeyboard !KeyboardEvent
  | EventTextInput !TextEvent
  | EventQuit
  deriving (Show, Eq)


data Event
  = Event
  { -- | Monotonic event time in nanoseconds since some unspecified starting point.
    eventTime    :: Word64
    -- | The event payload.
  , eventPayload :: EventPayload
  }


data EventsAPI (i :: Backend) (m :: Type -> Type)
  = EventsAPI
  { -- | Get all pending events. Never blocks - if no events are in the queue,
    -- @[]@ is returned.
    pollEvents :: m [Event]
    -- | Block for a specific number of milliseconds waiting for an event.
  , waitEvent  :: Int -> m (Maybe Event)
  }
