{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin         #-}
module Mutant.Eff.UI where

import           Data.Text (Text)
import           Polysemy  (makeSem)


newtype Node
  = Node { nodeId :: Int }
  deriving (Show)

data Event
  = EventClick
  deriving (Show)


data Listener
  = Listener
  { listenerId     :: Int
  , listenerNodeId :: Int
  , listenerEvent  :: Event
  } deriving (Show)


newtype Stage
  = Stage
  { stageNode :: Node }
  deriving (Show)


newtype Label
  = Label
  { labelNode :: Node }
  deriving (Show)


data UI m a where
  GetStage       :: UI m Stage
  NewLabel       :: Text -> UI m Label
  UpdateLabel    :: Label -> Text -> UI m ()
  AppendChild    :: Node -> Node -> UI m ()
  AddListener    :: Node -> Event -> m () -> UI m Listener
  RemoveListener :: Listener -> UI m ()
makeSem ''UI
