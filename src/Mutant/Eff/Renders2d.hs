{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
module Mutant.Eff.Renders2d where

import           Data.Kind              (Type)
--import           Data.Vector.Storable (Vector)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Foldable          (for_)
import           Data.Function          (fix)
import           GHC.Conc               (threadDelay)
import           Linear                 (V2 (..))
import           Polysemy               hiding (run)


data Rect a
  = Rect
  { rectUpperLeft :: V2 a
  , rectExtents   :: V2 a
  }


data Renders2dBackend
  = Renders2dJSaddle
  | Renders2dSDL


data family Texture (i :: Renders2dBackend)


data Renders2d (i :: Renders2dBackend) (m :: Type -> Type) a where
  Clear           :: Renders2d i m ()
  Present         :: Renders2d i m ()
  GetDimensions   :: Renders2d i m (V2 Int)

  DrawLine        :: V2 Int -> V2 Int -> Renders2d i m ()
  DrawRect        :: Rect Int -> Renders2d i m ()
  DrawTexture     :: Texture i -> Rect Int -> Rect Int -> Renders2d i m ()

  TextureLoad     :: String-> Renders2d i m (Either String (Texture i))
  TextureSize     :: Texture i -> Renders2d i m (V2 Int)
  TextureIsLoaded :: Texture i -> Renders2d i m Bool
makeSem ''Renders2d


-- | Helps with writing interpreters.
data Canvas window ctx
  = Canvas
  { canvasWindow :: window
  , canvasCtx    :: ctx
  }


-- | This is a test.
drawingStuff
  :: ( Member (Renders2d i) r
     , Member (Lift IO) r
     )
  => Sem r ()
drawingStuff = do
  V2 w h <- getDimensions
  let tl = 10
      tr = V2 (w - 10) 10
      br = V2 (w - 10) (h - 10)
      bl = V2 10 (h - 10)
      points = [tl, tr, br, bl]
      lnes = zip points $ drop 1 points ++ [tl]
      frame
        :: Member (Renders2d i) r
        => Member (Lift IO) r
        => Sem r ()
        -> Sem r ()
      frame f = do
        clear
        f
        present
        liftIO $ threadDelay 500000
  fix $ \loop -> do
    for_ lnes
      $ frame . uncurry drawLine
    frame
      $ drawRect
      $ Rect tl (br - tl)
    frame
      $ return ()
    loop
