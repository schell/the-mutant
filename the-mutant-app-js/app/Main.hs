{-# LANGUAGE CPP #-}
module Main where

import           Linear                  (V2 (..))
import           Mutant.Backends.JSaddle (getJSAnimationFrameAPI,
                                          getJSEventsAPI, getJSInstance,
                                          getJSRender2dAPI, runJS)
import           MutantExample           (mainLoop)


main :: IO ()
main = runJS "assets/" 8888 $ do
  i <- getJSInstance (V2 640 480)
  r <- getJSRender2dAPI i
  a <- getJSAnimationFrameAPI i
  e <- getJSEventsAPI i
  mainLoop r a e
