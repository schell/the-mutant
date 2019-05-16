module Main where

import           Linear              (V2 (..))
import           Mutant.Backends.SDL (getSDLEventsAPI, getSDLInstance,
                                      getSDLRender2dAPI, getSDLAnimationFrameAPI)
import           MutantExample       (mainLoop)

main :: IO ()
main = do
  i <- getSDLInstance "The Mutant Example" (V2 640 480)
  r <- getSDLRender2dAPI i "assets/"
  a <- getSDLAnimationFrameAPI i
  e <- getSDLEventsAPI i
  mainLoop r a e
