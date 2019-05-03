{-# LANGUAGE LambdaCase #-}
import           Control.Concurrent.Async              (async, cancel)
import           Data.Foldable                         (traverse_)
import qualified Mutant.Interpreters.JSaddle.Renders2d as JS
import qualified Mutant.Interpreters.SDL.Renders2d     as SDL
import           System.Environment                    (getArgs)


main :: IO ()
main = getArgs >>= \case
  ("--js":_) -> JS.renders2dTest
  ("--sdl":_) -> SDL.renders2dTest
  _ -> do
    threads <- traverse async [JS.renders2dTest, SDL.renders2dTest]
    putStrLn "press enter to exit"
    _ <- getLine
    traverse_ cancel threads
