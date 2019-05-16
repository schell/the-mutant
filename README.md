# The Mutant
This is a framework of game related APIs and backends for aspiring Haskell game enthusiasts.

## APIs

- [x] [2d graphics](https://github.com/schell/the-mutant/blob/master/src/Mutant/API/Render2d.hs)
- [ ] [input events](https://github.com/schell/the-mutant/blob/master/src/Mutant/API/Events.hs)
- [ ] audio
- [ ] 2d physics
- [ ] 3d gles graphics

## Backends

- [x] Linux
- [x] macOS
- [x] Windows
- [x] browser
- [ ] iOS
- [ ] Android

## Background
*The Mutant* picks up where other projects like [gelatin][gelatin], [glucose][glucose] and
[odin][odin] left off. My goal is to bring back some of the cool features from
those failed projects and get them up and running again in a slightly new context.
The point of it all is to be able to quickly write portable video games and multimedia
applications in Haskell.

*The Mutant* gets its name from its unapologetic use of in-place mutation, which
many Haskell devs fear.

Don't fear The Mutant.

Similarly, it "mutates" your game onto a
number of target platforms - the name is a double entendre. It also lends itself to cool
icons.

Don't fear The Mutant.

[gelatin]: https://github.com/schell/gelatin
[glucose]: https://github.com/schell/glucose
[odin]: https://github.com/schell/odin

## Examples

The various APIs are defined using records. Records live at an interesting point
in the solution space between typeclasses and effects. Unlike both typeclasses
and many effect systems, records have great type inference. There is no need to
type apply any of the API functions in order to use them. Unlike many effect
systems, records are fast and there are no new concepts or puzzles to solve in
order to run your computations - just pass the API to the function that needs them.

With records, just like typeclasses - you get an abstract API. Similar to an
effects system you get to see what "effects" are likely happening within your
computation simply by viewing the type signature.

### Rendering

```haskell
{-# LANGUAGE RecordWildCards #-}

import Mutant.API.Render2d
import Mutant.Geom

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
  null setDrawColor white
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
```

### Creating instances of an API

The various APIs all have creation functions provided by multiple backends.
Here's an example of creating a "mutant instance" and the 2d rendering API and
passing it to the previous `drawingStuff` function:

```haskell
import Mutant.Backends.SDL

main :: IO ()
main = do
  i <- getSDLInstance "My SDL Game" (V2 640 480)
  r <- getSDLRender2dAPI i "assetsDirectory/"
  drawingStuff r
```
