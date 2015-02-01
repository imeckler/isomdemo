module Simple where

import Stage
import Stage.Infix(..)
import Time
import Signal
import Time(second)
import Graphics.Collage(..)
import Color

aNiceDrawing = filled (Color.blue) (ngon 3 30)

pos =
  Stage.for (1 * second) (\t -> t * (40 / second))
  |> Stage.sustain

-- TODO: Try cycle

pos' =
  (Stage.for (1 * second) (\t -> t * (40 / second))
  +> \x0 -> Stage.for (1 * second) (\t -> x0 - t * (40 / second)))
  |> Stage.cycle

main =
  Stage.run (Signal.constant pos') (Time.every 30)
  |> Signal.map (\p -> collage 500 500 [moveX p aNiceDrawing])

