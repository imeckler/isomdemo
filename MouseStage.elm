module MouseStage where

import Stage
import Stage(Stage, ForATime)
import Stage.Infix(..)
import Time
import Signal
import Time(second)
import Graphics.Collage(..)
import Color
import Mouse
import Easing(..)

aNiceDrawing = filled (Color.blue) (ngon 3 30)

clickPosns : Signal (Int, Int)
clickPosns = Signal.sampleOn Mouse.clicks Mouse.position 

type alias State = ((Float, Float), Stage ForATime (Float, Float))

w = 500
h = 500

drawingPos : Signal (Stage ForATime (Float, Float))
drawingPos = 
  let update : (Int, Int) -> State -> State
      update (mx, my) ((x, y), _) =
        let (x', y') = (toFloat mx - w/2, h/2-toFloat my) in
        ( (x', y')
        , Stage.for second <|
            ease easeInOutQuad (pair float) (x, y) (x', y') second)
  in
  Signal.foldp update ((0, 0), Stage.stayFor 0 (0,0)) clickPosns
  |> Signal.map snd

main =
  Stage.run (Signal.map Stage.sustain drawingPos) (Time.every 30)
  |> Signal.map (\p -> collage w h [move p aNiceDrawing])

