module ClickAndFollow where

import Stage
import Stage(Stage, ForATime)
import Stage.Infix(..)
import Time
import Signal
import Time(second)
import Graphics.Collage(..)
import Graphics.Element(..)
import Color
import Mouse
import Easing(..)

aNiceDrawing = toForm (image 200 200 "https://avatars2.githubusercontent.com/u/1322514?v=3&s=460")

clickPosns : Signal (Int, Int)
clickPosns = Signal.sampleOn Mouse.clicks Mouse.position 

type alias State = ((Float, Float), Stage ForATime (Float, Float))

w = 500
h = 500

drawingPos : Signal (Stage ForATime (Float, Float))
drawingPos = 
  let update : (Int, Int) -> State -> State
      update mousePos (currPos, _) =
        let nextPos = pagePosToWorldPos mousePos in
        ( nextPos
        , Stage.for second <|
            ease easeInOutQuad (pair float) currPos nextPos second)
  in
  Signal.foldp update ((0, 0), Stage.stayFor 0 (0,0)) clickPosns
  |> Signal.map snd

clickCircle =
  let circleData = Signal.map (\p ->
        Stage.forever (\t -> (pagePosToWorldPos p, t * 500/second)))
        clickPosns
  in
  Stage.run circleData (Time.every 30)
  |> Signal.map (\(p, r) -> move p <| outlined (solid Color.red) <| circle r)

main =
  let drawing =
        Stage.run (Signal.map Stage.sustain drawingPos) (Time.every 30)
        |> Signal.map (\p -> move p aNiceDrawing)
  in
  Signal.map2 (\c d -> collage w h [c,d])
    clickCircle drawing


{-
main =
  Stage.run (Signal.map Stage.sustain drawingPos) (Time.every 30)
  |> Signal.map (\p -> collage w h [move p aNiceDrawing])
-}
-- Util
pagePosToWorldPos (x, y) = (toFloat x - w/2, h/2 - toFloat y)

