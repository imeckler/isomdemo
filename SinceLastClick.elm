module SinceLastClick where

import Stage
import Stage(Stage, ForATime, Forever)
import Stage.Infix(..)
import Time
import Signal
import Time(second, Time)
import Graphics.Collage(..)
import List
import Color
import Mouse
import Easing(..)

sinceLastClick : Signal (Stage Forever Time)
sinceLastClick = Signal.map (\_ -> Stage.forever (\t -> t)) Mouse.clicks

draw t =
  let w = t * pxPerMs in
  rect w 20 |> filled Color.red |> moveX (w/2)

main =
  Stage.run sinceLastClick (Time.every 30)
  |> Signal.map (\t -> collage 500 500 [draw t, ticks])

pxPerMs = 50 / second
ticks = 
  List.map (\i -> let x = i * 50 in traced defaultLine (segment (x,-10) (x,10))) [0..20]
  |> group

