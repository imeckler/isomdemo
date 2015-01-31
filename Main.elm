module Main where

import Easing(..)
import Stage (Stage, ForATime, Forever)
import Stage
import Stage.Infix(..)
import Transform2D(Transform2D)
import Transform2D
import Time (second)
import Signal
import List
import List((::))
import Graphics.Input (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Color
import Text
import Time
import Native.DemoUtil
import Html

type Transformation
  = Translate (Float, Float)
  | Rotation Float
  | ReflectY

animated : Transformation -> Transform2D -> Stage ForATime Transform2D
animated t tInit = case t of
  Translate pt ->
    Stage.for second
      (  ease easeInOutQuad (pair float) (0,0) pt second
      >> uncurry Transform2D.translation
      >> firstDo tInit)

  Rotation x ->
    Stage.for second
      (  ease easeInOutQuad float 0 x second
      >> Transform2D.rotation
      >> firstDo tInit)
  
  ReflectY ->
    Stage.for second
      (ease easeInOutQuad float 1 -1 second >> Transform2D.scaleX >> firstDo tInit)

 -- If only we had Either or polymorphic variants...
type TUpdate = Go | AddAnother Transformation
transSequencesBuilder : Signal (List Transformation, List Transformation)
transSequencesBuilder =
  Signal.merge (Signal.map (\_ -> Go) goClicks) (Signal.map AddAnother transformations)
  |> Signal.foldp (\u (curr, ts) -> case u of 
      Go           -> (List.reverse ts, [])
      AddAnother t -> (curr, t::ts)) ([], [])

transSequences     = Signal.map fst (Signal.sampleOn goClicks transSequencesBuilder)
sequenceInProgress = Signal.map snd transSequencesBuilder

tranimations : Signal (Stage Forever Transform2D)
tranimations =
  let trivial = (Transform2D.identity, Stage.stayForever Transform2D.identity)
      animNext : List (Transformation)
               -> (Transform2D, Stage Forever Transform2D)
               -> (Transform2D, Stage Forever Transform2D)
      animNext ts (lastTrans, _) = case ts of
        []      -> trivial
        t0::ts' ->
          let t' = List.foldl (\t r -> r +> animated t) (animated t0 lastTrans) ts' in
          (Stage.finalValue t', Stage.sustain t')
  in
  Signal.foldp animNext trivial transSequences
  |> Signal.map snd

trans = Stage.run tranimations (Time.every 30)

-- Signals and whatnot
goClicks : Signal ()
goClicks = Signal.subscribe goChan
goChan   = Signal.channel ()

transformations : Signal Transformation
transformations = Signal.subscribe transChan
transChan       = Signal.channel (Rotation 0)

goButton        = button (Signal.send goChan ()) "Go"
rotateButton    = button (Signal.send transChan (Rotation (pi / 4))) "Rotate"
translateButton = button (Signal.send transChan (Translate (20, 40))) "Translate"
reflectButton   = button (Signal.send transChan ReflectY) "Reflect"

-- Boring drawing stuff
drawing = Signal.map (\t -> 
  let sty = Text.defaultStyle in
  collage 500 500
  [ groupTransform t
    [ group 
      [ outlined (solid Color.blue) (square 40) 
      , toForm <| Text.centered <| Text.style {sty | height <- Just 30} (Text.fromString "R")
      ]
      |> moveX 100
    ]
  , traced (dotted Color.black) (segment (0, -1000) (0, 1000))
  , filled (Color.red) (circle 5)
  ])
  trans

drawTransStack =
  Html.toElement 300 500
  << Html.ul []
  << revMap (Html.li [] << sing << Html.text << toString)

main =
  Signal.map2 (\d ts -> 
    flow down
    [ d
    , flow right [translateButton, rotateButton, reflectButton, goButton]
    , drawTransStack ts
    ])
    drawing sequenceInProgress

-- UTIL
firstDo t1 t2 = Transform2D.multiply t2 t1

sing x = [x]

revMap f =
  let go acc xs = case xs of {[] -> acc; (x::xs') -> go (f x::acc) xs'}
  in go []
