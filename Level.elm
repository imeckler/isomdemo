module Level where

import Util(..)
import Move
import Move(Move(..))
import Graphics.Element(..)
import Transform2D(Transform2D)
import Html(..)
import Html.Attributes(..)
import Html.Events(..)
import Native.IsomUtil
import Color
import Time (second)
import Stage

type alias Level =
  { availableMoves : List Move
  , maxMoves       : Int
  , goal           : Transform2D
  }

type alias GameState =
  { movesLeft : Int
  , currMove  : Move
  , postMove  : Transform2D
  , preMove   : Transform2D
  , hasWon    : Bool
  }

type Update
  = Clicked Move
  | Hovered Move

tuply : Transform2D -> (Float, Float, Float, Float, Float, Float)
tuply = Native.IsomUtil.tuply

winning lev trans = 
  let (g0,g1,g2,g3,g4,g5) = tuply lev.goal
      (t0,t1,t2,t3,t4,t5) = tuply trans
      d = 
        List.map2 (\g t -> (g - t)^2)
          [g0,g1,g2,g3,g4,g5] [t0,t1,t2,t3,t4,t5]
        |> List.sum
  in
  d < 0.01

withAlpha a c = let {red,green,blue} = Color.toRgb c in
  Color.rgba red green blue a

endScreen w h color =
  let dur = 2 * second in
  Stage.for dur <| \t ->
    let a = ease easeInQuad float 0 1 dur t in
    filled (withAlpha a color) (rect w h)

w = 500
h = 500

initialState lev =
  { transLeft = lev.maxTrans
  , trans     = Transform2D.identity
  , hasWon    = False
  }

winOverlay = 
  filterMap (\s ->
    if s.hasWon then Just (endScreen w h Color.green)
                else Nothing)
    (Stage.stayForever (group []))
    state

state = state

transes =
  Signal.map (\s -> Move.interpret s.currMove s.preMove)

update lev m s =
  let postMove' = Transform2D.multiply s.postMove (Move.asTransform m)
  in
  if | s.hasWon         -> Nothing
     | s.movesLeft == 1 ->

       if | winning postMove' -> Just {s | hasWon <- True}
          | otherwise  -> Just (initialState lev)

     | otherwise ->
       Just
       { movesLeft = s.movesLeft - 1
       , currMove  = m
       , postMove  = postMove'
       , preMove   = s.postMove
       , hasWon    = False
       }

clickMoveChan : Signal (Maybe Move)
clickMoveChan = Signal.channel Nothing

hoverTransChan : Signal (Maybe Transformation)
hoverTransChan = Signal.channel Nothing

transButtons : Level -> Html
transButtons = 
  let (w, h) = (30, 30)
      r = 0.9 * min w h / 2
      arrow a =
        group
        [ traced (solid Color.black) (segment (0, -r) (0, r))
        , ngon 3 5
        ] |> rotate a
  
      arc a =
        let n = 50
            t = a / n
            f i = (r * cos (t*i), r * sin (t*i))
        in List.map f [0..n-1]

      refLine r a =
        group
        [ traced (solid Color.green) (segment (0, -r) (0, r))
        ] |> rotate a

      transButton t = 
        let art = case t of
              Translate (x, y) -> arrow (atan2 y x)
              Rotation a       -> arc a
              Reflection a     -> refLine a
        in
        div
        [ style [("height", px h), ("width", px w)]
        , onClick (Signal.send clickTransChan (Just t))
        , onMouseEnter (Signal.send hoverTransChan (Just t))
        , onMouseLeave (Signal.send hoverTransChan Nothing)
        ]
        [ fromElement (collage w h art) ]
  in
  flow right << List.map transButton << .availableTrans

-- UTIL
px n = toString n ++ "px"
