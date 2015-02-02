module Level where

import Util(..)
import Move
import Move(Move(..))
import Graphics.Element(..)
import Graphics.Collage(..)
import Transform2D(Transform2D)
import Transform2D
import Html(..)
import Html
import Html.Attributes(style)
import Html.Events(..)
import Native.IsomUtil
import Text
import Color
import Time (second)
import Time
import Stage
import Stage.Infix(..)
import List
import Easing (ease, float, easeInQuad)
import Signal
import Maybe

type alias Level =
  { availableMoves : List Move
  , maxMoves       : Int
  , initialTrans   : Transform2D
  , goal           : Transform2D
  }

type alias GameState =
  { movesLeft : Int
  , currMove  : Maybe Move
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

winScreen w h color =
  let dur = 2 * second
      sty a =
        { typeface = ["Futura", "sans-serif"]
        , height   = Just 100
        , color    = withAlpha a winTextColor
        , bold     = False
        , italic   = False
        , line     = Nothing
        }
      fade =
        Stage.sustain <| Stage.for dur <| \t ->
          let a = ease easeInQuad float 0 1 dur t in
          group
          [ filled (withAlpha a color) (rect w h)
          , formText (sty a) "You\nWin!"
          ]
  in
  Stage.stayFor (1 * second) (group [])
  <> fade

w = 500
h = 500

initialState : Level -> GameState
initialState lev =
  { movesLeft = lev.maxMoves
  , currMove  = Nothing
  , postMove  = lev.initialTrans
  , preMove   = lev.initialTrans
  , hasWon    = False
  }

winOverlay = 
  filterMap (\s ->
    if s.hasWon then Just (winScreen w h fadeColor)
                else Nothing)
    (Stage.stayForever (group []))

transes lev =
  let init = Stage.stayForever lev.initialTrans in
  filterMap (\s ->
    case s.currMove of
      Nothing -> Just init
      Just m  -> Just <| Stage.sustain <| Move.interpret m s.preMove)
    init

withBorder b c e =
  color c
    (container (widthOf e + (2 * b)) (heightOf e + (2 * b)) middle e)

update lev m s =
  let postMove' = Transform2D.multiply (Move.asTransform m) s.postMove 
  in
  if | s.hasWon              -> Nothing
     | winning lev postMove' ->
       Just 
       { hasWon    = True
       , movesLeft = s.movesLeft - 1
       , currMove  = Just m
       , postMove  = postMove'
       , preMove   = s.postMove
       }
     | s.movesLeft == 1      -> Just (initialState lev)
     | otherwise             ->
       Just
       { movesLeft = s.movesLeft - 1
       , currMove  = Just m
       , postMove  = postMove'
       , preMove   = s.postMove
       , hasWon    = False
       }

formText sty =
  Text.fromString
  >> Text.style sty 
  >> Text.centered
  >> toForm

defaultStyle =
  { height   = Just 14
  , color    = Color.black
  , typeface = ["Futura", "sans-serif"]
  , bold     = True
  , italic   = False
  , line     = Nothing
  }

anR color =
  Text.style {defaultStyle | height <- Just 50, color <- color} (Text.fromString "R")
  |> Text.centered
  |> toForm

goalGhost lev = anR ghostColor |> sing |> groupTransform lev.goal

defImage = anR playerColor

run lev =
  let state       = filterFold (\mm s -> mm `Maybe.andThen` \m -> update lev m s) (initialState lev) moveClicks
      butts       = Html.toElement w 100 (transButtons lev)
      ghost       = goalGhost lev
      movesLeft s =
        group
        [ filled fadeColor (circle 20)
        , formText {defaultStyle | height <- Just 20} (toString s.movesLeft)
        ]
  in
  Signal.map3 (\trans overlay s ->
    flow down
    [ collage w h
      [ move (-200, 200) (movesLeft s)
      , groupTransform trans [defImage]
      , ghost
      , overlay
      ]
      |> color backgroundColor
      |> withBorder 3 borderColor
    , butts
    ])
    (Stage.run (transes lev state) (Time.every 30))
    (Stage.run (winOverlay state) (Time.every 30))
    state

moveClicks = Signal.subscribe clickMoveChan
clickMoveChan : Signal.Channel (Maybe Move)
clickMoveChan = Signal.channel Nothing

hoverMoveChan : Signal.Channel (Maybe Move)
hoverMoveChan = Signal.channel Nothing

transButtons : Level -> Html
transButtons = 
  let (w, h) = (80, 80)
      r = 0.9 * min w h / 2
      thickness = 5
      thick c = let sty = solid c in {sty | width <- thickness}
      arrow a =
        group
        [ traced (thick Color.black) (segment (0, -r) (0, r))
        , filled Color.black (ngon 3 5)
        ] |> rotate a
  
      arc a =
        let n = 50
            t = a / n
            f i = (r * cos (t*i), r * sin (t*i))
        in List.map f [0..n-1]
  
      rotArc a =
        group
        [ traced (thick rotateArcColor) (arc a)
        , ngon 3 10
          |> filled rotateArcColor
          |> rotate (-pi /6)
          |> moveX r
          |> sing |> groupTransform (Transform2D.rotation a) -- why doesn't rotate work...
        ]

      refLine a =
        group
        [ traced (thick Color.green) (segment (-r, 0) (r, 0))
        ] |> rotate a

      transButton t = 
        let art = case t of
              Translate (x, y) -> arrow (atan2 y x)
              Rotation a       -> rotArc a
              Reflection a     -> refLine a
        in
        div
        [ style
          [ ("height", px h), ("width", px w)
          , ("backgroundColor", colorStr buttonBackgroundColor)
          , ("border", "1px solid black")
          , ("cursor", "pointer")
          , ("display", "inline-block")
          , ("margin", "3px")
          ]
        , onClick (Signal.send clickMoveChan (Just t))
        , onMouseEnter (Signal.send hoverMoveChan (Just t))
        , onMouseLeave (Signal.send hoverMoveChan Nothing)
        ]
        [ fromElement (collage w h [art]) ]
  in
  div
  [ style
    [ ("textAlign", "center")
    ]
  ] 
  << List.map transButton << .availableMoves

-- UTIL
colorStr c =
  let {red,green,blue,alpha} = Color.toRgb c in
  "rgba(" ++
  toString red ++ "," ++ 
  toString green ++ "," ++
  toString blue ++ "," ++
  toString alpha ++ ")"

px n = toString n ++ "px"
sing x = [x]

-- colors
backgroundColor  = Color.rgb 223 223 223
borderColor      = Color.rgb 188 188 188
playerColor      = Color.rgb 0 119 219
ghostColor       = Color.rgb 204 0 51
fadeColor        = Color.rgb 254 204 9
winTextColor     = Color.rgb 75 91 110
buttonBackgroundColor = winTextColor
rotateArcColor = fadeColor
movesLeftTextColor = winTextColor
-- reflLineColor =