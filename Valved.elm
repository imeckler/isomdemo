module Valved
  ( valved
  , valvedWithState
  ) where

import Signal

type AUpdate a = Release | Accum a

valvedInternal :
  (a -> s -> s)
  -> Signal a -> Signal ()
  -> s
  -> Signal {emitted : s, internal : s}
valvedInternal accum updates releases s0 =
  Signal.merge
    (Signal.map (\_ -> Release) releases)
    (Signal.map Accum updates)
  |>
  Signal.foldp (\u {emitted, internal} -> case u of
    Release -> {emitted = internal, internal = s0}
    Accum x -> {emitted = emitted, internal = accum x internal})
      {emitted = s0, internal = s0}

valvedWithState :
  (a -> s -> s)
  -> Signal a -> Signal ()
  -> s
  -> {emitted : Signal s, internal : Signal s}
valvedWithState accum updates releases s0 =
  let ss = valvedInternal accum updates releases s0
  in
  { emitted = Signal.map .emitted (Signal.sampleOn releases ss)
  , internal = Signal.map .internal ss
  }

valved :
  (a -> s -> s)
  -> Signal a -> Signal ()
  -> s
  -> Signal s
valved accum updates releases s0 =
  valvedInternal accum updates releases s0
  |> Signal.sampleOn releases
  |> Signal.map .emitted

