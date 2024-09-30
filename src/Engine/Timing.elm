module Engine.Timing exposing (Timing, fixedUpdate, new)


type alias Timing =
    Float


new : Timing
new =
    0


{-| Step time constant in miliseconds
-}
stepTime : Float
stepTime =
    0.02


{-| run state update at fixed interval.

dt is in milliseconds, so just pass in value from onAnimationFrame

-}
fixedUpdate : (Float -> a -> a) -> Float -> ( Timing, a ) -> ( Timing, a )
fixedUpdate f dt ( timing, state ) =
    let
        deltaSeconds : Float
        deltaSeconds =
            dt * 0.001
    in
    if timing + deltaSeconds >= stepTime then
        ( (timing + deltaSeconds) - stepTime
        , f stepTime state
        )
            |> fixedUpdate f 0

    else
        ( timing + deltaSeconds
        , state
        )
