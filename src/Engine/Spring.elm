module Engine.Spring exposing
    ( Spring
    , new
    , springForce
    , withContractBehaviour
    , withExtendBehaviour
    , withLength
    , withParticleTarget
    , withPositionTarget
    , withRate
    )

import Engine.Vector as Vector exposing (Vector)


type alias Spring =
    { target : SpringTarget
    , length : Float
    , behaviour : Length
    , rate : Float
    }


{-| Spring target, used to determine actual spring length

Particle: find particle position
Position: Fixed to a point

-}
type SpringTarget
    = Particle Int
    | Position Vector


{-| Spring length

Extend: Will only apply force if compressed below length

Contract: Will only apply force if extended beyond length (bungee)

Both: Will apply force to keep distance at exact length

-}
type Length
    = Extend
    | Contract
    | Both


new : Spring
new =
    Spring (Position Vector.zero) 0 Both 0


withParticleTarget : Int -> Spring -> Spring
withParticleTarget id spring =
    { spring | target = Particle id }


withPositionTarget : Vector -> Spring -> Spring
withPositionTarget position spring =
    { spring | target = Position position }


withExtendBehaviour : Spring -> Spring
withExtendBehaviour spring =
    { spring | behaviour = Extend }


withContractBehaviour : Spring -> Spring
withContractBehaviour spring =
    { spring | behaviour = Contract }


withLength : Float -> Spring -> Spring
withLength length spring =
    { spring | length = length }


withRate : Float -> Spring -> Spring
withRate rate spring =
    { spring | rate = rate }


springForce : Vector -> Vector -> Spring -> Vector
springForce position targetPosition spring =
    let
        distance : Vector
        distance =
            Vector.subtract position targetPosition

        magnitude =
            Vector.magnitude distance - spring.length

        force =
            distance
                |> Vector.normalize
                |> Maybe.withDefault Vector.zero
                |> Vector.scale (magnitude * spring.rate)
    in
    case spring.behaviour of
        Extend ->
            if magnitude < 0 then
                force

            else
                Vector.zero

        Contract ->
            if magnitude > 0 then
                force

            else
                Vector.zero

        Both ->
            force
