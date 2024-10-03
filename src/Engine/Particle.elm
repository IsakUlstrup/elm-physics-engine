module Engine.Particle exposing (Particle, applyDragForce, applyForce, applyGravity, new, setMass, setPosition, update)

import Engine.Vector as Vector exposing (Vector)


type alias Particle =
    { position : Vector
    , velocity : Vector
    , acceleration : Vector
    , damping : Float
    , inverseMass : Float
    }


new : Particle
new =
    Particle Vector.zero Vector.zero Vector.zero 0.995 10


setPosition : Vector -> Particle -> Particle
setPosition position particle =
    { particle | position = position }


setMass : Float -> Particle -> Particle
setMass mass particle =
    if max 0 mass == 0 then
        { particle | inverseMass = 0 }

    else
        { particle | inverseMass = 1 / mass }


{-| Apply constant gravity force, ignoring particle mass

If mass is infinite, no force will be applied

-}
applyGravity : Particle -> Particle
applyGravity particle =
    if particle.inverseMass /= 0 then
        { particle
            | acceleration =
                particle.acceleration
                    |> Vector.add (Vector.new 0 -10 0)
        }

    else
        particle


{-| Apply force to particle, the force will be scaled based on particle mass

If mass is infinite, no force will be applied

-}
applyForce : Vector -> Particle -> Particle
applyForce force particle =
    { particle
        | acceleration =
            particle.acceleration
                |> Vector.addScaledVector particle.inverseMass force
    }


applyDragForce : Float -> Float -> Particle -> Particle
applyDragForce k1 k2 particle =
    let
        velocity : Float
        velocity =
            Vector.magnitude particle.velocity

        dragCoeff : Float
        dragCoeff =
            k1 * velocity + k2 * velocity * velocity

        force : Vector
        force =
            particle.velocity
                |> Vector.normalize
                |> Maybe.withDefault Vector.zero
                |> Vector.scale -dragCoeff
    in
    applyForce force particle


{-| Integrate time step in seconds
-}
update : Float -> Particle -> Particle
update dt particle =
    { particle
        | position =
            particle.position
                |> Vector.addScaledVector dt particle.velocity
        , velocity =
            particle.velocity
                |> Vector.addScaledVector dt particle.acceleration
                |> Vector.scale (particle.damping ^ dt)
        , acceleration =
            Vector.zero
    }
