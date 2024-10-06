module Engine.Particle exposing (Particle, applyForce, applyGravity, dragForce, new, setMass, setPosition, update)

import Engine.Vector as Vector exposing (Vector)


type alias Particle =
    { position : Vector
    , velocity : Vector
    , acceleration : Vector
    , damping : Float
    , inverseMass : Float
    }


particleRadius : number
particleRadius =
    1


dragCoefficient : Float
dragCoefficient =
    0.47


frontalArea : Float
frontalArea =
    pi * particleRadius * particleRadius


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


{-| Calculate particle drag force
-}
dragForce : Float -> Particle -> Vector
dragForce density particle =
    let
        velocity : Float
        velocity =
            Vector.magnitude particle.velocity

        force : Float
        force =
            0.5 * density * velocity ^ 2 * dragCoefficient * frontalArea
    in
    particle.velocity
        |> Vector.normalize
        |> Maybe.withDefault Vector.zero
        |> Vector.scale -force


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
