module Engine.Particle exposing (Particle, applyForce, applyGravity, new, update)

import Engine.Vector as Vector exposing (Vector)


type alias Particle =
    { position : Vector
    , velocity : Vector
    , acceleration : Vector
    , damping : Float
    , inverseMass : Float
    }


new : Vector -> Particle
new position =
    Particle position Vector.zero Vector.zero 0.995 1000


applyGravity : Particle -> Particle
applyGravity particle =
    { particle
        | acceleration =
            particle.acceleration
                |> Vector.add (Vector.new 0 -10 0)
    }


applyForce : Vector -> Particle -> Particle
applyForce force particle =
    { particle
        | acceleration =
            particle.acceleration
                |> Vector.addScaledVector particle.inverseMass force
    }


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
        , acceleration = Vector.zero
    }
