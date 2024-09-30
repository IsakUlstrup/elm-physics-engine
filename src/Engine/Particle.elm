module Engine.Particle exposing (Particle, new, update)

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
    Particle position Vector.zero Vector.zero 0.995 0.5


update : Float -> Particle -> Particle
update dt particle =
    { particle
        | position =
            particle.position
                |> Vector.addScaledVector dt particle.velocity
        , velocity =
            particle.velocity
                |> Vector.addScaledVector dt (Vector.addScaledVector particle.inverseMass particle.acceleration particle.velocity)
                |> Vector.scale (particle.damping ^ dt)
    }
