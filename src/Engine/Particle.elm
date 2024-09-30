module Engine.Particle exposing (Particle, new)

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
