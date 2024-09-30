module Engine.Particle exposing (Particle, new)

import Engine.Vector as Vector exposing (Vector)


type alias Particle =
    { position : Vector
    , velocity : Vector
    , acceleration : Vector
    }


new : Vector -> Particle
new position =
    Particle position Vector.zero Vector.zero
