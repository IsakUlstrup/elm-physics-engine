module Engine.Rigidbody exposing (Rigidbody)

import Engine.Particle exposing (ForceGenerator)
import Engine.Quaternion exposing (Quaternion)
import Engine.Vector exposing (Vector)


type alias Rigidbody =
    { position : Vector
    , velocity : Vector
    , acceleration : Vector
    , orientation : Quaternion
    , inverseMass : Float
    , forceGenerators : List ForceGenerator
    }
