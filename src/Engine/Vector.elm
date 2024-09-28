module Engine.Vector exposing
    ( Vector
    , add
    , addScaledVector
    , componentProduct
    , cross
    , dot
    , invert
    , magnitude
    , new
    , normalize
    , subtract
    , zero
    )


type alias Vector =
    { x : Float
    , y : Float
    , z : Float
    }


new : Float -> Float -> Float -> Vector
new x y z =
    Vector x y z


zero : Vector
zero =
    new 0 0 0


magnitude : Vector -> Float
magnitude vector =
    vector
        |> magnitudeSquared
        |> sqrt


magnitudeSquared : Vector -> Float
magnitudeSquared vector =
    (vector.x * vector.x) + (vector.y * vector.y) + (vector.z * vector.z)


componentProduct : Vector -> Vector -> Vector
componentProduct v1 v2 =
    new (v1.x * v2.x) (v1.y * v2.y) (v1.z * v2.z)


{-| dot product, also called scalar product
-}
dot : Vector -> Vector -> Float
dot v1 v2 =
    (v1.x * v2.x) + (v1.y * v2.y) + (v1.z * v2.z)


{-| cross product, also called vector product
-}
cross : Vector -> Vector -> Vector
cross v1 v2 =
    new
        (v1.y * v2.z - v1.z * v2.y)
        (v1.z * v2.x - v1.x * v2.z)
        (v1.x * v2.y - v1.y * v2.x)


scale : Float -> Vector -> Vector
scale scalar vector =
    new (vector.x * scalar) (vector.y * scalar) (vector.z * scalar)


normalize : Vector -> Maybe Vector
normalize vector =
    let
        mag : Float
        mag =
            magnitude vector
    in
    if mag > 0 then
        Just (scale (1 / mag) vector)

    else
        Nothing


invert : Vector -> Vector
invert vector =
    scale -1 vector


add : Vector -> Vector -> Vector
add v1 v2 =
    new (v1.x + v2.x) (v1.y + v2.y) (v1.z + v2.z)


addScaledVector : Float -> Vector -> Vector -> Vector
addScaledVector vectorScale addVector vector =
    addVector
        |> scale vectorScale
        |> add vector


subtract : Vector -> Vector -> Vector
subtract subVector vector =
    new (vector.x - subVector.x) (vector.y - subVector.y) (vector.z - subVector.z)
