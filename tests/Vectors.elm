module Vectors exposing (suite)

-- import Fuzz

import Engine.Vector as Vector exposing (Vector)
import Expect
import Test exposing (Test, describe, test)


similarVector : Vector -> Vector -> Expect.Expectation
similarVector target vector =
    let
        precision : Float
        precision =
            0.001
    in
    Expect.all
        [ .x >> Expect.within (Expect.Absolute precision) target.x
        , .y >> Expect.within (Expect.Absolute precision) target.y
        , .z >> Expect.within (Expect.Absolute precision) target.z
        ]
        vector


suite : Test
suite =
    describe "Vector"
        [ test "Magnitude of vector 2 0 0 should be 2" <|
            \_ ->
                Vector.new 2 0 0
                    |> Vector.magnitude
                    |> Expect.within (Expect.Absolute 0.001) 2
        , test "Magnitude of vector -2 0 0 should be 2" <|
            \_ ->
                Vector.new -2 0 0
                    |> Vector.magnitude
                    |> Expect.within (Expect.Absolute 0.001) 2
        , test "Magnitude of vector 1 1 0 should be 1.414213562373095" <|
            \_ ->
                Vector.new 1 1 0
                    |> Vector.magnitude
                    |> Expect.within (Expect.Absolute 0.001) 1.414213562373095
        , test "Normalize zero vector" <|
            \_ ->
                Vector.zero
                    |> Vector.normalize
                    |> Expect.equal Nothing
        , test "Normalize non-zero vector" <|
            \_ ->
                Vector.new 20 -42.1 12.345
                    |> Vector.normalize
                    |> Maybe.map Vector.magnitude
                    |> Maybe.withDefault 0
                    |> Expect.within (Expect.Absolute 0.001) 1
        , test "Invert vector" <|
            \_ ->
                Vector.new -2 0 0
                    |> Vector.invert
                    |> similarVector (Vector.new 2 0 0)
        , test "Invert zero vector" <|
            \_ ->
                Vector.zero
                    |> Vector.invert
                    |> similarVector Vector.zero
        , test "Add two vectors together" <|
            \_ ->
                Vector.new 1 2 3
                    |> Vector.add (Vector.new 3 2 1)
                    |> similarVector (Vector.new 4 4 4)
        , test "Subtract vector" <|
            \_ ->
                Vector.new 1 2 3
                    |> Vector.subtract (Vector.new 3 2 1)
                    |> similarVector (Vector.new -2 0 2)
        , test "Add scaled vector" <|
            \_ ->
                Vector.zero
                    |> Vector.addScaledVector 3 (Vector.new 3 2 1)
                    |> similarVector (Vector.new 9 6 3)
        , test "Get component product" <|
            \_ ->
                let
                    a : Vector
                    a =
                        Vector.new -11 21 -10

                    b : Vector
                    b =
                        Vector.new -4 9 1
                in
                Vector.componentProduct a b
                    |> similarVector (Vector.new (a.x * b.x) (a.y * b.y) (a.z * b.z))
        , test "Get dot product" <|
            \_ ->
                let
                    a : Vector
                    a =
                        Vector.new -10 2 0

                    b : Vector
                    b =
                        Vector.new 4 9 1
                in
                Vector.dot a b
                    |> Expect.within (Expect.Absolute 0.001)
                        (a.x * b.x + a.y * b.y + a.z * b.z)
        , test "Get cross product" <|
            \_ ->
                let
                    a : Vector
                    a =
                        Vector.new 3 2 7

                    b : Vector
                    b =
                        Vector.new 1 4 2
                in
                Vector.cross a b
                    |> similarVector
                        (Vector.new
                            (a.y * b.z - a.z * b.y)
                            (a.z * b.x - a.x * b.z)
                            (a.x * b.y - a.y * b.x)
                        )
        ]
