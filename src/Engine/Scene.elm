module Engine.Scene exposing
    ( Scene
    , addParticle
    , addSpring
    , airDensity
    , empty
    , tick
    )

import Dict exposing (Dict)
import Engine.Particle as Particle exposing (Particle)
import Engine.Timing exposing (Timing)
import Engine.Vector as Vector exposing (Vector)


{-| A scene holds particles and logic systems
-}
type alias Scene =
    { particles : Dict Int Particle
    , springs : List Spring
    , idCounter : Int
    , timing : Timing
    }


empty : Scene
empty =
    Scene Dict.empty [] 0 Engine.Timing.new


addParticle : Particle -> Scene -> Scene
addParticle particle scene =
    { scene
        | particles = Dict.insert scene.idCounter particle scene.particles
        , idCounter = scene.idCounter + 1
    }


airDensity : Float
airDensity =
    0.00765


applyGravity : Dict Int Particle -> Dict Int Particle
applyGravity particles =
    Dict.map (\_ particle -> Particle.applyGravity particle) particles


applyDrag : Dict Int Particle -> Dict Int Particle
applyDrag particles =
    Dict.map (\_ particle -> Particle.applyForce (Particle.dragForce airDensity particle) particle) particles


particleUpdate : Float -> Dict Int Particle -> Dict Int Particle
particleUpdate dt particles =
    Dict.map (\_ particle -> Particle.update dt particle) particles


tick : Float -> Scene -> Scene
tick dt scene =
    let
        ( newTimer, newParticles ) =
            Engine.Timing.fixedUpdate
                (\d ps ->
                    ps
                        |> applyGravity
                        |> applyDrag
                        |> particleUpdate d
                        |> springForces scene.springs
                )
                dt
                ( scene.timing, scene.particles )
    in
    { scene
        | timing = newTimer
        , particles = newParticles
    }



-- SPRING


type alias Spring =
    { particle1 : Int
    , particle2 : Int
    , length : Float
    , rate : Float
    }


addSpring : Int -> Int -> Float -> Scene -> Scene
addSpring particle1 particle2 rate scene =
    case ( Dict.get particle1 scene.particles, Dict.get particle2 scene.particles ) of
        ( Just p1, Just p2 ) ->
            let
                distance : Float
                distance =
                    Vector.subtract p1.position p2.position
                        |> Vector.magnitude
            in
            { scene | springs = Spring particle1 particle2 distance rate :: scene.springs }

        _ ->
            scene


applySpring : Spring -> Dict Int Particle -> Dict Int Particle
applySpring spring particles =
    case ( Dict.get spring.particle1 particles, Dict.get spring.particle2 particles ) of
        ( Just p1, Just p2 ) ->
            let
                distance : Vector
                distance =
                    Vector.subtract p1.position p2.position

                magnitude =
                    (Vector.magnitude distance - spring.length) * spring.rate

                force =
                    distance
                        |> Vector.normalize
                        |> Maybe.withDefault Vector.zero
                        |> Vector.scale magnitude
            in
            -- Do spring stuff
            particles
                |> Dict.update spring.particle1 (Maybe.map (Particle.applyForce force))
                |> Dict.update spring.particle2 (Maybe.map (Particle.applyForce (Vector.scale -1 force)))

        _ ->
            particles


springForces : List Spring -> Dict Int Particle -> Dict Int Particle
springForces springs particles =
    List.foldl applySpring particles springs
