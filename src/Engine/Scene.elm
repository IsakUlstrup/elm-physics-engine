module Engine.Scene exposing
    ( Scene
    , addParticle
    , addSpring
    , addSystem
    , empty
    , tick
    )

import Dict exposing (Dict)
import Engine.Particle as Particle exposing (Particle)
import Engine.Timing exposing (Timing)
import Engine.Vector as Vector exposing (Vector)


{-| A scene holds particles and logic systems
-}
type alias Scene a =
    { particles : Dict Int Particle
    , springs : List Spring
    , idCounter : Int
    , timing : Timing
    , systems : List a
    }


empty : Scene a
empty =
    Scene Dict.empty [] 0 Engine.Timing.new []


addParticle : Particle -> Scene a -> Scene a
addParticle particle scene =
    { scene
        | particles = Dict.insert scene.idCounter particle scene.particles
        , idCounter = scene.idCounter + 1
    }


addSystem : a -> Scene a -> Scene a
addSystem system scene =
    { scene | systems = system :: scene.systems }


applySystem : (Particle -> Particle) -> Dict Int Particle -> Dict Int Particle
applySystem f particles =
    Dict.map (\_ value -> f value) particles


applySystems : List a -> (Float -> a -> Particle -> Particle) -> Float -> Dict Int Particle -> Dict Int Particle
applySystems systems f dt particles =
    List.foldl (\s -> applySystem (f dt s)) particles systems


tick : (Float -> a -> Particle -> Particle) -> Float -> Scene a -> Scene a
tick runSystem dt scene =
    let
        ( newTimer, newParticles ) =
            Engine.Timing.fixedUpdate
                (\d ps ->
                    ps
                        |> applySystems scene.systems runSystem d
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


addSpring : Int -> Int -> Float -> Scene a -> Scene a
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
