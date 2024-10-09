module Engine.Scene exposing (Scene, addParticle, addSystem, empty, tick)

import Dict exposing (Dict)
import Engine.Particle exposing (Particle)
import Engine.Timing exposing (Timing)


{-| A scene holds particles and logic systems
-}
type alias Scene a =
    { particles : Dict Int Particle
    , idCounter : Int
    , timing : Timing
    , systems : List a
    }


empty : Scene a
empty =
    Scene Dict.empty 0 Engine.Timing.new []


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
                (applySystems scene.systems runSystem)
                dt
                ( scene.timing, scene.particles )
    in
    { scene
        | timing = newTimer
        , particles = newParticles
    }



-- scene
