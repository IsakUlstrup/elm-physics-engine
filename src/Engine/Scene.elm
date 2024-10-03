module Engine.Scene exposing (Scene, addParticle, addSystem, empty, tick)

import Engine.Particle exposing (Particle)
import Engine.Timing exposing (Timing)


{-| A scene holds particles and logic systems
-}
type alias Scene a =
    { particles : List Particle
    , timing : Timing
    , systems : List a
    }


empty : Scene a
empty =
    Scene [] Engine.Timing.new []


addParticle : Particle -> Scene a -> Scene a
addParticle particle scene =
    { scene | particles = particle :: scene.particles }


addSystem : a -> Scene a -> Scene a
addSystem system scene =
    { scene | systems = system :: scene.systems }


applySystem : (Particle -> Particle) -> List Particle -> List Particle
applySystem f particles =
    List.map f particles


applySystems : List a -> (Float -> a -> Particle -> Particle) -> Float -> List Particle -> List Particle
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
