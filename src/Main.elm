module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Engine.Particle as Particle exposing (ForceGenerator, Particle)
import Engine.Scene as Scene exposing (Scene)
import Engine.Spring as Spring
import Engine.Vector as Vector exposing (Vector)
import Html exposing (Html)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes



-- MODEL


type alias Model =
    { scene : Scene
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Scene.empty
            |> Scene.addParticle
                (Particle.new
                    |> Particle.setMass 1
                    |> Particle.setPosition (Vector.new -80 20 0)
                    |> Particle.addForceGenerator
                        (Particle.Spring
                            (Spring.new
                                |> Spring.withLength 100
                                |> Spring.withParticleTarget 1
                                |> Spring.withContractBehaviour
                            )
                        )
                    |> Particle.addForceGenerator
                        Particle.Gravity
                    |> Particle.addForceGenerator
                        (Particle.Drag Scene.airDensity)
                )
            |> Scene.addParticle
                (Particle.new
                    |> Particle.setMass 0
                    |> Particle.setPosition (Vector.new 80 20 0)
                )
        )
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model
                | scene = Scene.tick dt model.scene
              }
            , Cmd.none
            )



-- VIEW


viewVector : String -> String -> Vector -> Svg msg
viewVector color label vector =
    Svg.g []
        [ Svg.line
            [ Svg.Attributes.x1 "0"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 (String.fromFloat vector.x)
            , Svg.Attributes.y2 (String.fromFloat -vector.y)
            , Svg.Attributes.stroke color
            , Svg.Attributes.strokeLinecap "round"
            ]
            []
        , Svg.text_
            [ Svg.Attributes.x (String.fromFloat vector.x)
            , Svg.Attributes.y (String.fromFloat -vector.y)
            , Svg.Attributes.fill color
            , Svg.Attributes.fontSize "0.3rem"
            ]
            [ Svg.text label ]
        ]


particleTransform : Particle -> Svg.Attribute msg
particleTransform particle =
    Svg.Attributes.transform
        ("translate("
            ++ String.fromFloat particle.position.x
            ++ ", "
            ++ String.fromFloat -particle.position.y
            ++ ")"
        )


cameraTransform : Vector -> Svg.Attribute msg
cameraTransform position =
    Svg.Attributes.transform
        ("translate("
            ++ String.fromFloat -position.x
            ++ ", "
            ++ String.fromFloat position.y
            ++ ")"
        )


viewParticle : Dict Int Particle -> ( Int, Particle ) -> Svg msg
viewParticle particles ( _, particle ) =
    let
        viewForceGenerator : ForceGenerator -> Svg msg
        viewForceGenerator generator =
            case generator of
                Particle.Gravity ->
                    viewVector "orange" "g" (Vector.new 0 -10 0)

                Particle.Drag density ->
                    viewVector "green" "d" (Particle.dragForce density particle)

                Particle.Spring spring ->
                    (case spring.target of
                        Spring.Particle id ->
                            Dict.get id particles
                                |> Maybe.map
                                    (\targetParticle ->
                                        Spring.springForce particle.position targetParticle.position spring
                                    )
                                |> Maybe.withDefault Vector.zero

                        Spring.Position position ->
                            Spring.springForce particle.position position spring
                    )
                        |> viewVector "yellow" "s"
    in
    Svg.g [ particleTransform particle ]
        [ Svg.circle
            [ Svg.Attributes.cx "0"
            , Svg.Attributes.cy "0"
            , Svg.Attributes.r "5"
            ]
            []
        , Svg.g [] (particle.forceGenerators |> List.map viewForceGenerator)
        , viewVector "red" "v" particle.velocity
        ]


view : Model -> Html Msg
view model =
    let
        particlePosition : Vector
        particlePosition =
            model.scene.particles
                |> Dict.get 0
                |> Maybe.map .position
                |> Maybe.withDefault Vector.zero
    in
    Html.main_ [ Html.Attributes.id "app" ]
        [ Svg.svg
            [ Svg.Attributes.viewBox "-150 -150 300 300"
            , Svg.Attributes.preserveAspectRatio "XmidYmid slice"
            ]
            [ Svg.g
                [ Svg.Attributes.class "camera"
                , cameraTransform particlePosition
                ]
                [ Svg.g [] (model.scene.particles |> Dict.toList |> List.map (viewParticle model.scene.particles))
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta Tick



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
