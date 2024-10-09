module Main exposing (Model, Msg, System, main)

import Browser
import Browser.Events
import Dict
import Engine.Particle as Particle exposing (Particle)
import Engine.Scene as Scene exposing (Scene)
import Engine.Vector as Vector exposing (Vector)
import Html exposing (Html)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes


airDensity : Float
airDensity =
    0.00765



-- SYSTEM


type System
    = Gravity
    | Drag
    | Time
    | Buoyancy


applySystem : Float -> System -> Particle -> Particle
applySystem dt system particle =
    case system of
        Gravity ->
            Particle.applyGravity particle

        Drag ->
            Particle.applyForce (Particle.dragForce airDensity particle) particle

        Time ->
            Particle.update dt particle

        Buoyancy ->
            let
                submerged : Float
                submerged =
                    min 0 particle.position.y

                force : Vector
                force =
                    Vector.new 0 1 0
                        |> Vector.scale -submerged
            in
            Particle.applyForce force particle



-- MODEL


type alias Model =
    { scene : Scene System
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Scene.empty
            |> Scene.addParticle
                (Particle.new
                    |> Particle.setMass 1
                    |> Particle.setPosition (Vector.new -80 20 0)
                    |> Particle.applyForce (Vector.new 300 0 0)
                )
            |> Scene.addSystem Gravity
            |> Scene.addSystem Time
            |> Scene.addSystem Drag
         -- |> Scene.addSystem Buoyancy
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
                | scene = Scene.tick applySystem dt model.scene
              }
            , Cmd.none
            )



-- VIEW


viewGrid : Vector -> Svg msg
viewGrid pos =
    let
        verticalLine : Int -> Svg msg
        verticalLine x =
            Svg.line
                [ Svg.Attributes.x1 (String.fromInt x)
                , Svg.Attributes.y1 "-500"
                , Svg.Attributes.x2 (String.fromInt x)
                , Svg.Attributes.y2 "500"
                ]
                []

        horizontalLine : Int -> Svg msg
        horizontalLine y =
            Svg.line
                [ Svg.Attributes.x1 "-500"
                , Svg.Attributes.y1 (String.fromInt y)
                , Svg.Attributes.x2 "500"
                , Svg.Attributes.y2 (String.fromInt y)
                ]
                []

        spacing : number
        spacing =
            100
    in
    Svg.g
        [ Svg.Attributes.stroke "#262626"
        , Svg.Attributes.strokeWidth "0.2"
        , Svg.Attributes.transform
            ("translate("
                ++ String.fromInt (modBy spacing (round -pos.x))
                ++ ", "
                ++ String.fromInt (modBy spacing (round pos.y))
                ++ ")"
            )
        ]
        (List.range -2 2
            |> List.concatMap
                (\i ->
                    [ verticalLine (i * spacing)
                    , horizontalLine (i * spacing)
                    ]
                )
        )


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


viewParticle : ( Int, Particle ) -> Svg msg
viewParticle ( _, particle ) =
    Svg.g [ particleTransform particle ]
        [ Svg.circle
            [ Svg.Attributes.cx "0"
            , Svg.Attributes.cy "0"
            , Svg.Attributes.r "5"
            ]
            []
        , viewVector "red" "v" particle.velocity
        , viewVector "green" "d" (Particle.dragForce airDensity particle)
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
            [ viewGrid particlePosition
            , Svg.g
                [ Svg.Attributes.class "camera"
                , cameraTransform particlePosition
                ]
                [ Svg.g [] (model.scene.particles |> Dict.toList |> List.map viewParticle)
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
