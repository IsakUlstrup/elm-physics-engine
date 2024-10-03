module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Engine.Particle as Particle exposing (Particle)
import Engine.Scene as Scene exposing (Scene)
import Engine.Vector exposing (Vector)
import Html exposing (Html)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes



-- SYSTEM


type System
    = Gravity
    | Drag
    | Time


applySystem : Float -> System -> Particle -> Particle
applySystem dt system particle =
    case system of
        Gravity ->
            Particle.applyGravity particle

        Drag ->
            Particle.applyDragForce 0.001 0.003 particle

        Time ->
            Particle.update dt particle



-- MODEL


type alias Model =
    { scene : Scene System
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Scene.empty
            |> Scene.addParticle Particle.new
            |> Scene.addSystem Gravity
            |> Scene.addSystem Time
            |> Scene.addSystem Drag
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


viewGrid : Svg msg
viewGrid =
    let
        viewVerticalLine : Int -> Svg msg
        viewVerticalLine i =
            Svg.line
                [ Svg.Attributes.x1 (i * 10 |> String.fromInt)
                , Svg.Attributes.y1 "-1000"
                , Svg.Attributes.x2 (i * 10 |> String.fromInt)
                , Svg.Attributes.y2 "1000"
                , (if i == 0 then
                    "axis line"

                   else if modBy 10 i == 0 then
                    "thick line"

                   else
                    "line"
                  )
                    |> Svg.Attributes.class
                ]
                []

        viewHorizontalLine : Int -> Svg msg
        viewHorizontalLine i =
            Svg.line
                [ Svg.Attributes.x1 "-1000"
                , Svg.Attributes.y1 (i * 10 |> String.fromInt)
                , Svg.Attributes.x2 "1000"
                , Svg.Attributes.y2 (i * 10 |> String.fromInt)
                , (if i == 0 then
                    "axis line"

                   else if modBy 10 i == 0 then
                    "thick line"

                   else
                    "line"
                  )
                    |> Svg.Attributes.class
                ]
                []
    in
    Svg.g [ Svg.Attributes.class "grid" ]
        [ Svg.g [] (List.range -20 20 |> List.map viewVerticalLine)
        , Svg.g [] (List.range -20 20 |> List.map viewHorizontalLine)
        ]


viewVector : Vector -> Svg msg
viewVector vector =
    Svg.g []
        [ Svg.line
            [ Svg.Attributes.x1 "0"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 (String.fromFloat vector.x)
            , Svg.Attributes.y2 (String.fromFloat -vector.y)
            , Svg.Attributes.stroke "red"
            , Svg.Attributes.strokeLinecap "round"
            ]
            []
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


viewParticle : Particle -> Svg msg
viewParticle particle =
    Svg.g [ particleTransform particle ]
        [ Svg.circle
            [ Svg.Attributes.cx "0"
            , Svg.Attributes.cy "0"
            , Svg.Attributes.r "5"
            ]
            []
        , viewVector particle.velocity
        ]


view : Model -> Html Msg
view model =
    Html.main_ [ Html.Attributes.id "app" ]
        [ Svg.svg
            [ Svg.Attributes.viewBox "-100 -100 200 200"
            , Svg.Attributes.preserveAspectRatio "XmidYmid slice"
            ]
            [ viewGrid
            , Svg.g [] (List.map viewParticle model.scene.particles)
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
