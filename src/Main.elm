module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Engine.Particle as Particle exposing (Particle)
import Engine.Vector as Vector
import Html exposing (Html)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes



-- MODEL


type alias Model =
    Particle


init : () -> ( Model, Cmd Msg )
init _ =
    ( Particle.new Vector.zero
        |> Particle.applyForce (Vector.new 12 0 0)
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( model
                |> Particle.applyGravity
                |> Particle.update dt
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


particleTransform : Particle -> Svg.Attribute msg
particleTransform particle =
    Svg.Attributes.transform ("translate(" ++ String.fromFloat particle.position.x ++ ", " ++ String.fromFloat -particle.position.y ++ ")")


viewParticle : Particle -> Svg msg
viewParticle particle =
    Svg.g [ particleTransform particle ]
        [ Svg.circle
            [ Svg.Attributes.cx "0"
            , Svg.Attributes.cy "0"
            , Svg.Attributes.r "5"
            ]
            []
        ]


view : Model -> Html Msg
view model =
    Html.main_ [ Html.Attributes.id "app" ]
        [ Svg.svg [ Svg.Attributes.viewBox "-250 -250 500 500", Svg.Attributes.preserveAspectRatio "XmidYmid meet" ]
            [ viewGrid
            , viewParticle model
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta ((\dt -> dt / 1000) >> Tick)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
