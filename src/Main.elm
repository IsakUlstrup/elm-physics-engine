module Main exposing (Model, Msg, main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes



-- MODEL


type alias Model =
    ()


init : () -> ( Model, Cmd Msg )
init _ =
    ( (), Cmd.none )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



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
        [ Svg.g [] (List.range -100 100 |> List.map viewVerticalLine)
        , Svg.g [] (List.range -100 100 |> List.map viewHorizontalLine)
        ]


view : Model -> Html Msg
view _ =
    Html.main_ [ Html.Attributes.id "app" ]
        [ Svg.svg [ Svg.Attributes.viewBox "-500 -500 1000 1000", Svg.Attributes.preserveAspectRatio "XmidYmid meet" ]
            [ viewGrid
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
