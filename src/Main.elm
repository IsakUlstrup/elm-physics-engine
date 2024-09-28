module Main exposing (Model, Msg, main)

import Browser
import Engine.Vector as Vector exposing (Vector)
import Html exposing (Html)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes



-- MODEL


type alias Model =
    List ( String, Vector )


init : () -> ( Model, Cmd Msg )
init _ =
    ( [ ( "red", Vector.new 5 3 0 )
      , ( "green", Vector.new 2.5 -2 0 )
      , ( "orange", Vector.cross (Vector.new 5 3 0) (Vector.new 2.5 -2 0) )
      ]
    , Cmd.none
    )



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
        [ Svg.g [] (List.range -20 20 |> List.map viewVerticalLine)
        , Svg.g [] (List.range -20 20 |> List.map viewHorizontalLine)
        ]


viewVector : ( String, Vector ) -> Svg msg
viewVector ( color, vector ) =
    Svg.g []
        [ Svg.line
            [ Svg.Attributes.x1 "0"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 (String.fromFloat (vector.x * 10))
            , Svg.Attributes.y2 (String.fromFloat -(vector.y * 10))
            , Svg.Attributes.stroke color
            , Svg.Attributes.strokeLinecap "round"
            ]
            []
        ]


view : Model -> Html Msg
view model =
    Html.main_ [ Html.Attributes.id "app" ]
        [ Svg.svg [ Svg.Attributes.viewBox "-100 -100 200 200", Svg.Attributes.preserveAspectRatio "XmidYmid meet" ]
            [ viewGrid
            , Svg.g [] (List.map viewVector model)
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
