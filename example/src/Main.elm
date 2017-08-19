module Main exposing (..)

import Html exposing (Html, text, div, img, button, input)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick, onInput)
import Dict exposing (Dict)
import Debounce
import Time


---- MODEL ----


type alias Model =
    Dict String String


init : ( Model, Cmd Msg )
init =
    ( Dict.fromList
        [ ( "a", "" )
        , ( "b", "" )
        , ( "c", "" )
        ]
    , Cmd.none
    )


delay =
    Time.millisecond * 500



---- UPDATE ----


type Msg
    = Deb String String
    | Input String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Deb value key ->
            let
                newModel =
                    Dict.insert key value model
            in
                ( newModel, Cmd.none )

        Input key value ->
            ( model, Debounce.debounce delay key (Deb value) )



---- VIEW ----


viewInput : ( String, String ) -> Html Msg
viewInput ( k, v ) =
    div []
        [ input [ onInput (Input k) ] []
        , div [] [ text v ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , div [] [ text "Your Elm App is working!" ]
        , div [] (List.map viewInput (Dict.toList model))
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
