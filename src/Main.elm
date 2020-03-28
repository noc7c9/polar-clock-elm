module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import PolarClock exposing (polarClock)
import Task
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type TimeValue
    = RealTime Time.Posix
    | ManualTime Int


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    , debugOffset : Int
    , debugPaused : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { zone = Time.utc
            , time = Time.millisToPosix 0
            , debugOffset = 0
            , debugPaused = False
            }

        cmd =
            Task.perform AdjustTimeZone Time.here
    in
    ( model, cmd )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | DebugOffsetIncrement Int
    | DebugOffsetDecrement Int
    | DebugOffsetReset
    | DebugTogglePause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updatedModel =
            case msg of
                Tick newTime ->
                    { model | time = newTime }

                AdjustTimeZone newZone ->
                    { model | zone = newZone }

                DebugOffsetIncrement value ->
                    { model | debugOffset = model.debugOffset + value }

                DebugOffsetDecrement value ->
                    { model | debugOffset = model.debugOffset - value }

                DebugOffsetReset ->
                    { model | debugOffset = 0 }

                DebugTogglePause ->
                    { model | debugPaused = not model.debugPaused }
    in
    ( updatedModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.debugPaused then
        Sub.none

    else
        Browser.Events.onAnimationFrame Tick



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewPolarClock model
        , viewDebug model
        ]


viewDebug : Model -> Html Msg
viewDebug model =
    let
        buttonGroup label offset =
            div []
                [ text label
                , button [ onClick (DebugOffsetIncrement offset) ] [ text "+" ]
                , button [ onClick (DebugOffsetDecrement offset) ] [ text "-" ]
                ]
    in
    div [ id "debug" ]
        [ text ("Offset: " ++ String.fromInt model.debugOffset)
        , buttonGroup "1 ms" 1
        , buttonGroup "10 ms" 10
        , buttonGroup "100 ms" 100
        , buttonGroup "Second" 1000
        , buttonGroup "Minute" (1000 * 60)
        , buttonGroup "Hour" (1000 * 60 * 60)
        , buttonGroup "Day" (1000 * 60 * 60 * 24)
        , buttonGroup "Month" (1000 * 60 * 60 * 24 * 30)
        , button [ onClick DebugOffsetReset ] [ text "Reset" ]
        , button [ onClick DebugTogglePause ]
            [ text
                (if model.debugPaused then
                    "Play"

                 else
                    "Pause"
                )
            ]
        ]


viewPolarClock : Model -> Html Msg
viewPolarClock model =
    let
        settings =
            { arcWidth = 5
            , gapWidth = 1
            , margin = 3
            , arcBorderRadius = 0.6
            , colorInitial = pi
            , colorDifference = -pi / 8
            , saturation = 0.65
            , lightness = 0.5
            , fontFamily = "'Lato', sans-serif"
            , fontSize = "2.6px"
            , textPosition = 0.375
            , textPad = 0.5
            }

        time =
            Time.millisToPosix (model.debugOffset + Time.posixToMillis model.time)
    in
    polarClock settings model.zone time
