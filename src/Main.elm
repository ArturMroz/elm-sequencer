module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Set exposing (Set)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { track : Track
    }



-- type alias Model =
--     { counter : Int
--     , tracks : Array Track
--     , playback : Playback
--     , playbackPosition : PlaybackPosition
--     , bpm : Int
--     , playbackSequence : Array (Set Clip)
--     }


type alias Track =
    { name : String

    -- , clip : Clip
    , sequence : Array Step
    }


type Step
    = On
    | Off


type alias Clip =
    String


type Playback
    = Playing
    | Stopped


type alias PlaybackPosition =
    Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( { track =
            { sequence = Array.initialize 16 (always Off)
            , name = "Kick"
            }
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ToggleStep Int Step


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleStep index step ->
            let
                selectedTrack =
                    model.track

                toggleStep =
                    if step == Off then
                        On

                    else
                        Off

                sequence_ =
                    selectedTrack.sequence |> Array.set index toggleStep

                track_ =
                    { selectedTrack | sequence = sequence_ }
            in
            ( { model | track = track_ }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    renderTrack model.track


renderTrack : Track -> Html Msg
renderTrack track =
    div
        [ class "track" ]
        [ p [ class "track-title" ] [ text track.name ]
        , div [ class "track-sequence" ] (renderSequence track.sequence)
        ]


renderSequence : Array Step -> List (Html Msg)
renderSequence sequence =
    Array.indexedMap renderStep sequence
        |> Array.toList


renderStep : Int -> Step -> Html Msg
renderStep index step =
    let
        classes =
            if step == On then
                "step active"

            else
                "step"
    in
    button
        [ onClick (ToggleStep index step)
        , class classes
        ]
        []
