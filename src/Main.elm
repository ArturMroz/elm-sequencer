port module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, input, li, ol, p, text)
import Html.Attributes exposing (class, maxlength, type_, value)
import Html.Events exposing (onClick)
import Set exposing (Set)
import Time exposing (..)


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
    { tracks : Array Track
    , playback : Playback
    , playbackPosition : Int
    , bpm : Int
    , playbackSequence : Array (Set Clip)
    }


type alias Track =
    { name : String

    -- , clip : Clip
    , sequence : Array Bool
    }


type Step
    = On
    | Off


type alias Clip =
    String


type Playback
    = Playing
    | Stopped


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tracks = Array.fromList [ initTrack "hat", initTrack "snare", initTrack "kick" ]
      , playback = Playing
      , playbackPosition = 16
      , bpm = 100
      , playbackSequence = Array.initialize 16 (always Set.empty)
      }
    , Cmd.none
    )



-- initSequence : Array Bool
-- initSequence =
--     Array.initialize 16 (always False)


initTrack : Clip -> Track
initTrack clip =
    { sequence = Array.initialize 16 (always False)
    , name = clip
    }



-- initHat : Track
-- initHat =
--     { sequence = initSequence
--     , name = "hat"
--     }
-- initSnare : Track
-- initSnare =
--     { sequence = initSequence
--     , name = "snare"
--     }
-- initKick : Track
-- initKick =
--     { sequence = initSequence
--     , name = "kick"
--     }
-- UPDATE


type Msg
    = ToggleStep Int Clip Int Bool
    | UpdatePlaybackPosition Time.Posix
    | StartPlayback
    | StopPlayback


setNestedArray : Int -> (a -> a) -> Array a -> Array a
setNestedArray index setFn array =
    case Array.get index array of
        Nothing ->
            array

        Just a ->
            Array.set index (setFn a) array


updateTrackStep : Int -> Int -> Array Track -> Array Track
updateTrackStep trackIndex stepIndex tracks =
    let
        toggleStep step =
            not step

        -- if step == Off then
        --     On
        -- else
        --     Off
        newSequence track =
            setNestedArray stepIndex toggleStep track.sequence

        newTrack track =
            { track | sequence = newSequence track }
    in
    setNestedArray trackIndex newTrack tracks


updatePlaybackSequence : Int -> Clip -> Array (Set Clip) -> Array (Set Clip)
updatePlaybackSequence stepIndex trackClip playbackSequence =
    let
        updateSequence clip sequence =
            if Set.member clip sequence then
                Set.remove clip sequence

            else
                Set.insert clip sequence
    in
    -- Array.set stepIndex (updateSequence trackClip) playbackSequence
    setNestedArray stepIndex (updateSequence trackClip) playbackSequence


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleStep trackIndex clip stepIndex step ->
            ( { model
                | tracks = updateTrackStep trackIndex stepIndex model.tracks
                , playbackSequence = updatePlaybackSequence stepIndex clip model.playbackSequence
              }
            , Cmd.none
            )

        UpdatePlaybackPosition _ ->
            let
                position_ =
                    if model.playbackPosition >= 15 then
                        0

                    else
                        model.playbackPosition + 1

                stepClips =
                    Array.get position_ model.playbackSequence
                        |> Maybe.withDefault Set.empty
            in
            ( { model | playbackPosition = position_ }
            , sendClips (Set.toList stepClips)
            )

        StartPlayback ->
            ( { model | playback = Playing }, Cmd.none )

        StopPlayback ->
            ( { model
                | playback = Stopped
                , playbackPosition = 16
              }
            , Cmd.none
            )


port sendClips : List Clip -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.playback == Playing then
        Time.every (bpmToMilliseconds model.bpm) UpdatePlaybackPosition

    else
        Sub.none


bpmToMilliseconds : Int -> Float
bpmToMilliseconds bpm =
    let
        secondsPerMinute =
            60

        millisecondsPerSecond =
            1000

        beats =
            4
    in
    (secondsPerMinute / toFloat bpm * millisecondsPerSecond) / beats



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ renderCursor model
        , renderTracks model
        , renderControls model
        ]


renderCursorPoint : Model -> Int -> Set String -> Html Msg
renderCursorPoint model index _ =
    let
        activeClass =
            if model.playbackPosition == index && model.playback == Playing then
                "_active"

            else
                ""
    in
    li [ class activeClass ] []


renderCursor : Model -> Html Msg
renderCursor model =
    ol
        [ class "cursor" ]
        (Array.toList <| Array.indexedMap (renderCursorPoint model) model.playbackSequence)


renderBPM : Model -> Html Msg
renderBPM model =
    input
        [ class "bpm-input"
        , value (String.fromInt model.bpm)
        , maxlength 3
        , type_ "number"
        , Html.Attributes.min "60"
        , Html.Attributes.max "300"
        ]
        []


renderPlaybackButton : Model -> Html Msg
renderPlaybackButton model =
    let
        togglePlayback =
            if model.playback == Stopped then
                StartPlayback

            else
                StopPlayback

        buttonClasses =
            if model.playback == Playing then
                "playback-button _playing"

            else
                "playback-button _stopped"
    in
    button
        [ onClick togglePlayback
        , class buttonClasses
        ]
        []


renderControls : Model -> Html Msg
renderControls model =
    div []
        [ p [] [ text (String.fromInt model.playbackPosition) ]

        -- , button [ onClick StartPlayback ] []
        -- , button [ onClick StopPlayback ] []
        , renderPlaybackButton model
        ]


renderTracks : Model -> Html Msg
renderTracks model =
    div []
        (model.tracks
            |> Array.indexedMap renderTrack
            |> Array.toList
        )


renderTrack : Int -> Track -> Html Msg
renderTrack index track =
    div
        [ class "track" ]
        [ p [ class "track-title" ] [ text track.name ]
        , div [ class "track-sequence" ] (renderSequence index track.name track.sequence)
        ]


renderSequence : Int -> Clip -> Array Bool -> List (Html Msg)
renderSequence index clip sequence =
    Array.indexedMap (renderStep index clip) sequence
        |> Array.toList


renderStep : Int -> Clip -> Int -> Bool -> Html Msg
renderStep trackIndex clip stepIndex step =
    let
        classes =
            if step then
                "step _active"

            else
                "step"
    in
    button
        [ onClick (ToggleStep trackIndex clip stepIndex step)
        , class classes
        ]
        []
