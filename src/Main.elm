port module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, input, label, li, ol, p, text)
import Html.Attributes exposing (class, classList, maxlength, type_, value)
import Html.Events exposing (onClick, onInput)
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


hat : Clip
hat =
    "hat"


kick : Clip
kick =
    "kick"


snare : Clip
snare =
    "snare"


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tracks = Array.fromList [ initTrack hat, initTrack snare, initTrack kick ]
      , playback = Stopped
      , playbackPosition = 16
      , bpm = 100
      , playbackSequence = Array.initialize 16 (always Set.empty)

      --   , playbackSequence = rockPreset
      }
    , Cmd.none
    )


rockPreset : Array Track
rockPreset =
    [ { name = hat
      , sequence = Array.initialize 16 (\n -> modBy 2 n == 0)
      }
    , { name = snare
      , sequence = Array.initialize 16 (\n -> n == 4 || n == 12)
      }
    , { name = kick
      , sequence = Array.initialize 16 (\n -> List.member n [ 0, 3, 6, 9, 10 ])
      }
    ]
        |> Array.fromList


funkPreset : Array Track
funkPreset =
    [ { name = hat
      , sequence = Array.initialize 16 (always True)
      }
    , { name = snare
      , sequence = Array.initialize 16 (\n -> n == 4 || n == 12)
      }
    , { name = kick
      , sequence = Array.initialize 16 (\n -> List.member n [ 0, 1, 6, 7 ])
      }
    ]
        |> Array.fromList



-- rockPreset : Array (Set Clip)
-- rockPreset =
--     [ Set.fromList [ hat, kick ]
--     , Set.empty
--     , Set.fromList [ hat ]
--     , Set.empty
--     , Set.fromList [ hat, snare ]
--     , Set.empty
--     , Set.fromList [ hat, kick ]
--     , Set.empty
--     , Set.fromList [ hat, kick ]
--     , Set.empty
--     , Set.fromList [ hat ]
--     , Set.empty
--     , Set.fromList [ hat, snare ]
--     , Set.empty
--     , Set.fromList [ hat ]
--     , Set.empty
--     ]
--         |> Array.fromList
-- funkPreset : Array (Set Clip)
-- funkPreset =
--     [ Set.fromList [ hat, kick ]
--     , Set.fromList [ hat, kick ]
--     , Set.fromList [ hat ]
--     , Set.fromList [ hat ]
--     , Set.fromList [ hat, snare ]
--     , Set.fromList [ hat ]
--     , Set.fromList [ hat, kick ]
--     , Set.fromList [ hat, kick ]
--     , Set.fromList [ hat ]
--     , Set.fromList [ hat ]
--     , Set.fromList [ hat ]
--     , Set.fromList [ hat ]
--     , Set.fromList [ hat, snare ]
--     , Set.fromList [ hat ]
--     , Set.fromList [ hat ]
--     , Set.fromList [ hat ]
--     ]
--         |> Array.fromList
-- |> Set.to


initTrack : Clip -> Track
initTrack clip =
    { sequence = Array.initialize 16 (always False)
    , name = clip
    }


type Msg
    = ToggleStep Int Clip Int Bool
    | UpdatePlaybackPosition Time.Posix
    | UpdateBPM String
    | StartPlayback
    | StopPlayback
    | SetRockPreset
    | SetFunkPreset


setNestedArray : Int -> (a -> a) -> Array a -> Array a
setNestedArray index setFn array =
    case Array.get index array of
        Nothing ->
            array

        Just x ->
            Array.set index (setFn x) array


updateTrackStep : Int -> Int -> Array Track -> Array Track
updateTrackStep trackIndex stepIndex tracks =
    let
        toggleStep step =
            not step

        newSequence track =
            setNestedArray stepIndex toggleStep track.sequence

        newTrack track =
            { track | sequence = newSequence track }
    in
    setNestedArray trackIndex newTrack tracks



-- updatePlaybackFromPreset : Array Track -> Array (Set Clip) -> Array (Set Clip)


updatePlaybackFromPreset : Model -> Array (Set Clip)
updatePlaybackFromPreset { tracks, playbackSequence } =
    let
        rockPreset2 =
            [ Set.fromList [ hat, kick ]
            , Set.empty
            , Set.fromList [ hat ]
            , Set.empty
            , Set.fromList [ hat, snare ]
            , Set.empty
            , Set.fromList [ hat, kick ]
            , Set.empty
            , Set.fromList [ hat, kick ]
            , Set.empty
            , Set.fromList [ hat ]
            , Set.empty
            , Set.fromList [ hat, snare ]
            , Set.empty
            , Set.fromList [ hat ]
            , Set.empty
            ]
                |> Array.fromList
    in
    rockPreset2


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

        UpdateBPM bpm ->
            let
                bpm_ =
                    Maybe.withDefault model.bpm (String.toInt bpm)
            in
            ( { model | bpm = bpm_ }, Cmd.none )

        StartPlayback ->
            ( { model | playback = Playing }, Cmd.none )

        StopPlayback ->
            ( { model
                | playback = Stopped
                , playbackPosition = 16
              }
            , Cmd.none
            )

        SetRockPreset ->
            ( { model
                | tracks = rockPreset

                -- , playbackSequence = updatePlaybackSequence 2 hat model.playbackSequence
                , playbackSequence = updatePlaybackFromPreset model
                , bpm = 110
              }
            , Cmd.none
            )

        SetFunkPreset ->
            ( { model
                | tracks = funkPreset
                , bpm = 90
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
        , onInput UpdateBPM
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

        buttonText =
            if model.playback == Stopped then
                "Play"

            else
                "Stop"
    in
    button
        [ onClick togglePlayback
        , class buttonClasses
        ]
        [ text buttonText ]


renderControls : Model -> Html Msg
renderControls model =
    div
        [ class "controls" ]
        [ renderPlaybackButton model
        , renderBPM model
        , div [] []
        , label [] [ text "presets" ]
        , button [ onClick SetRockPreset ] [ text "Rock" ]

        -- label [] [ text "BPM" ]
        -- p [] [ text (String.fromInt model.playbackPosition) ]
        -- , button [ onClick StartPlayback ] []
        -- , button [ onClick StopPlayback ] []
        -- , button [ onClick SetFunkPreset ] [ text "Funk" ]
        ]


renderTracks : Model -> Html Msg
renderTracks model =
    div []
        (model.tracks
            |> Array.indexedMap (renderTrack model.playbackPosition)
            |> Array.toList
        )


renderTrack : Int -> Int -> Track -> Html Msg
renderTrack playbackPosition index track =
    div
        [ class "track" ]
        [ p [ class "track-title" ] [ text track.name ]
        , div [ class "track-sequence" ] (renderSequence playbackPosition index track.name track.sequence)
        ]


renderSequence : Int -> Int -> Clip -> Array Bool -> List (Html Msg)
renderSequence playbackPosition index clip sequence =
    Array.indexedMap (renderStep playbackPosition index clip) sequence
        |> Array.toList


renderStep : Int -> Int -> Clip -> Int -> Bool -> Html Msg
renderStep playbackPosition trackIndex clip stepIndex step =
    button
        [ onClick (ToggleStep trackIndex clip stepIndex step)
        , classList
            [ ( "step", True )
            , ( "_active", step )
            , ( "_flashing", step && playbackPosition == stepIndex )
            ]
        ]
        []
