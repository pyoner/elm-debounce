effect module Debounce
    where { command = MyCmd }
    exposing
        ( debounce
        , kill
        )

import Dict exposing (Dict)
import Task exposing (Task)
import Time
import Process


type alias Key =
    String


type MyCmd msg
    = Request Time.Time Key (Key -> msg)
    | Kill Key (Key -> msg)


type Msg
    = Done Key


type alias State =
    Dict Key Process.Id


init : Task Never State
init =
    Task.succeed Dict.empty


debounce : Time.Time -> Key -> (Key -> msg) -> Cmd msg
debounce delay key tagger =
    command (Request delay key tagger)


kill : Key -> (Key -> msg) -> Cmd msg
kill key tagger =
    command (Kill key tagger)


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap f cmd =
    case cmd of
        Request delay key tagger ->
            Request delay key (tagger >> f)

        Kill key tagger ->
            Kill key (tagger >> f)


onEffects :
    Platform.Router msg Msg
    -> List (MyCmd msg)
    -> State
    -> Task Never State
onEffects router cmds state =
    case cmds of
        [] ->
            Task.succeed state

        (Request delay key tagger) :: tailCmds ->
            let
                task =
                    Process.sleep delay
                        |> Task.andThen
                            (\_ ->
                                Platform.sendToApp router
                                    (tagger key)
                            )
                        |> Task.andThen
                            (\_ ->
                                Platform.sendToSelf router
                                    (Done
                                        key
                                    )
                            )
            in
                case Dict.get key state of
                    Nothing ->
                        Process.spawn task
                            |> Task.andThen
                                (\id ->
                                    Task.succeed (Dict.insert key id state)
                                        |> Task.andThen
                                            (\newState ->
                                                onEffects
                                                    router
                                                    tailCmds
                                                    newState
                                            )
                                )

                    Just id ->
                        let
                            newState =
                                Dict.remove key state
                        in
                            Process.kill id
                                |> Task.andThen
                                    (\_ ->
                                        onEffects router
                                            cmds
                                            newState
                                    )

        (Kill key tagger) :: tailCmds ->
            case Dict.get key state of
                Nothing ->
                    Platform.sendToApp router (tagger key)
                        |> Task.andThen
                            (\_ ->
                                onEffects router tailCmds state
                            )

                Just id ->
                    let
                        newState =
                            Dict.remove key state
                    in
                        Process.kill id
                            |> Task.andThen
                                (\_ ->
                                    Platform.sendToApp router (tagger key)
                                )
                            |> Task.andThen
                                (\_ ->
                                    onEffects router
                                        tailCmds
                                        newState
                                )


onSelfMsg :
    Platform.Router msg Msg
    -> Msg
    -> State
    -> Task Never State
onSelfMsg router selfMsg state =
    case selfMsg of
        Done key ->
            Task.succeed (Dict.remove key state)
