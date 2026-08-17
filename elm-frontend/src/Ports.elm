port module Ports exposing (..)

import Json.Decode as Decode


type alias SidebarSetup =
    { waveformId : String
    , spectrumId : String
    }


port sidebarSetup : SidebarSetup -> Cmd msg


port sidebarWheel : Float -> Cmd msg


type alias ScorePanelSetup =
    { editorId : String
    , source : String
    , bpm : Float
    }


port scorePanelSetup : ScorePanelSetup -> Cmd msg


port scoreEditorChange : (String -> msg) -> Sub msg


port scoreGraphUpdate : { source : String, bpm : Float } -> Cmd msg


port scoreGraphResult : (Decode.Value -> msg) -> Sub msg


type alias AudioPlayPayload =
    { bpm : Float
    , instruments : List String
    , scoreSource : String
    }


port audioPlay : AudioPlayPayload -> Cmd msg


port audioStop : () -> Cmd msg


port audioSetBpm : Float -> Cmd msg


port audioCpu : (Float -> msg) -> Sub msg


port audioPlaying : (Bool -> msg) -> Sub msg


port audioError : (Maybe String -> msg) -> Sub msg
