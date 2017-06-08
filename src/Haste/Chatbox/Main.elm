module Haste.Chatbox.Main exposing (..)

import Html exposing (Html)
import Task exposing (Task)
import WebSocket
import Dom.Scroll as Scroll
import Keyboard
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Html.Attributes exposing (id, rows, cols, readonly, value)
import Html.Events as E

import Multitier exposing (MultitierProgram, MultitierCmd(..), Config, none, batch, performOnServer, performOnClient, map, (!!))
import Multitier.RPC as RPC exposing (rpc, RPC)
import Multitier.Server.WebSocket as ServerWebSocket exposing (ClientId)


-- MULTITIER-CONFIG

config: Config
config = { httpPort = 8081, hostname = "localhost" }

-- SERVER-MODEL

type alias Message = (String, String)

encode : Message -> Value
encode (from,message) = Encode.object [("from", Encode.string from), ("message", Encode.string message)]

decode : Decoder Message
decode = Decode.map2 (,) (Decode.field "from" Decode.string) (Decode.field "message" Decode.string)

type alias ServerModel = List Message

initServer : (ServerModel, Cmd ServerMsg)
initServer = [] ! []

-- SERVER-UPDATE

type ServerMsg = OnMessage (ClientId,String)

updateServer : ServerMsg -> ServerModel -> (ServerModel, Cmd ServerMsg)
updateServer serverMsg serverModel = case serverMsg of
  OnMessage (cid,m) -> case Decode.decodeString decode m of
    Result.Ok message -> (message :: serverModel) ! [ServerWebSocket.broadcast "chat" (Encode.encode 0 (encode message))]
    _ -> serverModel ! []

-- SERVER-SUBSCRIPTIONS

serverSubscriptions : ServerModel -> Sub ServerMsg
serverSubscriptions serverModel =
  Sub.batch [ ServerWebSocket.listen "chat" OnMessage ]

-- SERVER-STATE

type alias ServerState = ServerModel

serverState: ServerModel -> ServerState
serverState s = s

-- MODEL

type alias Model = { messages: List Message, name: String, message: String}

init : ServerState -> ( Model, MultitierCmd a Msg)
init messages = Model (List.reverse messages) "" "" !! []

-- UPDATE

type Msg = AddMessage String | OnEnter | OnNameChange String | OnMessageChange String | None

update : Msg -> Model -> ( Model, MultitierCmd a Msg )
update msg model = case msg of
  AddMessage m -> case Decode.decodeString decode m of
    Result.Ok message -> { model | messages = (message :: model.messages) } !! [performOnClient <| Task.attempt (always None) (Scroll.toBottom "chat")]
    _ -> model !! []
  OnEnter -> { model | message = "" } !! [performOnClient (WebSocket.send "ws://localhost:8081/chat" (Encode.encode 0 (encode (model.name,model.message))))]
  OnNameChange name -> { model | name = name } !! []
  OnMessageChange message -> { model | message = message } !! []
  None -> model!! []

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [WebSocket.listen "ws://localhost:8081/chat" AddMessage, Keyboard.downs (\key -> if key == 13 then OnEnter else None)]

-- VIEW

view : Model -> Html Msg
view model = Html.body [] [
  Html.text "Name: ", Html.input [E.onInput OnNameChange, value model.name] [], Html.br [] [],
  Html.textarea [id "chat", rows 10, cols 40, readonly True] [Html.text (List.foldl (++) "" (List.map (\(f,m) -> f++":"++m++"\n") model.messages))], Html.br [] [],
  Html.input [E.onInput OnMessageChange, value model.message] []]

-- MAIN

program : MultitierProgram Model ServerModel Msg ServerMsg
program =
  Multitier.program
    { config = config
    , init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    , serverState = serverState
    , serverRPCs = \_ -> rpc (always None) (\s -> (s, Task.succeed (), Cmd.none))
    , initServer = initServer
    , updateServer = updateServer
    , serverSubscriptions = serverSubscriptions }
