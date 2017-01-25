module Main exposing (..)

import Html exposing (Html)
import Html.Events as E
import Html.Attributes
import Task exposing (Task)
import Time exposing (Time, second)
import WebSocket
import String

import Multitier exposing (MultitierProgram, MultitierCmd(..), Config, none, batch, performOnServer, map, (!!))
import Multitier.RPC as RPC exposing (rpc, RPC)
import Multitier.Error exposing (Error(..))
import Multitier.Server.Console as Console
import Multitier.Server.WebSocket as ServerWebSocket exposing (SocketServer)

import Counter

-- MULTITIER-CONFIG

config: Config
config = { httpPort = 8081, hostname = "localhost" }

-- SERVER-MODEL

type alias ServerModel = { socketServer: Maybe SocketServer
                         , messages: List String
                         , counter: Counter.ServerModel }

initServer : (ServerModel, Cmd ServerMsg)
initServer = ServerModel Maybe.Nothing [] Counter.initServer ! []

-- SERVER-REMOTE-UPDATE

type RemoteServerMsg = Log String | SendMessage String | CounterProc Counter.RemoteServerMsg

serverRPCs : RemoteServerMsg -> RPC ServerModel Msg ServerMsg
serverRPCs rproc = case rproc of
  Log val ->
    rpc Handle (\serverModel -> (serverModel, Task.succeed (), Console.log val))
  SendMessage message ->
    rpc Handle (\serverModel -> let newMessages = message :: serverModel.messages in
                                           ({ serverModel | messages = newMessages }, Task.succeed (), broadcast serverModel (String.join "," newMessages)))
  CounterProc proc ->
    RPC.map CounterMsg CounterServerMsg (\counter serverModel -> { serverModel | counter = counter}) (\serverModel -> serverModel.counter) (Counter.serverRPCs proc)

-- SERVER-UPDATE

type ServerMsg = ServerTick | OnMessage (Int,String) | OnSocketOpen SocketServer |
                 CounterServerMsg Counter.ServerMsg |
                 Nothing

updateServer : ServerMsg -> ServerModel -> (ServerModel, Cmd ServerMsg)
updateServer serverMsg serverModel = case serverMsg of
  ServerTick ->                 serverModel ! [Console.log (toString serverModel.messages)]
  OnMessage (cid,message) ->    serverModel ! [ Console.log message]
  OnSocketOpen socketServer ->  { serverModel | socketServer = Just socketServer } ! []

  CounterServerMsg msg ->      let (counter, cmd) = Counter.updateServer msg serverModel.counter in
                                { serverModel | counter = counter } ! [ Cmd.map CounterServerMsg cmd]

  Nothing ->                    serverModel ! []

-- SERVER-SUBSCRIPTIONS

serverSubscriptions : ServerModel -> Sub ServerMsg
serverSubscriptions serverModel =
  Sub.batch [ Time.every 10000 (always ServerTick)
            , ServerWebSocket.listen OnSocketOpen OnMessage
            , Sub.map CounterServerMsg (Counter.serverSubscriptions serverModel.counter)]

broadcast : ServerModel -> String -> Cmd ServerMsg
broadcast serverModel message = case serverModel.socketServer of
  Just server -> ServerWebSocket.broadcast server message
  _ -> Cmd.none

-- SERVER-STATE

type alias ServerState = { messages: List String }

serverState: ServerModel -> ServerState
serverState {messages} = ServerState messages

-- MODEL

type alias Model = { input: String
                   , messages: List String
                   , error: String
                   , counter: Counter.Model }

init : ServerState -> ( Model, MultitierCmd RemoteServerMsg Msg)
init {messages} = let (counter, cmds) = Counter.init
       in (Model "" messages "" counter,  batch [ map CounterProc CounterMsg cmds ])

-- UPDATE

type Msg = OnInput String | Send |
           Handle (Result Error ()) | SetMessages String |
           CounterMsg Counter.Msg | None

update : Msg -> Model -> ( Model, MultitierCmd RemoteServerMsg Msg )
update msg model =
    case msg of
      OnInput text -> ({ model | input = text}, none)
      Send -> { model | input = "" } !! [performOnServer (SendMessage model.input)]
      Handle result -> case result of
        Ok _ -> model !! []
        _ -> { model | error = "error" } !! []
      SetMessages messages -> ({model | messages = String.split "," messages}, none)


      CounterMsg subMsg -> let (counter, cmds) = Counter.update subMsg model.counter in { model | counter = counter } !! [ map CounterProc CounterMsg cmds ]
      None -> ( model, none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [WebSocket.listen "ws://localhost:8081" SetMessages, Sub.map CounterMsg (Counter.subscriptions model.counter)]

-- VIEW

view : Model -> Html Msg
view model =
  Html.body [] [
  Html.node "link" [Html.Attributes.rel "stylesheet", Html.Attributes.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"] [],
  Html.div [] [
    Html.h1 [] [ Html.text "Multitier Elm - Client"],
    Html.div [] [
    Html.input [E.onInput OnInput, Html.Attributes.value model.input] [],
    Html.button [E.onClick Send] [Html.text "Send"]
    ],
    Html.div [] [
      Html.text (toString model.messages),
      Html.br [] [],
      Html.text model.error],
    Html.div [] [Html.map CounterMsg (Counter.view model.counter)]]]

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
    , serverRPCs = serverRPCs
    , initServer = initServer
    , updateServer = updateServer
    , serverSubscriptions = serverSubscriptions
    }
