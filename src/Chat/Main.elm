module Chat.Main exposing (..)

import Html exposing (Html)
import Html.Events as E
import Html.Attributes
import Task exposing (Task)
import Time exposing (Time, second)
import WebSocket
import String
import Process

import Multitier exposing (MultitierProgram, MultitierCmd(..), Config, none, batch, performOnServer, performOnClient, map, (!!))
import Multitier.RPC as RPC exposing (rpc, RPC)
import Multitier.Error exposing (Error(..))
import Multitier.Server.Console as Console
import Multitier.Server.WebSocket as ServerWebSocket exposing (ClientId)

import Chat.Counter as Counter

-- MULTITIER-CONFIG

config: Config
config = { httpPort = 8081, hostname = "localhost" }

-- SERVER-MODEL

type alias ServerModel = { messages: List String
                         , counter: Counter.ServerModel }

initServer : (ServerModel, Cmd ServerMsg)
initServer = ServerModel [] Counter.initServer ! []

-- SERVER-REMOTE-UPDATE

type RemoteServerMsg = Log String | SendMessage String | CounterProc Counter.RemoteServerMsg

serverRPCs : RemoteServerMsg -> RPC ServerModel Msg ServerMsg
serverRPCs rproc = case rproc of
  Log val ->
    rpc Handle (\serverModel -> (serverModel, Task.succeed (), Console.log val))
  SendMessage message ->
    rpc Handle (\serverModel -> let newMessages = message :: serverModel.messages in
                                           ({ serverModel | messages = newMessages }, (Process.sleep 20000 |> Task.andThen (\_ -> Task.succeed ())), Cmd.batch [broadcast (String.join "," newMessages), Task.perform (always MessageFromRPC) (Process.sleep 5000 |> Task.andThen (\_ -> Task.succeed ()))]))
  CounterProc proc ->
    RPC.map CounterMsg
      (\updateCounter serverModel -> let (newCounter,cmd) = updateCounter serverModel.counter in
        ({ serverModel | counter = newCounter}, Cmd.map CounterServerMsg cmd))
      (\serverModel -> serverModel.counter) (Counter.serverRPCs proc)

-- SERVER-UPDATE

type ServerMsg = ServerTick | OnMessage (ClientId,String) |
                 OnConnect ClientId | OnDisconnect ClientId |
                 CounterServerMsg Counter.ServerMsg | MessageFromRPC | Test

updateServer : ServerMsg -> ServerModel -> (ServerModel, Cmd ServerMsg)
updateServer serverMsg serverModel = case serverMsg of
  ServerTick ->                 serverModel ! [Console.log (toString serverModel.messages)]
  OnMessage (cid,message) ->    serverModel ! [Console.log message]

  CounterServerMsg msg ->      let (counter, cmd) = Counter.updateServer msg serverModel.counter in
                                { serverModel | counter = counter } ! [ Cmd.map CounterServerMsg cmd]

  OnConnect cid -> serverModel ! []
  OnDisconnect cid -> serverModel ! []
  MessageFromRPC -> serverModel ! [Task.perform (always Test) (Task.succeed ())]
  Test -> serverModel ! []

-- SERVER-SUBSCRIPTIONS

serverSubscriptions : ServerModel -> Sub ServerMsg
serverSubscriptions serverModel =
  Sub.batch [ Time.every 10000 (always ServerTick)
            , ServerWebSocket.listenAndMonitor "chat" OnConnect OnDisconnect OnMessage
            , Sub.map CounterServerMsg (Counter.serverSubscriptions serverModel.counter)]

broadcast : String -> Cmd ServerMsg
broadcast message = ServerWebSocket.broadcast "chat" message

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
       in (Model "" messages "" counter,  batch [ map CounterProc CounterMsg cmds ]) --, performOnServer (Log "log1"), performOnServer (Log "log2") ])

-- UPDATE

type Msg = OnInput String | Send |
           Handle (Result Error ()) | SetMessages String |
           CounterMsg Counter.Msg | ChildMessageTest | None

update : Msg -> Model -> ( Model, MultitierCmd RemoteServerMsg Msg )
update msg model =
    case msg of
      OnInput text -> ({ model | input = text}, performOnClient (Console.log "test"))
      Send -> { model | input = "" } !! [performOnServer (SendMessage model.input)]
      Handle result -> case result of
        Ok _ -> model !! []
        _ -> { model | error = "error" } !! []
      SetMessages messages -> {model | messages = String.split "," messages} !! [performOnClient (Task.perform (always ChildMessageTest) (Process.sleep 5000 |> Task.andThen (\_ -> Task.succeed ())))]


      CounterMsg subMsg -> let (counter, cmds) = Counter.update subMsg model.counter in { model | counter = counter } !! [ map CounterProc CounterMsg cmds ]
      ChildMessageTest -> ( model, none )
      None -> ( model, none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [WebSocket.listen "ws://localhost:8081/chat" SetMessages, Sub.map CounterMsg (Counter.subscriptions model.counter)]

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
