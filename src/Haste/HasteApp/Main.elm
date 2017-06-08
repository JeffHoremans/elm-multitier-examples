module Haste.HasteApp.Main exposing (..)

import Html exposing (Html)
import Task exposing (Task)
import Html.Attributes exposing (style, type_, value)
import Html.Events as E

import Multitier exposing (MultitierProgram, MultitierCmd(..), Config, performOnServer, (!!))
import Multitier.RPC as RPC exposing (rpc, RPC)
import Multitier.Error as Error exposing (Error)


-- MULTITIER-CONFIG

config: Config
config = { httpPort = 8081, hostname = "localhost" }

-- SERVER-MODEL

type alias ServerModel = String

initServer : (ServerModel, Cmd ServerMsg)
initServer = "This is not a message" ! []

-- SERVER-UPDATE

type ServerMsg = None

updateServer : ServerMsg -> ServerModel -> (ServerModel, Cmd ServerMsg)
updateServer serverMsg serverModel = serverModel ! []

-- SERVER-REMOTE-UPDATE

type RemoteServerMsg = Trade String

serverRPCs : RemoteServerMsg -> RPC ServerModel Msg ServerMsg
serverRPCs rproc = case rproc of
  Trade new -> rpc Handle (\old -> (new, Task.succeed old, Cmd.none))

-- MODEL

type alias Model = { input: String, message: String}

init : ( Model, MultitierCmd RemoteServerMsg Msg)
init = Model "" "" !! []

-- UPDATE

type Msg = OnInput String | TradeMessage | Handle (Result Error String)

update : Msg -> Model -> ( Model, MultitierCmd RemoteServerMsg Msg )
update msg model = case msg of
  OnInput input -> { model | input = input } !! []
  TradeMessage -> { model | input = ""} !! [performOnServer (Trade model.input)]
  Handle result -> case result of
    Result.Ok message -> { model | message = "The old message was: " ++ message } !! []
    _ -> model !! []

-- VIEW

view : Model -> Html Msg
view model = Html.body [] [
  Html.text "Enter a message: ", Html.input [E.onInput OnInput, value model.input] [], Html.button [E.onClick TradeMessage] [Html.text "Send"], Html.br [] [],
  Html.p [] [Html.text model.message]]

-- MAIN

program : MultitierProgram Model ServerModel Msg ServerMsg
program =  Multitier.program
    { config = config
    , init = \_ -> init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    , serverState = \s -> s
    , serverRPCs = serverRPCs
    , initServer = initServer
    , updateServer = updateServer
    , serverSubscriptions = \_ -> Sub.none }
