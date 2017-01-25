module ChatDebug.Main exposing (..)

import Multitier exposing (MultitierProgram)
import Multitier.Debugger as Debugger exposing (Model, ServerModel, Msg, ServerMsg)

import Chat.Main as Main

-- MAIN

program : MultitierProgram (Model Main.Model) (ServerModel Main.ServerModel) (Msg Main.Msg) (ServerMsg Main.ServerMsg)
program =
  Debugger.program
    { config = Main.config
    , init = Main.init
    , update = Main.update
    , subscriptions = Main.subscriptions
    , view = Main.view
    , serverState = Main.serverState
    , serverRPCs = Main.serverRPCs
    , initServer = Main.initServer
    , updateServer = Main.updateServer
    , serverSubscriptions = Main.serverSubscriptions
    }
