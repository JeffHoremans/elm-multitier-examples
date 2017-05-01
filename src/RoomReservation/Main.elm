module RoomReservation.Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events as E
import Html.Attributes
import Task exposing (Task)

import Multitier exposing (MultitierProgram, MultitierCmd(..), Config, none, batch, performOnServer, performOnClient, map, (!!))
import Multitier.RPC as RPC exposing (rpc, RPC)
import Multitier.Error exposing (Error(..))

-- MULTITIER-CONFIG

config: Config
config = { httpPort = 8081, hostname = "localhost" }

-- SERVER-MODEL

type alias ServerModel = { rooms: List Room }

type alias Room = { number: Int
                  , name: String
                  , seats: Int
                  , tables: Int
                  , reservations: List Reservation }

type alias Reservation = { time: String
                         , set: Maybe String}

defaultRooms: List Room
defaultRooms = [ Room 1 "Large conference room 1" 10 3 defaultReservations
        , Room 2 "Large conference room 2" 8 3 defaultReservations
        , Room 3 "Small conference room 1" 6 2 defaultReservations
        , Room 4 "Small conference room 2" 4 1 defaultReservations]

defaultReservations : List Reservation
defaultReservations = [ Reservation "10:00" Nothing
                      , Reservation "11:00" Nothing
                      , Reservation "12:00" Nothing
                      , Reservation "13:00" Nothing
                      , Reservation "14:00" Nothing
                      , Reservation "15:00" Nothing
                      , Reservation "16:00" Nothing
                      , Reservation "17:00" Nothing ]

initServer : (ServerModel, Cmd ServerMsg)
initServer = ServerModel defaultRooms ! []

-- SERVER-REMOTE-UPDATE

type RemoteServerMsg = UpdateRoom Room

serverRPCs : RemoteServerMsg -> RPC ServerModel Msg ServerMsg
serverRPCs rproc = case rproc of
  UpdateRoom newRoom ->
    rpc Handle (\serverModel ->
      let newRooms = List.map (\room -> if room.number == newRoom.number then newRoom else room) serverModel.rooms in
        ({ serverModel | rooms = newRooms }, Task.succeed newRooms, Cmd.batch []))

-- SERVER-UPDATE

type ServerMsg = ServerNone

updateServer : ServerMsg -> ServerModel -> (ServerModel, Cmd ServerMsg)
updateServer serverMsg serverModel = case serverMsg of
  ServerNone -> serverModel ! []

-- SERVER-SUBSCRIPTIONS

serverSubscriptions : ServerModel -> Sub ServerMsg
serverSubscriptions serverModel = Sub.none

-- SERVER-STATE

type alias ServerState = { rooms: List Room }

serverState: ServerModel -> ServerState
serverState {rooms} = ServerState rooms

-- MODEL

type alias Model = { rooms: List Room, name: String }

init : ServerState -> ( Model, MultitierCmd RemoteServerMsg Msg)
init {rooms} = Model rooms "" !! []

-- UPDATE

type Msg = SetReservation Room | Handle (Result Error (List Room)) | OnInput String | None

update : Msg -> Model -> ( Model, MultitierCmd RemoteServerMsg Msg )
update msg model = case msg of
  SetReservation room -> model !! [performOnServer (UpdateRoom room)]
  OnInput input -> { model | name = input } !! []
  Handle result -> case result of
    Ok rooms -> { model | rooms = rooms } !! []
    _ -> model !! []
  None -> model !! []

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  Html.body [] [
  Html.node "link" [Html.Attributes.rel "stylesheet", Html.Attributes.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"] [],
  Html.div [] [
    Html.h1 [] [ Html.text "Conference room reservation"],
    Html.div [] [
      roomsView model,
      Html.span [] [Html.text "Reservation name: "],
      Html.input [E.onInput OnInput, Html.Attributes.value model.name] []]]]

roomsView : Model -> Html Msg
roomsView model =
  let tableBody =
    model.rooms |> List.map (\room ->
      Html.tr [] [
        Html.td [] [Html.text room.name],
        Html.td [] [Html.text (toString room.seats)],
        Html.td [] [Html.text (toString room.tables)],
        Html.td [] [
          Html.tr [] (room.reservations |> List.map (\reservation -> case reservation.set of
            Just name ->  Html.td [] [Html.a [style [("margin-right", "1em"), ("width", "4em")], class "btn btn-danger", E.onClick (SetReservation { room | reservations = List.map (\reserv -> if reserv.time == reservation.time then { reserv | set = Nothing} else reserv) room.reservations})] [Html.text "Full", Html.br [] [], Html.p [] [Html.text ("(" ++ name ++ ")")]]]
            _ -> Html.td [] [Html.a [style [("margin-right", "1em"), ("width", "4em")], class "btn btn-success", E.onClick (SetReservation { room | reservations = List.map (\reserv -> if reserv.time == reservation.time then { reserv | set = Just model.name} else reserv) room.reservations })] [Html.text "Taken", Html.br [] [], Html.p [style [("visibility", "hidden")]] [Html.text "()"]]] ))]]) in
    Html.table [ class "table table-striped table-hover"] [
      Html.thead [] [
        Html.tr [] [
          Html.th [] [Html.text "Room"],
          Html.th [] [Html.text "Seats"],
          Html.th [] [Html.text "Tables"],
          Html.th [] [Html.text "Reservations"]],
        Html.tr [] [
          Html.th [] [],
          Html.th [] [],
          Html.th [] [],
          Html.th [] [Html.tr [] (defaultReservations |> List.map (\reservation -> Html.th [] [ Html.p [style [("margin-right", "1em"), ("width", "4em")]] [Html.text (reservation.time ++ " ")]]))]]],
      Html.tbody [] tableBody]

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
