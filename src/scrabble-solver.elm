module ScrabbleSolver where

import Array as Array exposing (Array)
import Char
import Dict as Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy2)
import Http exposing (Error(..))
import Json.Decode as Json exposing ((:=))
import Signal exposing (Signal, Address)
import Task exposing (Task, andThen, onError)

import BoardDesign as BD exposing (BoardDesign, TileMultiplier(..))
import Board as B exposing (Board, BoardCell(..), TileValue(..),
                            Dimension, Direction(..), Position)
import Move as M exposing (Move)

import BoardComponent as BoardC
import MoveListComponent as MoveListC

---- MODEL ----

type alias Model =
  { boardComponent: BoardC.Model
  , moveListComponent: MoveListC.Model
  , tileHand: String
  , displayMessage : Result String String
  }

type alias StorageModel =
  { board : String
  , boardDesignName : String
  , tileHand : String
  }

type alias SolutionQuery =
  { boardDesign : BoardDesign
  , board : Board
  , tileHand : String
  }

initialModel : Model
initialModel =
  { boardComponent = BoardC.initialModel
  , moveListComponent = MoveListC.initialModel
  , tileHand = ""
  , displayMessage = Ok ""
  }

storeModel : Model -> StorageModel
storeModel m =
  { board = B.boardToString m.boardComponent.boardBackup
  , boardDesignName = m.boardComponent.boardDesign.name
  , tileHand = m.tileHand
  }


---- UPDATE ----

type Action
  = NoOp
  | Reset
  | SetTileHand String
  | SetMoveList (Array Move)
  | SetBoardDesign BoardDesign
  | CommitMove
  | QueryInProgress
  | ReportError String
  | B BoardC.Action
  | ML MoveListC.Action

update : Action -> Model -> Model
update action model =
  case action of
    B act -> updateB act model
    ML act -> updateML act model
    NoOp -> model

    Reset ->
      { model | boardComponent = BoardC.update BoardC.Reset model.boardComponent
              , moveListComponent = MoveListC.update MoveListC.Reset model.moveListComponent
              , tileHand = "" }

    SetTileHand t ->
      { model | tileHand = t }

    SetMoveList ms ->
      { model | boardComponent = BoardC.update BoardC.ClearMove model.boardComponent
              , moveListComponent = MoveListC.update (MoveListC.SetMoveList ms) model.moveListComponent
              , displayMessage = Ok "" }

    SetBoardDesign name ->
      { model | boardComponent = BoardC.update (BoardC.SetBoardDesign name) model.boardComponent
              , moveListComponent = MoveListC.update MoveListC.Reset model.moveListComponent }

    CommitMove ->
      { model | boardComponent = BoardC.update BoardC.CommitMove model.boardComponent
              , moveListComponent = MoveListC.update MoveListC.Reset model.moveListComponent
              , tileHand = "" }

    QueryInProgress ->
      { model | moveListComponent = MoveListC.update MoveListC.Reset model.moveListComponent
              , displayMessage = (Ok "Searching...") }

    ReportError msg ->
      { model | displayMessage = Err msg }

updateB : BoardC.Action -> Model -> Model
updateB action model =
  case action of
    BoardC.NoOp -> model

    BoardC.SetTile pos cell newPos ->
      { model | boardComponent = BoardC.update action model.boardComponent
              , moveListComponent = MoveListC.update MoveListC.Reset model.moveListComponent }

    BoardC.SetCursor position ->
      { model | boardComponent = BoardC.update action model.boardComponent }

    BoardC.ToggleCursorDirection ->
      { model | boardComponent = BoardC.update action model.boardComponent }

    _ -> model

updateML : MoveListC.Action -> Model -> Model
updateML action model =
  case action of
    MoveListC.SelectMove i ->
      if model.moveListComponent.selectedMoveId == Just i
        then { model | boardComponent = BoardC.update BoardC.ClearMove model.boardComponent
                     , moveListComponent = MoveListC.update MoveListC.UnselectMove model.moveListComponent }
        else case Array.get i model.moveListComponent.moves of
               Nothing ->
                 { model | displayMessage = Err "Selected move could not be found." }
               Just selectedMove ->
                 { model | boardComponent = BoardC.update (BoardC.ShowMove selectedMove) model.boardComponent
                         , moveListComponent = MoveListC.update (MoveListC.SelectMove i) model.moveListComponent }

    _ -> model


---- VIEW ----

view : Address Action -> Model -> Html
view addr model =
  div [ id "scrabble-solver-ui" ]
      [ infoHeader addr model
      , lazy2 BoardC.view (Signal.forwardTo addr B) model.boardComponent
      , lazy2 MoveListC.view (Signal.forwardTo addr ML) model.moveListComponent
      , lazy2 controls addr model
      , infoFooter
      ]

infoHeader : Address Action -> Model -> Html
infoHeader addr model =
  let title = model.boardComponent.boardDesign.name ++ " Solver" in
  h1 [] [text title]

controls : Address Action -> Model -> Html
controls addr model =
  div [ id "controls" ]
      [ tileHand addr model
      , boardTypeSelectors addr model
      , searchButton addr model
      , selectButton addr model
      , resetButton addr model
      , displayMessage addr model
      ]

tileHand : Address Action -> Model -> Html
tileHand addr model =
  input [ id "tileHand"
        , placeholder "Tiles..."
        , value model.tileHand
        , maxlength 7
        , on "input" targetValue (\s -> Signal.message addr (SetTileHand s))
        ]
        []

boardTypeSelectors : Address Action -> Model -> Html
boardTypeSelectors addr model =
  div [ id "boardTypeSelectors" ]
      (List.map (boardTypeSelector addr model) (Dict.toList BD.boardDesigns))


boardTypeSelector : Address Action -> Model -> (String, BoardDesign) -> Html
boardTypeSelector addr model (boardType, boardDesign) =
  div [ class "boardTypeSelector" ]
      [ input [ id (boardType ++ "Selector")
              , type' "radio"
              , checked (model.boardComponent.boardDesign.name == boardType)
              , onClick addr (SetBoardDesign boardDesign)
              ]
              []
      , label []
              [ text boardType ]
      ]

searchButton : Address Action -> Model -> Html
searchButton addr model =
  input [ id "searchButton"
        , class "button"
        , type' "button"
        , value "Search"
        , onClick query.address
            (Just { boardDesign = model.boardComponent.boardDesign
                  , board = model.boardComponent.boardBackup
                  , tileHand = model.tileHand
                  })
        ]
        []

selectButton : Address Action -> Model -> Html
selectButton addr model =
  input [ id "selectButton"
        , class "button"
        , type' "button"
        , value "Select"
        , onClick addr CommitMove
        ]
        []

resetButton : Address Action -> Model -> Html
resetButton addr model =
  input [ id "resetButton"
        , class "button"
        , type' "button"
        , value "Reset"
        , onClick addr Reset
        ]
        []

displayMessage : Address Action -> Model -> Html
displayMessage addr model =
  let (className, msg) =
    case model.displayMessage of
      Ok msg -> ("infoMessage", msg)
      Err msg -> ("errorMessage", "Error: " ++ msg)
  in
  div [ id "displayMessage"
      , class className
      ]
      [ text msg ]

infoFooter =
  footer [ id "info" ]
         [ p []
           [ text "Written by "
           , a [ href "https://github.com/jasdennison" ] [ text "James Dennison" ]
           ]
         ]


---- TASKS ----

getSolution : Maybe SolutionQuery -> Task Http.Error (Array Move)
getSolution query =
  case query of
    Just q -> let parameters =
                [ ("boardDesign", q.boardDesign.asString),
                  ("board", B.boardToString q.board),
                  ("tileHand", q.tileHand) ]
              in
              Http.get M.moveDecoder (solveUrl parameters)
    Nothing -> Task.succeed Array.empty

solveUrl : List (String, String) -> String
solveUrl parameters = Http.url "http://scrabble-solver.jasdennison.uk:8080/solve" parameters


---- INPUTS ----

main : Signal Html
main = Signal.map (view actions.address) model

model : Signal Model
model = Signal.foldp update startingModel allActions

allActions : Signal Action
allActions = Signal.merge
               actions.signal
               (Signal.map queryToAction query.signal)

queryToAction : Maybe SolutionQuery -> Action
queryToAction query =
  case query of
    Just q -> QueryInProgress
    Nothing -> NoOp

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

query : Signal.Mailbox (Maybe SolutionQuery)
query = Signal.mailbox Nothing

startingModel : Model
startingModel =
  let m = initialModel
  in
  case getStorage of
    Just storedModel ->
      let storedBoardDesign = Maybe.withDefault (m.boardComponent.boardDesign) (Dict.get storedModel.boardDesignName BD.boardDesigns)
          storedBoard = B.parseBoard (storedBoardDesign.dimension) storedModel.board
          b = BoardC.initialModel
          b' = { b | board = storedBoard
                   , boardBackup = storedBoard
                   , boardDesign = storedBoardDesign }
      in
      { m | boardComponent = b'
          , tileHand = storedModel.tileHand }
    Nothing -> m

port requestSolution : Signal (Task Http.Error ())
port requestSolution =
  query.signal
    |> Signal.map getSolution
    |> Signal.map (\task ->
         (task
         `andThen` (\ms -> Signal.send actions.address (SetMoveList ms)))
         `onError` (\e -> Signal.send actions.address (ReportError (interpretHttpError e))))

interpretHttpError : Http.Error -> String
interpretHttpError e =
  case e of
    Timeout -> "Search request timed out."
    NetworkError -> "Search request failed due to network error."
    UnexpectedPayload msg -> "Search request payload error: " ++ msg
    BadResponse code msg -> "Search request failed: (" ++ toString code ++ ") - " ++ msg

isFocusAction : Action -> Bool
isFocusAction a =
  case a of
    B (BoardC.SetCursor _) -> True
    B (BoardC.SetTile _ _ _) -> True
    _ -> False

focusActionToId : Action -> String
focusActionToId a =
  let genId (x, y) = "#cell_" ++ toString x ++ "_" ++ toString y in
  case a of
    B (BoardC.SetCursor p) -> genId p
    B (BoardC.SetTile _ _ p) -> genId p
    _ -> ""

port focus : Signal String
port focus =
  actions.signal
    |> Signal.filter isFocusAction NoOp
    |> Signal.map focusActionToId

port getStorage : Maybe StorageModel

port setStorage : Signal StorageModel
port setStorage =
  Signal.map storeModel model

