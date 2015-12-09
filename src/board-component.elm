module BoardComponent
  ( Model, initialModel
  , Action(..), update
  , view) where

import Array as Array exposing (Array)
import Char
import Dict as Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing ((:=))
import Signal exposing (Signal, Address)

import BoardDesign as BD exposing (BoardDesign, TileMultiplier(..))
import Board as B exposing (Board, BoardCell(..), TileValue(..),
                            Dimension, Direction(..), Position)
import Move as M exposing (Move)

---- MODEL ----

type alias Model =
  { boardDesign: BoardDesign
  , board : Board
  , boardBackup : Board
  , cursorPosition : Position
  , cursorDirection : Direction
  }

initialModel : Model
initialModel =
  let bd = Maybe.withDefault (BD.emptyBoardDesign) (Dict.get "Scrabble" BD.boardDesigns)
  in
  { boardDesign = bd
  , boardBackup = B.emptyBoard bd.dimension
  , board = B.emptyBoard bd.dimension
  , cursorPosition = initialCursorPosition bd.dimension
  , cursorDirection = Right
  }

initialCursorPosition : Dimension -> Position
initialCursorPosition (x, y) =
  let halfX = toFloat x / 2
      halfY = toFloat y / 2
  in
  ((ceiling halfX) - 1, (ceiling halfY) - 1)


---- UPDATE ----

type Action
  = NoOp
  | Reset
  | SetTile Position BoardCell Position
  | SetBoardDesign BoardDesign
  | SetCursor Position
  | ToggleCursorDirection
  | ShowMove Move
  | CommitMove
  | ClearMove

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model

    Reset ->
      { model | board = B.emptyBoard model.boardDesign.dimension
              , boardBackup = B.emptyBoard model.boardDesign.dimension }

    SetTile pos cell newPos ->
      let newBoard = B.setBoardCell model.boardBackup pos cell
      in
      { model | board = newBoard
              , boardBackup = newBoard
              , cursorPosition = newPos }

    SetBoardDesign bd ->
      { model | boardDesign = bd
              , board = model.boardBackup }

    SetCursor position ->
      { model | cursorPosition = position }

    ToggleCursorDirection ->
      { model | cursorDirection = B.toggleDirection model.cursorDirection }

    ShowMove move ->
      { model | board = M.addMoveToBoard model.boardBackup move }

    CommitMove ->
      let newBoard = Array.map (Array.map B.convertMoveToPlacement) model.board
      in
      { model | board = newBoard
              , boardBackup = newBoard }

    ClearMove ->
      { model | board = model.boardBackup }


---- VIEW ----

view : Address Action -> Model -> Html
view addr model =
  let yDimension = snd model.boardDesign.dimension
  in
  div [ id "board" ]
      (List.map (boardRow addr model) [0..(yDimension - 1)])

boardRow : Address Action -> Model -> Int -> Html
boardRow addr model n =
  let xDimension = fst model.boardDesign.dimension
  in
  div [ id ("boardRow_" ++ toString n)]
      (List.map (\x -> boardCell addr model (x, n)) [0..(xDimension - 1)])

boardCell : Address Action -> Model -> Position -> Html
boardCell addr model (x, y) =
  let tileValue = Maybe.withDefault Empty (B.getBoardCell model.board (x, y))
  in
  input [ id ("cell_" ++ toString x ++ "_" ++ toString y)
        , class (boardCellClass model (x, y))
        , key (toString (x, y))
        , autofocus (model.cursorPosition == (x, y))
        , value (B.boardCellToString "" tileValue)
        , maxlength 1
        , on "keydown" keyDecoder (\k ->
            let action = (keyCodeToAction model.boardDesign.dimension
                                          (x, y)
                                          model.cursorDirection
                                          k)
            in
            Signal.message addr action)
        , onClick addr (SetCursor (x, y))
        ]
        []

boardCellClass : Model -> Position -> String
boardCellClass model pos =
  let tileType = case B.getBoardCell model.board pos of
                   Just (Proposed _) -> " moveCell"
                   Just (Full _) -> " fullCell"
                   _ -> ""
      active = if model.cursorPosition == pos
                 then " cursor" ++ toString model.cursorDirection
                 else ""
      multiplier = case BD.getMultiplierCell model.boardDesign pos of
                     Just None -> ""
                     Just mult -> " mult" ++ toString mult
                     Nothing -> ""
  in
  "boardCell" ++ tileType ++ multiplier ++ active

keyDecoder : Json.Decoder KeyCode
keyDecoder =
  Json.object2 KeyCode
    ("keyCode" := Json.int)
    ("shiftKey" := Json.bool)

type alias KeyCode =
  { keyCode : Int
  , shiftKey : Bool
  }

calcCursorPosition : Position -> Direction -> Dimension -> Position
calcCursorPosition pos dir dim =
  let newPos = B.movePosition pos dir
  in
  B.normalisePosition dim newPos

keyCodeToAction : Dimension -> Position -> Direction -> KeyCode -> Action
keyCodeToAction dim pos dir k =
  if k.keyCode == 37 then
    SetCursor (calcCursorPosition pos Left dim)
  else if k.keyCode == 38 then
    SetCursor (calcCursorPosition pos Up dim)
  else if k.keyCode == 39 then
    SetCursor (calcCursorPosition pos Right dim)
  else if k.keyCode == 40 then
    SetCursor (calcCursorPosition pos Down dim)
  else if k.keyCode == 8 then
    SetTile pos Empty (calcCursorPosition pos (B.oppositeDirection dir) dim)
  else if k.keyCode == 46 then
    SetTile pos Empty pos
  else if k.keyCode >= 65 && k.keyCode <= 90 && (not k.shiftKey) then
    let tileValue = Full (B.charToTileValue (Char.toLower (Char.fromCode k.keyCode)))
    in
    SetTile pos tileValue (calcCursorPosition pos dir dim)
  else if k.keyCode >= 65 && k.keyCode <= 90 && k.shiftKey then
    let tileValue = Full (B.charToTileValue (Char.toUpper (Char.fromCode k.keyCode)))
    in
    SetTile pos tileValue (calcCursorPosition pos dir dim)
  else if k.keyCode == 32 then
    ToggleCursorDirection
  else
    NoOp

