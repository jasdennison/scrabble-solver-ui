module MoveListComponent
  ( Model, initialModel
  , Action(..), update
  , view) where

import Array as Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Signal, Address)

import Move exposing (Move)

---- MODEL ----

type alias Model =
  { moves : Array Move
  , selectedMoveId : Maybe Int
  }

initialModel : Model
initialModel =
  { moves = Array.empty
  , selectedMoveId = Nothing
  }


---- UPDATE ----

type Action
  = Reset
  | SetMoveList (Array Move)
  | SelectMove Int
  | UnselectMove

update : Action -> Model -> Model
update action model =
  case action of

    Reset ->
      { model | moves <- Array.empty
              , selectedMoveId <- Nothing }

    SetMoveList ms ->
      { model | moves <- ms
              , selectedMoveId <- Nothing }

    SelectMove i ->
      { model | selectedMoveId <- Just i }

    UnselectMove ->
      { model | selectedMoveId <- Nothing }


---- VIEW ----

view : Address Action -> Model -> Html
view  addr model =
  if Array.isEmpty model.moves
     then div [ id "emptyMoveList"] []
     else div [ id "moveList" ]
          (Array.toList (Array.indexedMap (moveItem addr model) model.moves))

moveItem : Address Action -> Model -> Int -> Move -> Html
moveItem addr model i move =
  div [ id ("moveItem_" ++ toString i)
      , key (toString i)
      , class ("moveItem" ++ (
          case model.selectedMoveId of
            Nothing -> ""
            Just n -> if i == n then "-active" else ""))
      , onClick addr (SelectMove i)
      ]
      [ moveScore move.score
      , moveWord move.word
      ]

moveWord : String -> Html
moveWord word =
  div [ class "moveWord" ]
      [ text word ]

moveScore : Int -> Html
moveScore score =
  div [ class "moveScore" ]
      [ text (toString score) ]

