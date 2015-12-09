module Move
  ( Move
  , addMoveToBoard
  , moveDecoder
  ) where

import Array as Array exposing (Array)
import Char
import Json.Decode as Json exposing ((:=))
import String

import Board exposing (..)

type alias Move =
  { word : String
  , direction : Direction
  , position : Position
  , score : Int
  , tiles : List TileValue
  }

addMoveToBoard : Board -> Move -> Board
addMoveToBoard b m = addMoveTilesToBoard b m.position m.direction m.tiles

addMoveTilesToBoard : Board -> Position -> Direction -> List TileValue -> Board
addMoveTilesToBoard b pos dir tiles =
  let nextPos = movePosition pos dir
  in
  case tiles of
    [] -> b
    (t::ts) -> case getBoardCell b pos of
                 Just Empty -> addMoveTilesToBoard (setBoardCell b pos (Proposed t)) nextPos dir ts
                 _ -> addMoveTilesToBoard b nextPos dir (t::ts)

moveDecoder : Json.Decoder (Array Move)
moveDecoder =
  Json.array <|
    Json.object5 Move
      ("word" := Json.string)
      (Json.map parseDirection ("direction" := Json.string))
      (Json.map convertPosition ("position" := (Json.tuple2 (,) Json.int Json.int)))
      ("score" := Json.int)
      (Json.map (List.map charToTileValue << String.toList) ("tiles" := Json.string))

convertPosition : Position -> Position
convertPosition (x, y) = (y-1, x-1)

