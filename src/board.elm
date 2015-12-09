module Board
  ( Board, BoardCell(..), TileValue(..)
  , Dimension, Position, Direction(..)
  , emptyBoard, getBoardCell, setBoardCell
  , parseBoard, boardToString, boardCellToString
  , charToTileValue, convertMoveToPlacement
  , parseDirection, movePosition, normalisePosition
  , toggleDirection, oppositeDirection
  ) where

import Array as Array exposing (Array)
import Char
import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Num exposing (..)
import String

type alias Dimension = (Int, Int)
type alias Position = (Int, Int)
type Direction = Up | Down | Left | Right

type alias Board = Array (Array BoardCell)

type BoardCell = Empty | Full TileValue | Proposed TileValue

type TileValue = BlankT Char | MarkedT Char


emptyBoard : Dimension -> Board
emptyBoard (x, y) = Array.repeat y (Array.repeat x Empty)

getBoardCell : Board -> Position -> Maybe BoardCell
getBoardCell board (x, y) =
  let row = Array.get y board
  in
  case row of
    Nothing -> Nothing
    Just a -> Array.get x a

setBoardCell : Board -> Position -> BoardCell -> Board
setBoardCell board (x, y) cell =
  let row = Array.get y board
  in
  case row of
    Nothing -> board -- error
    Just r -> Array.set y (Array.set x cell r) board

parseBoard : Dimension -> String -> Board
parseBoard d s =
  case parse (pBoard d) s of
    (Done b, ctx) -> b
    (Fail e, ctx) -> emptyBoard d

pBoard : Dimension -> Parser Board
pBoard (h, w) =
  let buildArray xs = Array.fromList (List.map Array.fromList xs)
  in
  (count h (pRow w pBoardCell)) `andThen` (\ms ->
  succeed (buildArray ms))

pBoardCell : Parser BoardCell
pBoardCell =
  ((char '.') `andThen` (\_ -> succeed Empty)) `or`
  (lower `andThen` (\c -> succeed (Full (MarkedT c)))) `or`
  (upper `andThen` (\c -> succeed (Full (BlankT c))))

pRow : Int -> Parser a -> Parser (List a)
pRow n p =
  (count n p) `andThen` (\rs ->
  eol `andThen` (\_ ->
  succeed rs))

count : Int -> Parser a -> Parser (List a)
count n p =
  let accumulate x acc =
    if x <= 0
      then succeed (List.reverse acc)
      else p `andThen` (\a -> accumulate (x-1) (a::acc))
  in
  accumulate n []

boardToString : Board -> String
boardToString board =
  let rowToString row = Array.foldr (++) "\n" (Array.map (boardCellToString ".") row)
  in
  Array.foldr (++) "" (Array.map rowToString board)

charToTileValue : Char -> TileValue
charToTileValue c =
  if Char.isLower c
    then MarkedT c
    else BlankT c

boardCellToString : String -> BoardCell -> String
boardCellToString empty bc =
  case bc of
    Empty -> empty
    Full (MarkedT c) -> String.fromChar c
    Full (BlankT c) -> String.fromChar c
    Proposed (MarkedT c) -> String.fromChar c
    Proposed (BlankT c) -> String.fromChar c

convertMoveToPlacement : BoardCell -> BoardCell
convertMoveToPlacement cell =
  case cell of
    Proposed t -> Full t
    _ -> cell

parseDirection : String -> Direction
parseDirection dir =
  case dir of
    "Up" -> Up
    "Down" -> Down
    "Left" -> Left
    "Right" -> Right
    _ -> Right -- error

movePosition : Position -> Direction -> Position
movePosition (x, y) dir =
  case dir of
    Up -> (x, y-1)
    Down -> (x, y+1)
    Left -> (x-1, y)
    Right -> (x+1, y)

normalisePosition : Dimension -> Position -> Position
normalisePosition (h, w) (x, y) =
  let normalisedX = bound 0 (h-1) x
      normalisedY = bound 0 (w-1) y
  in
  (normalisedX, normalisedY)

bound : Int -> Int -> Int -> Int
bound lower upper value =
  if value < lower then
    lower
  else if value > upper then
    upper
  else
    value

toggleDirection : Direction -> Direction
toggleDirection d =
  case d of
    Right -> Down
    Down -> Right
    _ -> d -- Cursor direction can only be Right or Down

oppositeDirection : Direction -> Direction
oppositeDirection d =
  case d of
    Up -> Down
    Down -> Up
    Left -> Right
    Right -> Left

