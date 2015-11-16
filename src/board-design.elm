module BoardDesign
  ( BoardDesign
  , TileMultiplier(..)
  , boardDesigns
  , emptyBoardDesign
  , getMultiplierCell
  ) where

import Array as Array exposing (Array)
import Parser exposing (..)
import Parser.Char exposing (..)
import Parser.Number exposing (..)
import Dict as Dict exposing (Dict)

import Board exposing (Dimension, Position)

type TileMultiplier = None | DL | TL | DW | TW

type alias BoardDesign =
  { dimension: Dimension
  , letterScores: Dict Char Int
  , bonus: Int
  , multipliers: Array (Array TileMultiplier)
  , name : String
  , asString: String
  }


emptyBoardDesign : BoardDesign
emptyBoardDesign =
  let (h, w) = (15, 15)
  in
  { dimension = (h, w)
  , letterScores = Dict.empty
  , bonus = 0
  , multipliers = Array.repeat h (Array.repeat w None)
  , name = ""
  , asString = ""
  }

getMultiplierCell : BoardDesign -> Position -> Maybe TileMultiplier
getMultiplierCell bd (x, y) =
  let row = Array.get y bd.multipliers
  in
  case row of
    Nothing -> Nothing
    Just a -> Array.get x a

boardDesigns : Dict String BoardDesign
boardDesigns =
  let scrabbleBD = parse parseBoardDesign scrabbleBoardDesign
      wordsWithFriendsBD = parse parseBoardDesign wordsWithFriendsBoardDesign
      designs = [ ("Scrabble", scrabbleBoardDesign, scrabbleBD)
                , ("Words-with-friends", wordsWithFriendsBoardDesign, wordsWithFriendsBD) ]
  in
  List.foldl addParsedBoards Dict.empty designs

addParsedBoards : (String, String, Result String BoardDesign) -> Dict String BoardDesign -> Dict String BoardDesign
addParsedBoards (id, raw, parsed) dict =
 let bd = Result.map (\r -> { r | name <- id, asString <- raw }) parsed
 in
 case bd of
   Ok r -> Dict.insert r.name r dict
   Err e -> dict -- error

scrabbleBoardDesign : String
scrabbleBoardDesign = """15 15
a=1 b=3 c=3 d=2 e=1 f=4 g=2 h=4 i=1 j=8 k=5 l=1 m=3 n=1 o=1 p=3 q=10 r=1 s=1 t=1 u=1 v=4 w=4 x=8 y=4 z=10
50
T..2...T...2..T
.D...3...3...D.
..D...2.2...D..
2..D...2...D..2
....D.....D....
.3...3...3...3.
..2...2.2...2..
T..2...D...2..T
..2...2.2...2..
.3...3...3...3.
....D.....D....
2..D...2...D..2
..D...2.2...D..
.D...3...3...D.
T..2...T...2..T
"""

wordsWithFriendsBoardDesign : String
wordsWithFriendsBoardDesign = """15 15
a=1 b=4 c=4 d=2 e=1 f=4 g=3 h=3 i=1 j=10 k=5 l=2 m=4 n=2 o=1 p=4 q=10 r=1 s=1 t=1 u=2 v=5 w=4 x=8 y=3 z=10
35
...T..3.3..T...
..2..D...D..2..
.2..2.....2..2.
T..3...D...3..T
..2...2.2...2..
.D...3...3...D.
3...2.....2...3
...D.......D...
3...2.....2...3
.D...3...3...D.
..2...2.2...2..
T..3...D...3..T
.2..2.....2..2.
..2..D...D..2..
...T..3.3..T...
"""

parseBoardDesign : Parser BoardDesign
parseBoardDesign =
  pDimension `andThen` (\d ->
  pLetterScores `andThen` (\ls ->
  pBonus `andThen` (\b ->
  (pMultiplierCells d) `andThen` (\ms ->
  succeed { dimension = d
          , letterScores = ls
          , bonus = b
          , multipliers = ms
          , name = ""
          , asString = "" }))))

pDimension : Parser Dimension
pDimension =
  natural `andThen` (\h ->
  (some space) `andThen` (\_ ->
  natural `andThen` (\w ->
  eol `andThen` (\_ ->
  succeed (h, w)))))

pLetterScore : Parser (Char, Int)
pLetterScore =
  lower `andThen` (\c ->
  (symbol '=') `andThen` (\_ ->
  natural `andThen` (\s ->
  succeed (c, s))))

pLetterScores : Parser (Dict Char Int)
pLetterScores =
  (pLetterScore `separatedBy` space) `andThen` (\ls ->
  eol `andThen` (\_ ->
  succeed (Dict.fromList ls)))

pCharToMultiplier : Char -> TileMultiplier -> Parser TileMultiplier
pCharToMultiplier c t =
  (symbol c) `andThen` (\_ ->
  succeed t)

pTileMultiplier : Parser TileMultiplier
pTileMultiplier =
  pCharToMultiplier '.' None `or`
  pCharToMultiplier '2' DL `or`
  pCharToMultiplier '3' TL `or`
  pCharToMultiplier 'D' DW `or`
  pCharToMultiplier 'T' TW

pRow : Int -> Parser a -> Parser (List a)
pRow n p =
  (count n p) `andThen` (\rs ->
  eol `andThen` (\_ ->
  succeed rs))

pBonus : Parser Int
pBonus =
  natural `andThen` (\b ->
  eol `andThen` (\_ ->
  succeed b))

pMultiplierCells : Dimension -> Parser (Array (Array TileMultiplier))
pMultiplierCells (h, w) =
  let buildArray xs = Array.fromList (List.map Array.fromList xs)
  in
  (count h (pRow w pTileMultiplier)) `andThen` (\ms ->
  succeed (buildArray ms))

space : Parser Char
space = symbol ' '

eol : Parser Char
eol = symbol '\n'

count : Int -> Parser a -> Parser (List a)
count n p =
  let accumulate x acc =
    if x <= 0
      then succeed (List.reverse acc)
      else p `andThen` (\a -> accumulate (x-1) (a::acc))
  in
  accumulate n []

