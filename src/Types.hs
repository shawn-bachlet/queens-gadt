{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Types where

import Control.Lens ((%~), (&), (.~), (?~), (^.), (^?), _Just, ix, makeLenses, over, Lens')
import Graphics.Gloss
  ( Color,
    Display (InWindow),
    Picture (Blank),
    black,
    color,
    display,
    light,
    pictures,
    rectangleSolid,
    red,
    translate,
    white,
  )
import Data.Map as M ( Map )

data PieceType = King | Queen | Bishop | Rook | Knight | Pawn
  deriving (Show, Eq)

data PieceColor = Black | White
  deriving (Show, Eq)

data Piece
  = Piece
      { _pieceType :: PieceType,
        _pieceColor :: PieceColor,
        _sprite :: Picture
      }
  deriving (Eq, Show)
makeLenses ''Piece

data Assets
  = Assets
      { _blackKing :: Piece,
        _blackQueen :: Piece,
        _blackBishop :: Piece,
        _blackRook :: Piece,
        _blackKnight :: Piece,
        _blackPawn :: Piece,
        _whiteKing :: Piece,
        _whiteQueen :: Piece,
        _whiteBishop :: Piece,
        _whiteRook :: Piece,
        _whiteKnight :: Piece,
        _whitePawn :: Piece
      }
makeLenses ''Assets

data Column = A | B | C | D | E | F | G | H
  deriving (Enum, Show, Eq, Ord)

data Row = Eight | Seven | Six | Five | Four | Three | Two | One
  deriving (Enum, Show, Eq, Ord)

data Square
  = Square
      { _background :: Picture,
        _yOffset :: Float,
        _xOffset :: Float,
        _piece :: Maybe Piece
      }
  deriving (Eq, Show)
makeLenses ''Square

type Space = (Row, Column)

newtype TotalMap k v = TotalMap (k -> v)

lookup :: k -> TotalMap k v -> v
lookup s (TotalMap f) = f s

insert :: Eq k => k -> v -> TotalMap k v -> TotalMap k v
insert k v (TotalMap f) = TotalMap $
  \k' ->
    if k == k'
      then v
      else f k'

update :: Eq k => k -> Lens' v a -> a -> TotalMap k v -> TotalMap k v
update k l a (TotalMap f) = TotalMap $
  \k' ->
    if k == k'
      then f k' & l .~ a
      else f k'

terminal :: v -> TotalMap k v
terminal v = TotalMap $ const v

terminalBoard = terminal $ Square Blank 0 0 Nothing

data GameState
  = GameState
      { _board :: TotalMap Space Square,
        _selectedPiece :: Maybe Square
      }
makeLenses ''GameState
