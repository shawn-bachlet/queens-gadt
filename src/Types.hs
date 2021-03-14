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

newtype TotalMap = TotalMap (Space -> Square)

lookup :: Space -> TotalMap -> Square
lookup s (TotalMap f) = f s

insert :: Space -> Square -> TotalMap -> TotalMap
insert space square (TotalMap f) = TotalMap $
  \space' ->
    if space == space'
      then square
      else f space'

update :: Space -> Lens' Square a -> a -> TotalMap -> TotalMap
update s l a (TotalMap f) = TotalMap $
  \s' ->
    if s == s'
      then f s' & l .~ a
      else f s'

empty :: TotalMap
empty = TotalMap $ \s -> Square Blank 0 0 Nothing

data GameState
  = GameState
      { _board :: TotalMap,
        _selectedPiece :: Maybe Square
      }
makeLenses ''GameState
