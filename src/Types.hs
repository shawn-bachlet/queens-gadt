{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens ((%~), (&), (.~), (?~), (^.), (^?), _Just, ix, makeLenses, over)
import Graphics.Gloss
  ( Color,
    Display (InWindow),
    Picture,
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
import Data.Map as M

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

data GameState
  = GameState
      { _board :: M.Map (Row, Column) Square,
        _selectedPiece :: Maybe Piece
      }

makeLenses ''Piece

makeLenses ''Assets

makeLenses ''Square

makeLenses ''GameState

