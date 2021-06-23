{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module Types where

import Control.Lens ((%~), (&), (.~), (?~), (^.), (^?), Lens', _Just, ix, makeLenses, over)
import Data.Kind (Type)
import Data.Map as M (Map)
import GHC.TypeLits (Nat)
import Graphics.Gloss
  ( Display(InWindow), Picture(Blank), Color, black, color, display, light, pictures, rectangleSolid
  , red, translate, white
  )

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

type Square :: PieceColor -> Column -> Row -> Type
newtype Square c x y
  = Square
      { _piece :: Maybe Piece
      }
  deriving (Eq, Show)
makeLenses ''Square

data Space = Space Row Column

data Board =
  Board
    { _a1 :: Square 'Black 'A 'One
    , _a2 :: Square 'White 'A 'Two
    , _a3 :: Square 'Black 'A 'Three
    , _a4 :: Square 'White 'A 'Four
    , _a5 :: Square 'Black 'A 'Five
    , _a6 :: Square 'White 'A 'Six
    , _a7 :: Square 'Black 'A 'Seven
    , _a8 :: Square 'White 'A 'Eight
    , _b1 :: Square 'White 'B 'One
    , _b2 :: Square 'Black 'B 'Two
    , _b3 :: Square 'White 'B 'Three
    , _b4 :: Square 'Black 'B 'Four
    , _b5 :: Square 'White 'B 'Five
    , _b6 :: Square 'Black 'B 'Six
    , _b7 :: Square 'White 'B 'Seven
    , _b8 :: Square 'Black 'B 'Eight
    , _c1 :: Square 'Black 'C 'One
    , _c2 :: Square 'White 'C 'Two
    , _c3 :: Square 'Black 'C 'Three
    , _c4 :: Square 'White 'C 'Four
    , _c5 :: Square 'Black 'C 'Five
    , _c6 :: Square 'White 'C 'Six
    , _c7 :: Square 'Black 'C 'Seven
    , _c8 :: Square 'White 'C 'Eight
    , _d1 :: Square 'White 'D 'One
    , _d2 :: Square 'Black 'D 'Two
    , _d3 :: Square 'White 'D 'Three
    , _d4 :: Square 'Black 'D 'Four
    , _d5 :: Square 'White 'D 'Five
    , _d6 :: Square 'Black 'D 'Six
    , _d7 :: Square 'White 'D 'Seven
    , _d8 :: Square 'Black 'D 'Eight
    , _e1 :: Square 'Black 'E 'One
    , _e2 :: Square 'White 'E 'Two
    , _e3 :: Square 'Black 'E 'Three
    , _e4 :: Square 'White 'E 'Four
    , _e5 :: Square 'Black 'E 'Five
    , _e6 :: Square 'White 'E 'Six
    , _e7 :: Square 'Black 'E 'Seven
    , _e8 :: Square 'White 'E 'Eight
    , _f1 :: Square 'White 'F 'One
    , _f2 :: Square 'Black 'F 'Two
    , _f3 :: Square 'White 'F 'Three
    , _f4 :: Square 'Black 'F 'Four
    , _f5 :: Square 'White 'F 'Five
    , _f6 :: Square 'Black 'F 'Six
    , _f7 :: Square 'White 'F 'Seven
    , _f8 :: Square 'Black 'F 'Eight
    , _g1 :: Square 'Black 'G 'One
    , _g2 :: Square 'White 'G 'Two
    , _g3 :: Square 'Black 'G 'Three
    , _g4 :: Square 'White 'G 'Four
    , _g5 :: Square 'Black 'G 'Five
    , _g6 :: Square 'White 'G 'Six
    , _g7 :: Square 'Black 'G 'Seven
    , _g8 :: Square 'White 'G 'Eight
    , _h1 :: Square 'White 'H 'One
    , _h2 :: Square 'Black 'H 'Two
    , _h3 :: Square 'White 'H 'Three
    , _h4 :: Square 'Black 'H 'Four
    , _h5 :: Square 'White 'H 'Five
    , _h6 :: Square 'Black 'H 'Six
    , _h7 :: Square 'White 'H 'Seven
    , _h8 :: Square 'Black 'H 'Eight
    }
makeLenses ''Board

type SLookup :: PieceColor -> Column -> Row -> Type
data SLookup x y z where
  A1 :: SLookup 'Black 'A 'One
  A2 :: SLookup 'White 'A 'Two
  A3 :: SLookup 'Black 'A 'Three
  A4 :: SLookup 'White 'A 'Four
  A5 :: SLookup 'Black 'A 'Five
  A6 :: SLookup 'White 'A 'Six
  A7 :: SLookup 'Black 'A 'Seven
  A8 :: SLookup 'White 'A 'Eight
  B1 :: SLookup 'White 'B 'One
  B2 :: SLookup 'Black 'B 'Two
  B3 :: SLookup 'White 'B 'Three
  B4 :: SLookup 'Black 'B 'Four
  B5 :: SLookup 'White 'B 'Five
  B6 :: SLookup 'Black 'B 'Six
  B7 :: SLookup 'White 'B 'Seven
  B8 :: SLookup 'Black 'B 'Eight
  C1 :: SLookup 'Black 'C 'One
  C2 :: SLookup 'White 'C 'Two
  C3 :: SLookup 'Black 'C 'Three
  C4 :: SLookup 'White 'C 'Four
  C5 :: SLookup 'Black 'C 'Five
  C6 :: SLookup 'White 'C 'Six
  C7 :: SLookup 'Black 'C 'Seven
  C8 :: SLookup 'White 'C 'Eight
  D1 :: SLookup 'White 'D 'One
  D2 :: SLookup 'Black 'D 'Two
  D3 :: SLookup 'White 'D 'Three
  D4 :: SLookup 'Black 'D 'Four
  D5 :: SLookup 'White 'D 'Five
  D6 :: SLookup 'Black 'D 'Six
  D7 :: SLookup 'White 'D 'Seven
  D8 :: SLookup 'Black 'D 'Eight
  E1 :: SLookup 'Black 'E 'One
  E2 :: SLookup 'White 'E 'Two
  E3 :: SLookup 'Black 'E 'Three
  E4 :: SLookup 'White 'E 'Four
  E5 :: SLookup 'Black 'E 'Five
  E6 :: SLookup 'White 'E 'Six
  E7 :: SLookup 'Black 'E 'Seven
  E8 :: SLookup 'White 'E 'Eight
  F1 :: SLookup 'White 'F 'One
  F2 :: SLookup 'Black 'F 'Two
  F3 :: SLookup 'White 'F 'Three
  F4 :: SLookup 'Black 'F 'Four
  F5 :: SLookup 'White 'F 'Five
  F6 :: SLookup 'Black 'F 'Six
  F7 :: SLookup 'White 'F 'Seven
  F8 :: SLookup 'Black 'F 'Eight
  G1 :: SLookup 'Black 'G 'One
  G2 :: SLookup 'White 'G 'Two
  G3 :: SLookup 'Black 'G 'Three
  G4 :: SLookup 'White 'G 'Four
  G5 :: SLookup 'Black 'G 'Five
  G6 :: SLookup 'White 'G 'Six
  G7 :: SLookup 'Black 'G 'Seven
  G8 :: SLookup 'White 'G 'Eight
  H1 :: SLookup 'White 'H 'One
  H2 :: SLookup 'Black 'H 'Two
  H3 :: SLookup 'White 'H 'Three
  H4 :: SLookup 'Black 'H 'Four
  H5 :: SLookup 'White 'H 'Five
  H6 :: SLookup 'Black 'H 'Six
  H7 :: SLookup 'White 'H 'Seven
  H8 :: SLookup 'Black 'H 'Eight

data SomeSLookup where
  SomeSLookup :: SLookup x y z -> SomeSLookup

toLens :: SLookup a b c -> Lens' Board (Square a b c)
toLens A1 = a1
toLens A2 = a2
toLens A3 = a3
toLens A4 = a4
toLens A5 = a5
toLens A6 = a6
toLens A7 = a7
toLens A8 = a8
toLens B1 = b1
toLens B2 = b2
toLens B3 = b3
toLens B4 = b4
toLens B5 = b5
toLens B6 = b6
toLens B7 = b7
toLens B8 = b8
toLens C1 = c1
toLens C2 = c2
toLens C3 = c3
toLens C4 = c4
toLens C5 = c5
toLens C6 = c6
toLens C7 = c7
toLens C8 = c8
toLens D1 = d1
toLens D2 = d2
toLens D3 = d3
toLens D4 = d4
toLens D5 = d5
toLens D6 = d6
toLens D7 = d7
toLens D8 = d8
toLens E1 = e1
toLens E2 = e2
toLens E3 = e3
toLens E4 = e4
toLens E5 = e5
toLens E6 = e6
toLens E7 = e7
toLens E8 = e8
toLens F1 = f1
toLens F2 = f2
toLens F3 = f3
toLens F4 = f4
toLens F5 = f5
toLens F6 = f6
toLens F7 = f7
toLens F8 = f8
toLens G1 = g1
toLens G2 = g2
toLens G3 = g3
toLens G4 = g4
toLens G5 = g5
toLens G6 = g6
toLens G7 = g7
toLens G8 = g8
toLens H1 = h1
toLens H2 = h2
toLens H3 = h3
toLens H4 = h4
toLens H5 = h5
toLens H6 = h6
toLens H7 = h7
toLens H8 = h8

withSomeSLookup :: (forall x y z. SLookup x y z -> r) -> SomeSLookup -> r
withSomeSLookup f (SomeSLookup x) =  f x

toSLookup :: Column -> Row -> SomeSLookup
toSLookup A One = SomeSLookup A1
toSLookup A Two = SomeSLookup A2
toSLookup A Three = SomeSLookup A3
toSLookup A Four = SomeSLookup A4
toSLookup A Five = SomeSLookup A5
toSLookup A Six = SomeSLookup A6
toSLookup A Seven = SomeSLookup A7
toSLookup A Eight = SomeSLookup A8
toSLookup B One = SomeSLookup B1
toSLookup B Two = SomeSLookup B2
toSLookup B Three = SomeSLookup B3
toSLookup B Four = SomeSLookup B4
toSLookup B Five = SomeSLookup B5
toSLookup B Six = SomeSLookup B6
toSLookup B Seven = SomeSLookup B7
toSLookup B Eight = SomeSLookup B8
toSLookup C One = SomeSLookup C1
toSLookup C Two = SomeSLookup C2
toSLookup C Three = SomeSLookup C3
toSLookup C Four = SomeSLookup C4
toSLookup C Five = SomeSLookup C5
toSLookup C Six = SomeSLookup C6
toSLookup C Seven = SomeSLookup C7
toSLookup C Eight = SomeSLookup C8
toSLookup D One = SomeSLookup D1
toSLookup D Two = SomeSLookup D2
toSLookup D Three = SomeSLookup D3
toSLookup D Four = SomeSLookup D4
toSLookup D Five = SomeSLookup D5
toSLookup D Six = SomeSLookup D6
toSLookup D Seven = SomeSLookup D7
toSLookup D Eight = SomeSLookup D8
toSLookup E One = SomeSLookup E1
toSLookup E Two = SomeSLookup E2
toSLookup E Three = SomeSLookup E3
toSLookup E Four = SomeSLookup E4
toSLookup E Five = SomeSLookup E5
toSLookup E Six = SomeSLookup E6
toSLookup E Seven = SomeSLookup E7
toSLookup E Eight = SomeSLookup E8
toSLookup F One = SomeSLookup F1
toSLookup F Two = SomeSLookup F2
toSLookup F Three = SomeSLookup F3
toSLookup F Four = SomeSLookup F4
toSLookup F Five = SomeSLookup F5
toSLookup F Six = SomeSLookup F6
toSLookup F Seven = SomeSLookup F7
toSLookup F Eight = SomeSLookup F8
toSLookup G One = SomeSLookup G1
toSLookup G Two = SomeSLookup G2
toSLookup G Three = SomeSLookup G3
toSLookup G Four = SomeSLookup G4
toSLookup G Five = SomeSLookup G5
toSLookup G Six = SomeSLookup G6
toSLookup G Seven = SomeSLookup G7
toSLookup G Eight = SomeSLookup G8
toSLookup H One = SomeSLookup H1
toSLookup H Two = SomeSLookup H2
toSLookup H Three = SomeSLookup H3
toSLookup H Four = SomeSLookup H4
toSLookup H Five = SomeSLookup H5
toSLookup H Six = SomeSLookup H6
toSLookup H Seven = SomeSLookup H7
toSLookup H Eight = SomeSLookup H8

data GameState
  = GameState
      { _moveHistory :: [(Piece, Space)]
      , _selectedPiece :: Maybe (Piece, Space)
      , _board :: Board
      }
makeLenses ''GameState
