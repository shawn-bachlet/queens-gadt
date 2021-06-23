{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Rendering where

import Control.Lens ((^.))
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownNat, natVal)
import Graphics.Gloss (Picture, color, pictures, translate)
import Graphics.Gloss.Data.Color (black, light, white)
import Graphics.Gloss.Data.Picture (rectangleSolid)
import Types

type Render :: Type -> Constraint
class Render a where
  render :: a -> Picture

instance (KnownColor c, KnownColumn x, KnownRow y) => Render (Square c x y) where
  render (Square mPiece) =
    maybe
      (translate (rowVal @y) (colVal @x) (colorVal @c))
      (\p' ->
        pictures
          [ translate  (rowVal @y) (colVal @x) (colorVal @c)
          , translate  (rowVal @y) (colVal @x) (p' ^. sprite)
          ]
      )
      mPiece

instance Render GameState where
  render gs = render (gs ^. board)

instance Render Board where
  render board = pictures
    [ render $ board ^. a1
    , render $ board ^. a2
    , render $ board ^. a3
    , render $ board ^. a4
    , render $ board ^. a5
    , render $ board ^. a6
    , render $ board ^. a7
    , render $ board ^. a8
    , render $ board ^. b1
    , render $ board ^. b2
    , render $ board ^. b3
    , render $ board ^. b4
    , render $ board ^. b5
    , render $ board ^. b6
    , render $ board ^. b7
    , render $ board ^. b8
    , render $ board ^. c1
    , render $ board ^. c2
    , render $ board ^. c3
    , render $ board ^. c4
    , render $ board ^. c5
    , render $ board ^. c6
    , render $ board ^. c7
    , render $ board ^. c8
    , render $ board ^. d1
    , render $ board ^. d2
    , render $ board ^. d3
    , render $ board ^. d4
    , render $ board ^. d5
    , render $ board ^. d6
    , render $ board ^. d7
    , render $ board ^. d8
    , render $ board ^. e1
    , render $ board ^. e2
    , render $ board ^. e3
    , render $ board ^. e4
    , render $ board ^. e5
    , render $ board ^. e6
    , render $ board ^. e7
    , render $ board ^. e8
    , render $ board ^. f1
    , render $ board ^. f2
    , render $ board ^. f3
    , render $ board ^. f4
    , render $ board ^. f5
    , render $ board ^. f6
    , render $ board ^. f7
    , render $ board ^. f8
    , render $ board ^. g1
    , render $ board ^. g2
    , render $ board ^. g3
    , render $ board ^. g4
    , render $ board ^. g5
    , render $ board ^. g6
    , render $ board ^. g7
    , render $ board ^. g8
    , render $ board ^. h1
    , render $ board ^. h2
    , render $ board ^. h3
    , render $ board ^. h4
    , render $ board ^. h5
    , render $ board ^. h6
    , render $ board ^. h7
    , render $ board ^. h8
    ]

type KnownColumn :: Column -> Constraint
class KnownColumn a where
  colVal :: Float
instance KnownColumn 'A where
  colVal = -157.5
instance KnownColumn 'B where
  colVal = -112.5
instance KnownColumn 'C where
  colVal = -67.5
instance KnownColumn 'D where
  colVal = -22.5
instance KnownColumn 'E where
  colVal = 22.5
instance KnownColumn 'F where
  colVal = 67.5
instance KnownColumn 'G where
  colVal = 112.5
instance KnownColumn 'H where
  colVal = 157.5

type KnownRow :: Row -> Constraint
class KnownRow a where
  rowVal:: Float
instance KnownRow 'One where
  rowVal = -157.5
instance KnownRow 'Two where
  rowVal = -112.5
instance KnownRow 'Three where
  rowVal = -67.5
instance KnownRow 'Four where
  rowVal = -22.5
instance KnownRow 'Five where
  rowVal = 22.5
instance KnownRow 'Six where
  rowVal = 67.5
instance KnownRow 'Seven where
  rowVal = 112.5
instance KnownRow 'Eight where
  rowVal = 157.5

type KnownColor :: PieceColor -> Constraint
class KnownColor a where
  colorVal :: Picture
instance KnownColor 'Black where
  colorVal = color (light . light $ black) $ rectangleSolid 45 45
instance KnownColor 'White where
  colorVal = color white $ rectangleSolid 45 45
