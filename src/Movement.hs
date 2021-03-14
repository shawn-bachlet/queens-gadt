module Movement where

import Types
import qualified Data.Set as S

canMove :: PieceType -> PieceColor -> Space -> S.Set Space
canMove King _ (row, column) =
  S.fromList
    [ (succ row, column)
    , (pred row, column)
    , (row, succ column)
    , (row, pred column)
    , (succ row, succ column)
    , (succ row, pred column)
    , (pred row, succ column)
    , (pred row, pred column)
    ]
canMove Queen _ (row, column) = S.fromList []
canMove Bishop _ (row, column) = S.fromList []
canMove Rook _ (row, column) = S.fromList []
canMove Knight _ (row, column) = S.fromList []
canMove Pawn Black (row, column) = S.fromList []
canMove Pawn White (row, column) = S.fromList []
