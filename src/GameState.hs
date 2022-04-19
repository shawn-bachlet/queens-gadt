module GameState where

import Control.Lens ((%~), (&), (.~), (?~), (^.), (^?), _Just, ix, makeLenses, over)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import qualified Data.Map as M
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
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Types
  ( Assets (Assets),
    Column (A, G, H),
    GameState (GameState),
    Piece (Piece),
    PieceColor (Black, White),
    PieceType (Bishop, King, Knight, Pawn, Queen, Rook),
    Row (Eight, One),
    Square (Square),
    blackBishop,
    blackKing,
    blackKnight,
    blackPawn,
    blackQueen,
    blackRook,
    piece,
    whiteBishop,
    whiteKing,
    whiteKnight,
    whitePawn,
    whiteQueen,
    whiteRook,
  )

-- | loadImage takes a file path and returns Graphics.Gloss.Picture. If the
-- picture cannot be loaded an error message with the failed file will be
-- returned.
loadImage :: FilePath -> ExceptT String IO Picture
loadImage filePath =
  ExceptT $
    maybe
      (Left $ "failed to load file: " <> filePath)
      Right
      <$> loadJuicyPNG filePath

-- | setup loads all piece sprites from src/sprites
setup :: ExceptT String IO Assets
setup =
  Assets
    <$> fmap (Piece King Black) (loadImage "src/sprites/blackKing.png")
    <*> fmap (Piece Queen Black) (loadImage "src/sprites/blackQueen.png")
    <*> fmap (Piece Bishop Black) (loadImage "src/sprites/blackBishop.png")
    <*> fmap (Piece Rook Black) (loadImage "src/sprites/blackRook.png")
    <*> fmap (Piece Knight Black) (loadImage "src/sprites/blackKnight.png")
    <*> fmap (Piece Pawn Black) (loadImage "src/sprites/blackPawn.png")
    <*> fmap (Piece King White) (loadImage "src/sprites/whiteKing.png")
    <*> fmap (Piece Queen White) (loadImage "src/sprites/whiteQueen.png")
    <*> fmap (Piece Bishop White) (loadImage "src/sprites/whiteBishop.png")
    <*> fmap (Piece Rook White) (loadImage "src/sprites/whiteRook.png")
    <*> fmap (Piece Knight White) (loadImage "src/sprites/whiteKnight.png")
    <*> fmap (Piece Pawn White) (loadImage "src/sprites/whitePawn.png")

-- | The color on the end of each row is repeated when the row is unrolled. So I
-- enumerated 2 full rows until a cycle occurs
checkers :: [Color]
checkers =
  take 64 $
    cycle
      [ light . light $ black,
        white,
        light . light $ black,
        white,
        light . light $ black,
        white,
        light . light $ black,
        white,
        white,
        light . light $ black,
        white,
        light . light $ black,
        white,
        light . light $ black,
        white,
        light . light $ black
      ]

-- | indices generates all the square names used for chess algebraic notation
indices :: [(Row, Column)]
indices = [(row, clm) | row <- [One .. Eight], clm <- [A .. H]]

emptyBoard :: [Square]
emptyBoard =
  zipWith
    (\(x, y) clr -> Square (color clr $ rectangleSolid 45 45) x y Nothing)
    [(x, y) | x <- take 8 (iterate (+ 45) (-157.5)), y <- take 8 (iterate (+ 45) (-157.5))]
    checkers

pieceLayout :: Assets -> [Square]
pieceLayout assets =
  zipWith
    (\p sqr -> sqr & piece .~ p)
    ( [ Just $ assets ^. whiteRook,
        Just $ assets ^. whiteKnight,
        Just $ assets ^. whiteBishop,
        Just $ assets ^. whiteQueen,
        Just $ assets ^. whiteKing,
        Just $ assets ^. whiteBishop,
        Just $ assets ^. whiteKnight,
        Just $ assets ^. whiteRook
      ]
        <> replicate 8 (Just $ assets ^. whitePawn)
        <> replicate 32 Nothing
        <> replicate 8 (Just $ assets ^. blackPawn)
        <> [ Just $ assets ^. blackRook,
             Just $ assets ^. blackKnight,
             Just $ assets ^. blackBishop,
             Just $ assets ^. blackQueen,
             Just $ assets ^. blackKing,
             Just $ assets ^. blackBishop,
             Just $ assets ^. blackKnight,
             Just $ assets ^. blackRook
           ]
    )
    emptyBoard

loadInitialState :: ExceptT String IO GameState
loadInitialState =
  flip GameState Nothing . foldr (\(space, square) m -> M.insert space square m) M.empty . zip indices . pieceLayout <$> setup
