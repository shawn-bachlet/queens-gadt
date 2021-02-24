{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens ((%~), (&), (.~), (^.), makeLenses, over)
import Control.Monad.Except (ExceptT (..), runExceptT)
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

data Assets
  = Assets
      { _blackKing :: Picture,
        _blackQueen :: Picture,
        _blackBishop :: Picture,
        _blackRook :: Picture,
        _blackKnight :: Picture,
        _blackPawn :: Picture,
        _whiteKing :: Picture,
        _whiteQueen :: Picture,
        _whiteBishop :: Picture,
        _whiteRook :: Picture,
        _whiteKnight :: Picture,
        _whitePawn :: Picture
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
        _piece :: Maybe Picture
      }
  deriving (Eq, Show)

makeLenses ''Square

newtype GameState
  = GameState
      {_board :: M.Map (Row, Column) Square}

makeLenses ''GameState

-- | loadImage takes a file path and returns Graphics.Gloss.Picture. If the
-- picture cannot be loaded an error message with the failed file will be
-- returned.
loadImage :: FilePath -> ExceptT String IO Picture
loadImage filePath = do
  ExceptT $
    maybe
      (Left $ "failed to load file: " <> filePath)
      Right
      <$> loadJuicyPNG filePath

-- | setup loads all piece sprites from src/sprites
setup :: ExceptT String IO Assets
setup =
  Assets
    <$> loadImage "src/sprites/blackKing.png"
    <*> loadImage "src/sprites/blackQueen.png"
    <*> loadImage "src/sprites/blackBishop.png"
    <*> loadImage "src/sprites/blackRook.png"
    <*> loadImage "src/sprites/blackKnight.png"
    <*> loadImage "src/sprites/blackPawn.png"
    <*> loadImage "src/sprites/whiteKing.png"
    <*> loadImage "src/sprites/whiteQueen.png"
    <*> loadImage "src/sprites/whiteBishop.png"
    <*> loadImage "src/sprites/whiteRook.png"
    <*> loadImage "src/sprites/whiteKnight.png"
    <*> loadImage "src/sprites/whitePawn.png"

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
indices = [(row, clm) | clm <- [A .. H], row <- [Eight .. One]]

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

render :: GameState -> Picture
render (GameState board) =
  pictures
    . map
      ( \(Square b y x p) ->
          maybe
            (translate x y b)
            (\p' -> pictures [translate x y b, translate x y p'])
            p
      )
    $ M.elems board

main :: IO ()
main = do
  eAssets <- runExceptT setup
  case eAssets of
    Left err -> print err
    Right assets -> do
      let initialState = GameState . M.fromList . zip indices $ pieceLayout assets
      display
        (InWindow "Chess" (360, 360) (360, 360))
        red
        (render initialState)


