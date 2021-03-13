{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens ((%~), (&), (.~), (?~), (^.), (^?), _Just, ix, makeLenses, over)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Time (UTCTime)
import Graphics.Gloss
  ( Display(InWindow), Color, Picture, black, color, display, light, pictures, rectangleSolid, red
  , translate, white
  )
import Graphics.Gloss.Interface.IO.Game
  ( Event(EventKey), Key(MouseButton), KeyState(Up), MouseButton(LeftButton), playIO
  )
import Graphics.Gloss.Juicy (loadJuicyPNG)
import qualified Data.Map as M

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

data Column = A | B | C | D | E | F | G | H -- Reverse this enum
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
      deriving (Show)

makeLenses ''Piece
makeLenses ''Assets
makeLenses ''Square
makeLenses ''GameState

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
indices = [(row, clm) | clm <- [H,G .. A], row <- [Eight .. One]]

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
render (GameState board _) =
  pictures
    . map
      ( \(Square b y x p) ->
          maybe
            (translate x y b)
            (\p' -> pictures [translate x y b, translate x y p'])
            (p ^? _Just . sprite)
      )
    $ M.elems board

handleClick :: Event -> GameState -> IO GameState
handleClick (EventKey (MouseButton LeftButton) Up _ (x, y)) g@(GameState board (Just piece')) = do
  print (x,y)
  print (lookupRow x, lookupCol y)
  print "-------------"
  case M.lookup (lookupRow x, lookupCol y) board of
    Just sqr ->
      case sqr ^. piece of
        Just p -> do
          print 1
          pure g
        Nothing ->
          pure $ GameState (board & ix (lookupRow x, lookupCol y) . piece ?~ piece') Nothing
    Nothing -> pure g
handleClick (EventKey (MouseButton LeftButton) Up _ (x, y)) g@(GameState board Nothing) = do
  print (x,y)
  print (lookupRow x, lookupCol y)
  print "-------------"
  case M.lookup (lookupRow x, lookupCol y) board of
    Just sqr ->
      case sqr ^. piece of
        Just p -> do
          print 0
          pure $ GameState board (Just p)
        Nothing -> pure g
    Nothing -> pure g
handleClick _ state = pure state

lookupCol :: Float -> Column
lookupCol x
  | x >= (-180) && x < (-135) = H
  | x >= (-135) && x < (-90) = G
  | x >= (-90) && x < (-45) = F
  | x >= (-45) && x < 0 = E
  | x >= 0 && x < 45 = D
  | x >= 45 && x < 90 = C
  | x >= 90 && x < 135 = B
  | otherwise = A

lookupRow :: Float -> Row
lookupRow y
  | y >= (-180) && y < (-135) = Eight
  | y >= (-135) && y < (-90) = Seven
  | y >= (-90) && y < (-45) = Six
  | y >= (-45) && y < 0 = Five
  | y >= 0 && y < 45 = Four
  | y >= 45 && y < 90 = Three
  | y >= 90 && y < 135 = Two
  | otherwise = One

main :: IO ()
main = do
  eAssets <- runExceptT setup
  case eAssets of
    Left err -> print err
    Right assets -> do
      let initialState = flip GameState Nothing . M.fromList . zip indices $ pieceLayout assets
      playIO
        (InWindow "Chess" (360, 360) (360, 360))
        red
        5
        initialState
        (pure . render)
        handleClick
        (\_ w -> pure w)
