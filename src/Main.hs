{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Lens ((%~), (&), (.~), (?~), (^.), (^?), _Just, ix, makeLenses, over)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.Map (lookup, update)
import Data.Maybe (mapMaybe)
import GameState (indices, loadInitialState)
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
import Graphics.Gloss.Interface.IO.Game
  ( Event (EventKey),
    Key (MouseButton),
    KeyState (Up),
    MouseButton (LeftButton),
    playIO,
  )
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Movement
import Types
  ( Column (..),
    GameState (GameState),
    Row (..),
    Square (Square),
    piece,
    selectedPiece,
    sprite,
    xOffset,
    yOffset,
  )
import Prelude hiding (lookup)

render :: GameState -> Picture
render (GameState board _) =
  let squares = mapMaybe (flip lookup board) $ indices
   in pictures
        . map
          ( \(Square b y x p) ->
              maybe
                (translate x y b)
                (\p' -> pictures [translate x y b, translate x y p'])
                (p ^? _Just . sprite)
          )
        $ squares

handleClick :: Event -> GameState -> IO GameState
handleClick (EventKey (MouseButton LeftButton) Up _ (y, x)) g@(GameState board (Just selection)) = do
  print  (lookupRow x, lookupCol y)
  pure g
--   case (lookupRow x, lookupCol y) of
--     (Just x', Just y') -> do
--       print (x', y')
--       case lookup (x', y') board of
--         Just sqr ->
--           case sqr ^. piece of
--             Just p -> do
--               print p
--               pure $ g & selectedPiece .~ Nothing
--             Nothing -> do
--               case (lookupRow (selection ^. xOffset), lookupCol (selection ^. yOffset)) of
--                 (Just sx, Just sy) ->
--                   pure $
--                     GameState
--                       ( update (const . Just $ sqr & piece .~ Nothing) (sx, sy)
--                           . update (const . Just $ sqr & piece .~ selection ^. piece) (x', y')
--                           $ board
--                       )
--                       Nothing
--                 _ -> pure $ g & selectedPiece .~ Nothing
--         Nothing -> pure $ g & selectedPiece .~ Nothing
--     _ -> pure $ g & selectedPiece .~ Nothing
handleClick (EventKey (MouseButton LeftButton) Up _ (y, x)) g@(GameState board Nothing) = do
  print  (lookupRow x, lookupCol y)
  pure g
--  lookup (lookupRow x, lookupCol y) board &
--      \sqr -> case sqr ^. piece of
--        Just p -> do
--          print p
--          pure $ GameState board (Just sqr)
--        Nothing -> pure g
handleClick _ state = pure state

lookupCol :: Float -> Maybe Column
lookupCol x
  | x >= (-180) && x < (-135) = Just A
  | x >= (-135) && x < (-90) = Just B
  | x >= (-90) && x < (-45) = Just C
  | x >= (-45) && x < 0 = Just D
  | x >= 0 && x < 45 = Just E
  | x >= 45 && x < 90 = Just F
  | x >= 90 && x < 135 = Just G
  | x >= 135 && x < 180 = Just H
  | otherwise = Nothing

lookupRow :: Float -> Maybe Row
lookupRow y
  | y >= (-180) && y < (-135) = Just One
  | y >= (-135) && y < (-90) = Just Two
  | y >= (-90) && y < (-45) = Just Three
  | y >= (-45) && y < 0 = Just Four
  | y >= 0 && y < 45 = Just Five
  | y >= 45 && y < 90 = Just Six
  | y >= 90 && y < 135 = Just Seven
  | y >= 135 && y < 180 = Just Eight
  | otherwise = Nothing

main :: IO ()
main = do
  initialState <- runExceptT loadInitialState
  case initialState of
    Left err -> print err
    Right initialState' -> do
      playIO
        (InWindow "Chess" (360, 360) (360, 360))
        red
        5
        initialState'
        (pure . render)
        handleClick
        (const pure)
