{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE PolyKinds #-}
module Main where

import Control.Lens
  ( (%~), (&), (.~), (?~), (^.), (^?), Lens', _1, _2, _Just, ix, makeLenses, over, set, view
  )
import Control.Monad (join)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import Data.Kind (Type)
import GameState (indices, loadInitialState)
import Graphics.Gloss
  ( Display(InWindow), Color, Picture, black, color, display, light, pictures, rectangleSolid, red
  , translate, white
  )
import Graphics.Gloss.Interface.IO.Game
  ( Event(EventKey), Key(MouseButton), KeyState(Up), MouseButton(LeftButton), playIO
  )
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Movement
import Prelude hiding (lookup)
import Rendering
import Types


handleClick :: Event -> GameState -> IO GameState
handleClick (EventKey (MouseButton LeftButton) Up _ (x, y)) g@(GameState history (Just selection) board ) = do
  let col = lookupCol y
      row = lookupRow x
      Space row' col' = selection ^. _2
      piece = join $ lookup board <$> col <*> row
  case (piece, col, row) of
    (Nothing, Just c, Just r) ->
      pure $ GameState history Nothing
        ( update col' row' Nothing
        . update c r (Just $ selection ^. _1)
        $ board
        )
    _ -> do
      pure $ g & selectedPiece .~ Nothing
handleClick (EventKey (MouseButton LeftButton) Up _ (x, y)) g@(GameState history Nothing board) = do
  let col = lookupCol y
      row = lookupRow x
      piece = join $ lookup board <$> col <*> row
  case (piece, col, row) of
    (Just p, Just c, Just r) -> do
      pure $ GameState history (Just (p, Space r c)) board
    _ -> pure g
handleClick _ state = pure state

lookup :: Board -> Column -> Row -> Maybe Piece
lookup b col row = withSomeSLookup ((\x -> b ^. x . piece) . toLens) (toSLookup col row)

update :: Column -> Row -> Maybe Piece -> Board -> Board
update col row mp b = withSomeSLookup ((\x -> set (x . piece) mp b) . toLens) (toSLookup col row)

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
        (pure.render)
        handleClick
        (const pure)
