{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens ((%~), (&), (.~), (?~), (^.), (^?), _Just, ix, makeLenses, over)
import GameState ( loadInitialState )
import Graphics.Gloss
  ( Display(InWindow), Color, Picture, black, color, display, light, pictures, rectangleSolid, red
  , translate, white
  )
import Graphics.Gloss.Interface.IO.Game
  ( Event(EventKey), Key(MouseButton), KeyState(Up), MouseButton(LeftButton), playIO
  )
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import Data.Map as M (lookup, elems)
import Types
    ( GameState(GameState),
      Square(Square),
      Row(..),
      Column(..),
      sprite, piece )

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
        (\_ w -> pure w)
