module Lib
    ( start
    , move
    , Position(..)
    , emptyBoard
    , Board
    , Outcome(..)
    ) where

import           Data.Foldable    hiding (foldr)
import           Data.List        (intercalate)
import           Data.Map         (Map)
import qualified Data.Map         as M
import           Data.Traversable
import           Prelude          hiding (all, any, concat, mapM)

data Choice = O | X deriving Eq

instance Show Choice where
  show O = "O"
  show X = "X"

data Board = Board (Map Position Choice) [(Position, Choice)] deriving Eq

instance Show Board where
  show (Board m _) =
    let p x = case M.lookup x m of
                Nothing -> ' '
                Just O -> 'O'
                Just X -> 'X'
        line a b c =
          concat
             [
               "| "
             , [p a]
             , " | "
             , [p b]
             , " | "
             , [p c]
             , " |"
             ]
        blank = ".===.===.===."
    in intercalate "\n"
         [
           blank
         , line NW N NE
         , blank
         , line W C E
         , blank
         , line SW S SE
         , blank
         ]

data Position =
  N | E | S | W | NE | NW | SE | SW | C
  deriving (Enum, Bounded, Ord, Eq, Show, Read)

data FinishedBoard = FinishedBoard (Maybe Choice) Board deriving Eq

instance Show FinishedBoard where
  show (FinishedBoard Nothing b) =
    show b ++ "\nDraw"
  show (FinishedBoard (Just p) b) =
    show b ++ "\n" ++ show p ++ " wins"

data Outcome = InvalidMove | Inplay Board | Done FinishedBoard deriving Eq

instance Show Outcome where
  show InvalidMove =
    "?"
  show (Inplay b) =
    show b
  show (Done b) =
    show b

emptyBoard :: Board
emptyBoard = Board M.empty []

start :: Position -> Board
start p = Board (M.singleton p X) [(p, X)]

move :: Board -> Position -> Outcome
move (Board _ []) p = judge (start p)
move b@(Board m ((_,c):_)) p = if M.member p m then InvalidMove
  else judge (move' b p (switch c) )

switch :: Choice -> Choice
switch X = O
switch O = X

move' :: Board -> Position -> Choice -> Board
move' (Board m moves) p c = Board (M.insert p c m) ((p, c):moves)

wins :: [(Position, Position, Position)]
wins = [ (NW, N, NE)
       , (N,  C, S)
       , (NE, E, SE)
       , (NW, N, NE)
       , (W, C, E)
       , (SW, S, SE)
       , (NW, C, SE)
       , (SW, C, NE)
       , (SW, W, NW)]

judge :: Board -> Outcome
judge b@(Board _ [])  = Inplay b
judge b@(Board m ((_,c):_))
  | isWin = Done (FinishedBoard (Just c) b)
  | isDraw = Done (FinishedBoard Nothing b)
  | otherwise =  Inplay b
  where
        allEqual (a:b':t) = a == b' && allEqual (b':t)
        allEqual _ = True
        isDraw = all (`M.member` m) [minBound..]
        isWin = any (\(a', b', c') -> any allEqual $ mapM (`M.lookup` m) [a', b', c']) wins
