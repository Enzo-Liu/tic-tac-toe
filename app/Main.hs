module Main where

import           Lib
import           System.IO

play :: Board -> IO Outcome
play b = do
  putStr "which position you want to go: "
  hFlush stdout
  c <- getLine
  let nx = move b (case c of
        "N" ->  N
        "E" ->  E
        "S" ->  S
        "W" ->  W
        "NE" -> NE
        "NW" -> NW
        "SE" ->  SE
        "SW" -> SW
        _ ->  C)
  print nx
  return nx

runGame :: Board -> Outcome -> IO Outcome
runGame _ (Inplay b) = runWithBoard b
runGame b' InvalidMove = putStrLn "wrong move" >> runWithBoard b'
runGame _ oc = return oc

runWithBoard :: Board -> IO Outcome
runWithBoard b = play b >>= runGame b

gameLoop :: IO Outcome
gameLoop = putStrLn "game starts, X move first" >> runWithBoard emptyBoard

main :: IO ()
main = gameLoop >>= \_-> pure ()
-- main = putStrLn "Hello, what's your name?"   >> getLine >>= (\name -> putStrLn ("Hey " ++ name ++ ", you rock!"))
