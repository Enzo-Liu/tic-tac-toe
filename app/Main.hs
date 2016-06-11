module Main where

import           Lib
import           System.IO

parsePosition :: String -> Maybe Position
parsePosition str = case reads str of
  [(p, "")] -> Just p
  _ -> Nothing

play :: Board -> IO Outcome
play b = do
  putStr "which position you want to go: "
  hFlush stdout
  c <- getLine
  let nx = case parsePosition c of
        Just p -> move b p
        _ -> InvalidMove
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
