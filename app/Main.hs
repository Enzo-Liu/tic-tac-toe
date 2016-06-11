module Main where

import           Control.Monad.Loops
import           Lib

getPos :: String -> Maybe Position
getPos = undefined

initGame :: IO Outcome
initGame = do
  putStrLn "game starts, X move first"
  play emptyBoard

play :: Board -> IO Outcome
play b = do
  putStr "which position you want to go: "
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

runGame :: Outcome -> IO Outcome
runGame (Inplay b) = play b

done :: Outcome -> Bool
done (Done _) = True
done _ = False

gameLoop :: IO Outcome
gameLoop = initGame >>= iterateUntilM done runGame

main :: IO ()
main = gameLoop >>= \_-> pure ()
