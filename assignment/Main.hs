import Board
import Player
import Ai

main :: IO ()
main = loopGame emptyBoard

loopGame :: Board -> IO()
loopGame board = do
    printBoard board

    if gameOver board
    then putStrLn "Game over!"
    else do
        (x,y) <- getMove board
        let newBoard = makeMove board x y 'X'
        loopGame newBoard