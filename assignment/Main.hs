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
        let playerMoveDoneBoard = makeMove board x y 'X'
        if gameOver playerMoveDoneBoard
        then putStrLn "Game over!"
        else do
            let (aiX,aiY) = bestMove playerMoveDoneBoard
            let newBoard = makeMove playerMoveDoneBoard aiX aiY 'O'
            loopGame newBoard