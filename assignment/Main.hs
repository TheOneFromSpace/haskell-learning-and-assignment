import Board
import Player

main :: IO ()
main = do
    (x,y) <- getMove
    let board1 = makeMove emptyBoard x y 'X'
    printBoard board1
    (x,y) <- getMove
    let board2 = makeMove board1 x y 'X'
    printBoard board2

    