import Board
import Player

main :: IO ()
main = do
    printBoard emptyBoard
    (x,y) <- getMove emptyBoard
    let board1 = makeMove emptyBoard x y 'X'
    printBoard board1
    (x,y) <- getMove board1
    let board2 = makeMove board1 x y 'X'
    printBoard board2

    