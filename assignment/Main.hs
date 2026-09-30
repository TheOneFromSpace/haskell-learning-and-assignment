import Board

main :: IO ()
main = do
    let board1 = makeMove emptyBoard 0 0 'X'
    let board2 = makeMove board1 2 1 'X'

    print board2