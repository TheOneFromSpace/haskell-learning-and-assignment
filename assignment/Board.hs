module Board where

-- board is een 2d lijst van locaties en of er de X of O of null in zit
type Board = [[Char]]

emptyBoard :: Board
emptyBoard = 
    [ [' ',' ',' ']
    , [' ',' ',' ']
    , [' ',' ',' ']
    ]

-- functie maakt een nieuwe board aan met context van het oude board en returned deze
makeMove :: Board -> Int ->Int -> Char -> Board
makeMove board x y p =
    take y board
    ++ [replaceAt x (board !! y) p]
    -- haskell maakt een lijst met ook de oude row, deze verwijder je hier
    ++ drop (y+1) board

-- functie verandert een specifieke row
replaceAt :: Int -> [Char] -> Char -> [Char]
replaceAt x row p =
    take x row
    -- maakt een lijst van de player character (X of O) omdat de ++ parameter alleen werkt met lijst functies
    ++ [p]
    ++ drop (x+1) row

printBoard :: Board -> IO ()
printBoard board = do
    putStrLn (board !! 0 !! 0 : " | " ++ [board !! 0 !! 1] ++ " | " ++ [board !! 0 !! 2])
    putStrLn "--+---+--"
    putStrLn (board !! 1 !! 0 : " | " ++ [board !! 1 !! 1] ++ " | " ++ [board !! 1 !! 2])
    putStrLn "--+---+--"
    putStrLn (board !! 2 !! 0 : " | " ++ [board !! 2 !! 1] ++ " | " ++ [board !! 2 !! 2])

emptyPositions :: Board -> [(Int, Int)]
emptyPositions board =
    [ (x,y)
    | y <- [0..2]
    , x<- [0..2]
    , board !! y !! x == ' '
    ]

boardIsEmpty :: Board -> Int -> Int -> Bool
boardIsEmpty board x y =
    board !! x !! y == ' '