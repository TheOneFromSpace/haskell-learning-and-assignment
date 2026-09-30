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