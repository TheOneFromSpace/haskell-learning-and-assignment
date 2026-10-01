module Player where

import Board

getMove :: Board -> IO (Int,Int)
getMove board = do
    putStrLn "voer x en y in: "
    input <- getLine
    let [x,y] = map read (words input)
    if squareIsEmpty board x y
    then return (x,y)
    else do 
        putStrLn "ongeldige zet"
        getMove board