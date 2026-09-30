module Player where

getMove :: IO (Int,Int)
getMove = do
    putStrLn "voer x en y in: "
    input <- getLine
    let [x,y] = map read (words input)
    return (x,y)