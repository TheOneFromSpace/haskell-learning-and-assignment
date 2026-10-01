module Ai where

import Board

possibleSituation :: Board -> Char -> [Board]
possibleSituation board c =
    [ makeMove board x y c
    | (x,y) <- emptyPositions board
    ]

score :: Board -> Int
score board =
    if winCheck board == just 'O'
    then 1
    else if winCheck board == Just 'X'
    then -1
    else 0