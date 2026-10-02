module Ai where

import Board

possibleSituation :: Board -> Char -> [Board]
possibleSituation board c =
    [ makeMove board x y c
    | (x,y) <- emptyPositions board
    ]

score :: Board -> Int
score board =
    if winCheck board == Just 'O'
    then 1
    else if winCheck board == Just 'X'
    then -1
    else 0

minimax :: Board -> Char -> Int
minimax board player =
    if gameOver board
    then score board
    else if player == 'O'
    then maximum [minimax next 'X' | next <- possibleSituation board 'O']
    else minimum [minimax next 'O' | next <- possibleSituation board 'X']

bestMove :: Board -> (Int, Int)
bestMove board =
    let moves = emptyPositions board
        situations = possibleSituation board 'O'
        scores = map (\b -> minimax b 'X') situations
        bestScore = maximum scores
        bestIndex = head [i | (i, s) <- zip [0..] scores, s == bestScore]
    in moves !! bestIndex