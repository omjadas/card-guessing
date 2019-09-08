--  File     : Proj1.hs
--  Author   : Omja Das
--  Purpose  : An implementation of a card guessing game

module Proj1 (feedback, initialGuess, nextGuess, GameState) where

    import Card

    data GameState

    feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)

    initialGuess :: Int -> ([Card], GameState)

    nextGuess :: ([Card], GameState) -> (Int, Int, Int, Int, Int) -> ([Card], GameState)
