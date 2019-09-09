--  File     : Proj1.hs
--  Author   : Omja Das
--  Purpose  : An implementation of a card guessing game

module Proj1 (feedback, initialGuess, nextGuess, GameState) where

    import Data.List
    import Card

    data GameState = GameState Int

    feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
    feedback t g = (feedback1 t g,
                    feedback2 t g,
                    feedback3 t g,
                    feedback4 t g,
                    feedback5 t g)

    feedback1 :: [Card] -> [Card] -> Int
    feedback1 t g = length (intersect t g)

    feedback2 :: [Card] -> [Card] -> Int
    feedback2 [] _ = 0
    feedback2 ((Card _ r):ts) gs
        | r < (sgs !! 0) = 1 + feedback2 ts gs
        | otherwise      = 0 + feedback2 ts gs
        where sgs = sort [r | (Card _ r) <- gs]

    feedback3 :: [Card] -> [Card] -> Int
    feedback3 t g
        | otherwise = sum [min (length tr) (length gr) | tr <- trs, gr <- grs, tr !! 0 == gr !! 0]
        where trs = group (sort [r | (Card _ r) <- t])
              grs = group (sort [r | (Card _ r) <- g])
 
    feedback4 :: [Card] -> [Card] -> Int
    feedback4 [] _ = 0
    feedback4 ((Card _ r):ts) gs
        | r > (sgs !! 0) = 1 + feedback4 ts gs
        | otherwise      = 0 + feedback4 ts gs
        where sgs = reverse (sort [r | (Card _ r) <- gs])

    feedback5 :: [Card] -> [Card] -> Int
    feedback5 t g
        | otherwise = sum [min (length ts) (length gs) | ts <- tss, gs <- gss, ts !! 0 == gs !! 0]
        where tss = group (sort [s | (Card s _) <- t])
              gss = group (sort [s | (Card s _) <- g])

    initialGuess :: Int -> ([Card], GameState)
    initialGuess _ = ([], GameState 0)

    nextGuess :: ([Card], GameState) -> (Int, Int, Int, Int, Int) -> ([Card], GameState)
    nextGuess _ _ = ([], GameState 0)
