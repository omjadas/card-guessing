--  File     : Proj1.hs
--  Author   : Omja Das
--  Purpose  : An implementation of a card guessing game

module Proj1 (feedback,
              initialGuess,
              nextGuess,
              GameState) where

import Data.List
import Card

data GameState = GameState {not_guessed::[[Card]],
                            feedbacks::[(Int, Int, Int, Int, Int)]}

feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
feedback ts gs = (feedback1 ts gs,
                  feedback2 ts gs,
                  feedback3 ts gs,
                  feedback4 ts gs,
                  feedback5 ts gs)

feedback1 :: [Card] -> [Card] -> Int
feedback1 t g = length (intersect t g)

feedback2 :: [Card] -> [Card] -> Int
feedback2 [] _ = 0
feedback2 ((Card _ r):ts) gs
    | r < (sgs !! 0) = 1 + feedback2 ts gs
    | otherwise      = 0 + feedback2 ts gs
  where
    sgs = sort [rank g | g <- gs]

feedback3 :: [Card] -> [Card] -> Int
feedback3 ts gs
    | otherwise = sum [min (length t_rank) (length g_rank) | t_rank <- t_ranks,
                      g_rank <- g_ranks,
                      t_rank !! 0 == g_rank !! 0]
  where
    t_ranks = group (sort [rank t | t <- ts])
    g_ranks = group (sort [rank g | g <- gs])

feedback4 :: [Card] -> [Card] -> Int
feedback4 [] _ = 0
feedback4 ((Card _ r):ts) gs
    | r > (sgs !! 0) = 1 + feedback4 ts gs
    | otherwise      = 0 + feedback4 ts gs
  where
    sgs = reverse (sort [rank g | g <- gs])

feedback5 :: [Card] -> [Card] -> Int
feedback5 ts gs = sum [min (length t_suit) (length g_suit) | t_suit <- t_suits,
                                   g_suit <- g_suits,
                                   t_suit !! 0 == g_suit !! 0]
  where
    t_suits = group (sort [suit t | t <- ts])
    g_suits = group (sort [suit g | g <- gs])

initialGuess :: Int -> ([Card], GameState)
initialGuess x = (guess, (GameState (delete guess all_guesses) []))
  where 
    all_guesses = subseqOfSize x ([minBound..maxBound]::[Card])
    guess = all_guesses !! (length all_guesses `div` 2)

nextGuess :: ([Card], GameState) -> (Int, Int, Int, Int, Int) -> ([Card], GameState)
nextGuess _ _ = ([], (GameState [] [(0,0,0,0,0)]))

subseqOfSize :: Ord a => Int -> [a] -> [[a]]
subseqOfSize 0 _  = [[]]
subseqOfSize 1 xs = [[x] | x <- xs]
subseqOfSize n xs
    | otherwise = [[x] ++ y | x <- xs, y <- minus1, x < (y!!0)]
  where
    minus1 = subseqOfSize (n-1) xs
