--  File     : Proj1.hs
--  Author   : Omja Das <odas@student.unimelb.edu.au>
--  Purpose  : An implementation of a card guessing game

-- | This module implements the card guessing game as described in the
--   project specification.

module Proj1
      ( feedback
      , initialGuess
      , nextGuess
      , GameState
      ) where

import Data.List
import Card

-- | A GameState object. Each GameState contains a list of possible card
--   combinations for future guesses.

data GameState = GameState {notGuessed::[[Card]]}

-- | 'feedback' generates the feedback for a guess given an answer.

feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
feedback targets guesses =
    ( feedback1 targets guesses
    , feedback2 targets guesses
    , feedback3 targets guesses
    , feedback4 targets guesses
    , feedback5 targets guesses
    )

-- | 'feedback1' determines how many cards in the guess are also in the
--   answer.

feedback1 :: [Card] -> [Card] -> Int
feedback1 t g = length (t `intersect` g)

-- | 'feedback2' determines how many cards in the answer have a rank lower
--   than the lowest rank in the guess.

feedback2 :: [Card] -> [Card] -> Int
feedback2 [] _ = 0
feedback2 ((Card _ r) : targets) guesses
    | r < (sguesses !! 0) = 1 + feedback2 targets guesses
    | otherwise           = 0 + feedback2 targets guesses
  where
    sguesses = sort [ rank g | g <- guesses ]

-- | 'feedback3' determines how many cards in the answer have the same rank 
--   as a card in the guess.

feedback3 :: [Card] -> [Card] -> Int
feedback3 targets guesses | otherwise = sum
    [ min (length t_rank) (length g_rank)
    | t_rank <- t_ranks
    , g_rank <- g_ranks
    , t_rank !! 0 == g_rank !! 0
    ]
  where
    t_ranks = group (sort [ rank t | t <- targets ])
    g_ranks = group (sort [ rank g | g <- guesses ])

-- | 'feedback4' determines how many cards in the answer have a rank higher
--   than the lowest rank in the guess.

feedback4 :: [Card] -> [Card] -> Int
feedback4 [] _ = 0
feedback4 ((Card _ r) : targets) guesses
    | r > (sguesses !! 0) = 1 + feedback4 targets guesses
    | otherwise           = 0 + feedback4 targets guesses
  where
    sguesses = reverse (sort [ rank g | g <- guesses ])

-- | 'feedback5' determines how many cards in the answer have the same suit
--   as a card in the guess.

feedback5 :: [Card] -> [Card] -> Int
feedback5 targets guesses = sum
    [ min (length t_suit) (length g_suit)
    | t_suit <- t_suits
    , g_suit <- g_suits
    , t_suit !! 0 == g_suit !! 0
    ]
  where
    t_suits = group (sort [ suit t | t <- targets ])
    g_suits = group (sort [ suit g | g <- guesses ])

-- | 'initialGuess' takes the number of cards in the answer and outputs a
--   pair of a guess and GameSate.

initialGuess :: Int -> ([Card], GameState)
initialGuess n = (guess, (GameState allGuesses))
  where
    allGuesses = subseqOfSize n ([minBound .. maxBound] :: [Card])
    guess      = allGuesses !! (length allGuesses `div` 2)

-- | 'nextGuess' takes a pair of a guess and GameSate and a 5-tuple of Ints
--   representing the feedback from the previous guess. The output is a pair
--   containing a guess and an updated GameState.

nextGuess
    :: ([Card], GameState)
    -> (Int, Int, Int, Int, Int)
    -> ([Card], GameState)
nextGuess (guess, guesses) f = (next, newState)
  where
    newState = GameState (delete guess (notGuessed guesses))
    possibleGuesses = [ g
                      | g <- notGuessed newState
                      , (feedback g guess) == f
                      ]
    next = possibleGuesses !! (length possibleGuesses `div` 2)

-- | 'subseqOfSize' takes an Int (n) and a list and generates all possible
--   subsequences that are n long.

subseqOfSize :: Ord a => Int -> [a] -> [[a]]
subseqOfSize 0 _  = [[]]
subseqOfSize 1 xs = [ [x] | x <- xs ]
subseqOfSize n xs = [ [x] ++ y | x <- xs, y <- minus1, x < (y !! 0) ]
  where
    minus1 = subseqOfSize (n - 1) xs
