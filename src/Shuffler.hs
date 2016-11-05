module Shuffler where

import Common
import System.Random
import System.Random.Shuffle
import Data.List

shuffleDeck :: State -> IO State

-- TODO: Implement a random shuffling algorithm
--shuffleDeck state = return (state)
shuffleDeck state@State{players=pl,
                          deck = dc, 
                          d_stack = dstk } 
                          = return State{
                                     players = pl,
                                     deck =shuffles seed,
                                     d_stack = dstk}

seed = mkStdGen 1000

-- Shuffle function
shuffles :: (RandomGen gen) => gen -> [Card]
shuffles = shuffle' fullDeck 108


