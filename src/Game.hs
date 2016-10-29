module Game where

import Common
import System.Console.ANSI (Color(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White))

initGame :: Int -> State

-- TODO: Implement a method to initialize a new game given n players
initGame n = State { players = initPlayers n,
                     deck = fullDeck,
                     d_stack = [ ] }
------------------------------------------------------------------------
initPlayers :: Int ->[Player]
initPlayers n
        | (n > 0) = [HPlayer {name = "Player" ++ show n, hand = [Card {color = Black, value = One}]}] ++ initPlayers (n-1)
        | otherwise = [ ]
-- TODO: Implement a method to setup the game
setupGame :: State -> IO State
setupGame gs = return (gs)
