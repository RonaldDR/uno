module Game where

import Common
import Shuffler
import System.Console.ANSI (Color(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White))

initialCardCount :: Int
initialCardCount = 7

initGame :: Int -> State

-- TODO: Implement a method to initialize a new game given n players
initGame n = State { players = initPlayers n,
                     deck = fullDeck,
                     d_stack = [ ] }
------------------------------------------------------------------------
initPlayers :: Int ->[Player]
initPlayers n  
        | (n > 0) = [HPlayer {name = "Player" ++ show n, hand = [ ]}] ++ initPlayers (n-1)
        | otherwise = [ ]
-- TODO: Implement a method to setup the game
setupGame :: State -> IO State
setupGame gs = do
	curr <- shuffleDeck gs
	return (dealCards(curr))
-- Deal Cards
dealCards :: State -> State
dealCards state@State{
				players = plyrs,
				deck = dck,
				d_stack = dstck
				} = State{
						players = dealp dck plyrs,
						deck = dealc dck plyrs,
						d_stack = dstck
					}

dealp :: Deck -> [Player] -> [Player]
dealp [] _ = []
dealp _ [] = []
dealp (c1:c2:c3:c4:c5:c6:c7:cs) (p:ps) = p{hand=[c1,c2,c3,c4,c5,c6,c7]} : dealp cs ps

dealc :: Deck -> [Player] -> Deck
dealc [ ] _ = [ ]
dealc deck [ ] = deck
dealc deck (p:ps) = dealc (drop 7 deck) ps