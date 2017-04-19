module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List

--  File       : Proj1.hs
--  Author     : Lan Yang
--  Student Id : 746569
--  User name  : lany4
--  Purpose    : An implementation of card guessing game

-------------------------------------------------------------------------------
-- This file contain three main functions : feedback , initialGuess and  
-- nextGuess, feedback function will give a quintuple of counts of feedback
-- which are correct cards , lower ranks , correct ranks , hihg ranks and
-- correct suits. 
-- The initialGuess function returns a list of card which length depend on how 
-- many cards in answer and a gamestate which contain all possiable combination
-- of possiable answer.
-- The nextGuess function returns a list of card which would be the card I 
-- choose as next guess and gamestate which is potential correct conbination I
-- filter out.
-------------------------------------------------------------------------------

data GameState = Candidate [[Card]]
               deriving (Show)
-------------------------------------------------------------------------------
-- The feedback function contain seven functions to support it. For each int in
-- result of feedback, I can call those function to calculateit. 
-------------------------------------------------------------------------------

feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback answer guess = (a,b,c,d,e)
            where a = corCard answer guess
                  b = lowRank answer guess
                  c = corRank answer guess
                  d = highRank answer guess
                  e = corSuit answer guess

-------------------------------------------------------------------------------
-- Those two function are used to get the suits or ranks of a list cards.
-------------------------------------------------------------------------------
getSuit :: [Card] -> [Suit]
getSuit [] = []
getSuit anyList = [suit (head anyList)] ++ getSuit (tail anyList)

getRank :: [Card] -> [Rank]
getRank [] = []
getRank anyList = [rank (head anyList)] ++ getRank (tail anyList)

-------------------------------------------------------------------------------
-- This function can calculate the correct cards by comparing each card in 
-- answer with guess list. If it is in the guess list, delete this card in both
-- list, continue to compare next card in answer with rest in guess and count+1 
-- If it's not, just compare next card in answer with same guess(don't delete 
-- anything in guess) and nothing change on count. 
-------------------------------------------------------------------------------
corCard :: [Card] -> [Card] -> Int
-- This calculate correct cards.
corCard [] _ = 0
corCard answer guess = 
      if (head answer) `elem` guess 
        then 
          1 + (corCard (tail answer) (delete (head answer) guess))
        else 
          0 + (corCard (tail answer) guess)

-------------------------------------------------------------------------------
-- Those two functions can calculate the lower ranks and higher ranks by 
-- comparing rank of each card in answer with max-rank or min-rank card in 
-- guess. If we get one rank in answer lower than min-rank in guess, count+1.
-- higher ranks works same way as lower ranks.
-------------------------------------------------------------------------------
lowRank :: [Card] -> [Card] -> Int
-- This calculate lower ranks.
lowRank [] _ = 0
lowRank answer guess = 
      if (head (getRank answer)) < (minimum (getRank guess))
        then
          1 + (lowRank (tail answer) guess)
        else
          0 + (lowRank (tail answer) guess)

highRank :: [Card] -> [Card] -> Int
-- This calculate higher ranks.
highRank [] _ = 0
highRank answer guess = 
      if (head (getRank answer)) > (maximum (getRank guess))
        then
          1 + (highRank (tail answer) guess)
        else
          0 + (highRank (tail answer) guess)

-------------------------------------------------------------------------------
-- Those two functions can calculate the number of correct ranks or suits by 
-- comparing rank or suit in answer and guess.First we need to sort those list,
-- for comparing suit we can just sort the answer or guess but , for rank , we
-- need to use sortBy which allowed us to sort by rank. After sorting, we 
-- compare each element in answer and guess, for each comparison, we delete
-- the smaller one from their list,and pass them to next comparison. Only we 
-- find a match, count+1.  
-------------------------------------------------------------------------------
corRank :: [Card] -> [Card] -> Int
-- This calculate correct ranks.
corRank _ [] = 0
corRank [] _ = 0
corRank answer guess 
  |rank (head ans) < rank (head gue)   = 0 + corRank (tail ans) gue
  -- Delete the smaller one
  |rank (head ans) == rank (head gue)  = 1 + corRank (tail ans) (tail gue)
  -- match! then count +1 
  |rank (head ans) > rank(head gue)    = 0 + corRank ans (tail gue)
  -- Delete the smaller one
  where ans = sortBy (\(Card s1 r1) (Card s2 r2)-> r1 `compare` r2) answer
        gue = sortBy (\(Card s1' r1') (Card s2' r2')-> r1' `compare` r2') guess
-- Use sortBy to let it sort by second element of (Card Suit Rank)

corSuit :: [Card] -> [Card] -> Int
-- This calculate correct suits.
corSuit _ [] = 0
corSuit [] _ = 0
corSuit answer guess 
   |suit (head ans) < suit (head gue)   = 0 + corSuit (tail ans) gue
   -- Delete the smaller one
   |suit (head ans) == suit (head gue)  = 1 + corSuit (tail ans) (tail gue)
   -- match! then count +1 
   |suit (head ans) > suit(head gue)    = 0 + corSuit ans (tail gue)
   -- Delete the smaller one
   where ans = sort answer
         gue = sort guess
-- In this case sort will according to first elemnt of (Card Suit Rank)

-------------------------------------------------------------------------------
-- The initialGuess function has one function(initialCandiate) to support it. 
-- For first guess, since we only need to consider three case: 2cards, 3cards
-- and 4cards, so I given a fixed combination card to each case.The combination
-- choose is follow as Hint4,for n card ansewerchoose ranks that are about 
-- 13/(n + 1) ranks apart.
-------------------------------------------------------------------------------

initialGuess :: Int -> ([Card],GameState)
initialGuess n 
   |n == 2  = ([(Card Club R6),(Card Spade R10)],initialState)
   |n == 3  = ([(Card Club R5),(Card Heart R8),(Card Spade R10)],initialState)
   |n == 4  = ([(Card Club R4),(Card Diamond R8),(Card Heart Jack),
               (Card Spade King)],initialState)
     where initialState = Candidate (initialCandiate n)

-------------------------------------------------------------------------------
-- The initialCandiate function generates all the combinations of 2cards, 
-- 3cards, or 4cards. In order to avoid repetition, we can make each element 
-- in one combination is bigger than the previous element. 
-------------------------------------------------------------------------------
initialCandiate :: Int -> [[Card]]
initialCandiate n 
 | n == 2 = [[i,j]|i <- [Card Club R2 .. Card Spade Ace],
                   j <- [Card Club R2 .. Card Spade Ace], 
                   j > i]
 | n == 3 = [[x,y,z]|x <- [Card Club R2 .. Card Spade Ace],
                     y <- [Card Club R2 .. Card Spade Ace],
                     z <- [Card Club R2 .. Card Spade Ace], 
                     z > y, y > x]
 | n == 4 = [[i,j,x,y]|i <- [Card Club R2 .. Card Spade Ace],
                       j <- [Card Club R2 .. Card Spade Ace],
                       x <- [Card Club R2 .. Card Spade Ace],
                       y <- [Card Club R2 .. Card Spade Ace], 
                       y > x, x > j, j > i]

-------------------------------------------------------------------------------
-- The nextGuess function has two functions(satCandidate,remover) to support it
-- This function will take previous guess and candidate as well as its feedback
-- as input and return a new [Card] and candidate. The new [Card] is the new 
-- combination we use as next guess. It is in the new candidate(nextGamestate)
-- because the new candidate is the set of possible answer we filter out.
-- The idea of this function is every time we guess, we will give a [Card] and
-- candidate as input, then we use every element in candidate as answer to 
-- compare to the given [Card], if the feedback is same as the feedback it
-- compares to actual answer, then we put it in a nextGamestate. The next 
-- conbination of card we choose, is one of the nextGamestate. 
-------------------------------------------------------------------------------
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (preCard,(Candidate candidate)) preFeedback =
  let Candidate nextGamestate 
                = remover (preCard,(Candidate candidate)) preFeedback in 
    ((head nextGamestate),Candidate nextGamestate)

-------------------------------------------------------------------------------
-- Here I use two funtions which seems unnecessary, but it's easy for me to 
-- understand how it works ,when I read this code again after a long time.
-- the satCandidate means satisfying condition to be candidate which is the 
-- feedback with preCard is same as actual answer's feedback with preCard
-------------------------------------------------------------------------------
satCandidate :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> Bool
satCandidate (preCard,(Candidate candidate)) preFeedback 
    |preFeedback == (feedback (head candidate) preCard)  = True 
    |otherwise                                           = False

-------------------------------------------------------------------------------
-- If the satCandidate is true, it is the candidate, then, we add it in a list,
-- else we just ignore it. This function is called by nextGusee to calculate
-- the nextGamestate.
-------------------------------------------------------------------------------      
remover :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> GameState
remover (preCard,(Candidate [])) _ = Candidate []
remover (preCard,(Candidate candidate)) preFeedback 
  |isCandidate  = Candidate ([head candidate] ++ candidateSet)
  |otherwise    = Candidate ([] ++ candidateSet)
   where Candidate candidateSet = 
                    remover (preCard,(Candidate (tail candidate))) preFeedback
         isCandidate = satCandidate (preCard,(Candidate candidate)) preFeedback