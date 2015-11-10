module Main where

import           Data.List
import           System.Random

data Price = Donkey
           | Car
  deriving (Show, Eq, Ord)

data Tactic = Stay
            | Change
  deriving (Show, Eq)

boards = nub $ permutations [Donkey, Donkey, Car]

data Choice = First
            | Second
            | Third
  deriving (Show, Eq, Enum)

data Game = Game { board :: [Price], initial :: Choice }
  deriving Show

allGames :: [Game]
allGames = [Game b i | b <- boards
                     , i <- [First, Second, Third]]

montyChoice :: Game -> Choice
montyChoice game =
  case initial game of
    First ->
      case b !! 1 of
        Donkey -> Second
        Car    -> Third
    Second ->
      case head b of
        Donkey -> First
        Car    -> Third
    Third ->
      case head b of
        Donkey -> First
        Car    -> Second
  where
    b = board game

choose :: Game -> Choice -> Price
choose g c =
  case c of
    First  -> head b
    Second -> b !! 1
    Third  -> b !! 2
  where
    b = board g

montyChoiceIsAllwaysDonkey :: Bool
montyChoiceIsAllwaysDonkey = and [choose g (montyChoice g) == Donkey | g <- allGames]

montyChoiceIsNeverInitial :: Bool
montyChoiceIsNeverInitial = and
                              [montyChoice g /= initial g | g <- allGames ]

finalChoice :: Tactic -> Game -> Choice
finalChoice Stay g  = initial g
finalChoice Change g  =
  case initial g of
    First ->
      case monty of
        Second -> Third
        Third  -> Second
    Second ->
      case monty of
        First -> Third
        Third -> First
    Third ->
      case monty of
        First  -> Second
        Second -> First
    where monty = montyChoice g
          

playGame::Tactic -> Game -> Price
playGame t g = choose g c 
  where
    c = finalChoice t g 


winChance :: Tactic -> Float
winChance t = wins/total
  where 
    allgames = allGamePrices t
    wins = fromIntegral $ length  (filter (==Car) allgames)
    total = fromIntegral $ length allgames
    allGamePrices t = [  playGame t g| g <- allGames ]

main :: IO ()
main = undefined
