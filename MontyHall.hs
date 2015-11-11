module Main where

import           System.Random
import           Data.Set hiding (filter)
import System.Environment
import Control.Monad

type Choice = Int

data Price = Donkey
           | Car
  deriving (Show, Eq)

data Tactic = Stay
            | Change
  deriving (Show, Read, Eq)

data Game = Game { doorCount :: Int, winning :: Int, initial :: Choice }
  deriving Show

allGames :: Int -> [Game]
allGames n = [Game n b i | b <- [1 .. n]
                         , i <- [1 .. n]]

montyChoice :: Game -> [Choice]
montyChoice g = if initial g == winning g
                  then 
                  -- should kick out an arbitrary element, but we kick the last
                  init allBut
                  else allBut
  where
    allBut = [x | x <- [1 .. (doorCount g)]
                , x /= initial g
                , x /= winning g]

choose :: Game -> Choice -> Price
choose g c
  | c == winning g = Car
choose g c = Donkey

finalChoice :: Tactic -> Game -> Choice
finalChoice Stay g = initial g
finalChoice Change g = head . toList $ fromList [1 .. (doorCount g)] `difference`
                                       fromList (montyChoice g) `difference`
                                       singleton (initial g)

playGame :: Tactic -> Game -> Price
playGame t g = choose g c
  where
    c = finalChoice t g

winChance :: Int -> Tactic -> Float
winChance n t = wins / total
  where
    pricesWon = [playGame t g | g <- allGames n]
    wins = fromIntegral $ length (filter (== Car) pricesWon)
    total = fromIntegral $ length pricesWon

randomGame :: Int -> IO Game
randomGame n =
  do
    i <- getStdRandom (randomR (1, n))
    w <- getStdRandom (randomR (1, n))
    return $ Game n w i

playVerboseGame :: Tactic -> Game -> IO Bool
playVerboseGame t g =
  do
    let win = playGame t g == Car
    putStrLn $ "Playing with " ++
               show (doorCount g) ++
               " doors, car behind door: " ++
               show (winning g) ++
               " with tactic: " ++
               show t
    putStrLn $ "Player chooses first: " ++
               show (initial g) ++
               ", then Monty opens doors: " ++
               show (montyChoice g)
    putStrLn $ "Players final choice " ++ show (finalChoice t g) ++ " and " ++ (if win 
                                                                                  then "WINS!"
                                                                                  else "LOOSES.")
    return win

performRandomBatch :: Tactic -> Int -> Int -> IO ()
performRandomBatch t d n = 
      do 
        g <- randomGame d;
        playVerboseGame t g;
        when  (n > 1) $ performRandomBatch t d (n-1)

main :: IO ()
main = 
  do 
    args <- getArgs;
    let count = (read::String -> Int) $ head args
    let doors = ( read::String -> Int) $  args !! 1
    let tactic = (read::String -> Tactic) $ args !! 2
    performRandomBatch tactic doors count

-- Tests
montyChoiceIsAllwaysDonkey :: Bool
montyChoiceIsAllwaysDonkey = and [winning g `notElem` montyChoice g | g <- allGames 10]

montyChoiceIsNeverInitial :: Bool
montyChoiceIsNeverInitial = and [initial g `notElem` montyChoice g | g <- allGames 10]

