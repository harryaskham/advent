module TwentyNineteen.Day16 where

import Data.Char (digitToInt, intToDigit)
import Data.List (scanl')

errorCode :: Int -> Int -> [Integer]
errorCode l 0 = take l $ tail $ cycle [0, 1, 0, -1]
errorCode l n = take l $ tail . cycle $ concat $ replicate (n + 1) <$> [0, 1, 0, -1]

combineCodeWithNumber :: [Integer] -> [Integer] -> Integer
combineCodeWithNumber code number = fromIntegral . digitToInt . last . show $ sum mulPairs
  where
    pairs = zip code number
    mulPairs = ((*) <$> fst <*> snd) <$> pairs

runPhase :: [Integer] -> [Integer]
runPhase number = take (length number) $ (combineCodeWithNumber <$> fst <*> snd) <$> codeNumbers
  where
    codes = errorCode (length number) <$> [0 ..]
    codeNumbers = zip codes (repeat number)

runPhases :: Int -> [Integer] -> [Integer]
runPhases 0 number = number
runPhases n number = runPhases (n -1) (runPhase number)

-- Okay, need to be smarter here.
-- We need to drop 5970807 digits off the 100-phased number
-- But each digit depends on all others at each phase
-- But, these digits run in a cycle, even though the error code doesn't
-- We can find the error number for the interesting digits easily.
-- So to create digit 5970808 we need errorCode 5970807 over the entire 10000-replication number
-- We can do this once, but how to do this over the next phase?
--
-- Recognise that replication == cancelling out.
-- Any -1 in the same spot as a +1 cancels out and 0 doesn't contribute
-- But +1 must be in the same alignment as the -1, can;t just cancel 1 and -1
--
-- The last digit always remains the same!
-- The last half could be computed incrementally
-- But that's still on a length of 6,500,000 ... which isn't that bad
-- We are looking for 5970807, so well into the second half
-- And the second half never depend on the first half.
-- So we can work backwards from the last digit until we reach our target digit, compute it, and that's a phase
-- Run again 100 times and read out the first 8

-- Build the new number simply by starting with the last and adding the previous.
runEfficientPhase :: [Integer] -> [Integer]
runEfficientPhase = tail . scanl' sumD 0

sumD :: Integer -> Integer -> Integer
sumD a b = fromIntegral . digitToInt . last $ show (a + b)

runEfficientPhases :: Int -> [Integer] -> [Integer]
runEfficientPhases 0 number = number
runEfficientPhases n number = runEfficientPhases (n -1) (runEfficientPhase number)

offset :: [Integer] -> Int
offset number = read $ intToDigit . fromInteger <$> take 7 number

-- Get from a list in a cyclic fashion
modGet :: Int -> [Integer] -> Integer
modGet n xs = xs !! (n `mod` length xs)

-- Get the last N thigns "efficiently"
-- Better if we use a Vector or Array.
efficientLastN :: Int -> [Integer] -> [Integer]
efficientLastN n xs = modGet <$> [length xs - n .. length xs - 1] <*> pure xs

part12 :: IO ()
part12 = do
  ls <- lines <$> readFile "input/2019/16.txt"
  let number = fromIntegral . digitToInt <$> head ls
      target = 5970807
      bigNumber = concat $ replicate 10000 number
      lastNumber = drop target bigNumber -- efficientLastN (length bigNumber - target) bigNumber
  print $ take 8 $ runPhases 100 number
  print $ take 8 . reverse $ runEfficientPhases 100 (reverse lastNumber)
