module TwentyTwenty.Day16 where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf, sort, transpose)
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Text.ParserCombinators.Parsec
  ( GenParser,
    char,
    digit,
    eof,
    many,
    many1,
    noneOf,
    parse,
    sepBy,
    string,
  )

inputPath :: String
inputPath = "input/2020/16.txt"

type Range = (Int, Int)

data Constraint = Constraint String [Range] deriving (Show, Eq, Ord)

type Ticket = [Int]

parseInput :: GenParser Char st ([Constraint], Ticket, [Ticket])
parseInput = do
  constraints <- many1 constraint
  eol
  string "your ticket:"
  eol
  yourTicket <- ticket
  eol
  string "nearby tickets:"
  eol
  nearbyTickets <- many ticket
  eof
  return (constraints, yourTicket, nearbyTickets)
  where
    eol = char '\n'
    constraint = do
      name <- many1 (noneOf ":\n")
      string ": "
      ranges <- range `sepBy` string " or "
      eol
      return $ Constraint name ranges
    range = do
      lower <- many1 digit
      char '-'
      higher <- many1 digit
      return (read lower, read higher)
    ticket = do
      tickets <- many digit `sepBy` char ','
      eol
      return $ read <$> tickets

readInput :: IO ([Constraint], Ticket, [Ticket])
readInput = do
  Right i <- parse parseInput "[input]" <$> readFile inputPath
  return i

meetsConstraint :: Constraint -> Int -> Bool
meetsConstraint (Constraint _ ranges) val =
  or [val >= low && val <= high | (low, high) <- ranges]

meetsAnyConstraints :: [Constraint] -> Int -> Bool
meetsAnyConstraints cs val = or $ meetsConstraint <$> cs <*> pure val

invalidValues :: [Constraint] -> Ticket -> [Int]
invalidValues cs = filter (not . meetsAnyConstraints cs)

part1 :: IO Int
part1 = do
  (constraints, _, tickets) <- readInput
  return . sum . concat $ invalidValues constraints <$> tickets

ticketValid :: [Constraint] -> Ticket -> Bool
ticketValid cs = null . invalidValues cs

findAssignment ::
  IORef (S.Set (S.Set Constraint)) ->
  S.Set Constraint ->
  [[Int]] ->
  [Constraint] ->
  IO (Maybe [Constraint])
findAssignment _ _ [] constraints = return (Just (reverse constraints))
findAssignment deadEnds' constraintsLeft (fvs : fvss) constraints = do
  deadEnds <- readIORef deadEnds'
  if constraintsLeft `S.member` deadEnds || null possibleConstraints
    then fail
    else do
      results <- traverse doNextIter possibleConstraints
      case catMaybes results of
        [] -> fail
        (x : _) -> return $ Just x
  where
    fail = do
      modifyIORef' deadEnds' (S.insert constraintsLeft)
      return Nothing
    possibleConstraints =
      filter
        (\c -> all (meetsConstraint c) fvs)
        (S.toList constraintsLeft)
    doNextIter c =
      findAssignment deadEnds' (S.delete c constraintsLeft) fvss (c : constraints)

part2 :: IO Int
part2 = do
  (constraints, ticket, tickets) <- readInput
  let validTickets = ticket : filter (ticketValid constraints) tickets
      fieldValues = sort <$> transpose validTickets
  deadEnds <- newIORef S.empty
  Just fieldAssignments <-
    findAssignment deadEnds (S.fromList constraints) fieldValues []
  let isDeparture (Constraint name _, _) = "departure" `isPrefixOf` name
  return $ product (snd <$> filter isDeparture (zip fieldAssignments ticket))
