module Day7 (part1, part2) where

import Data.List (delete, drop, filter, foldl', minimum, reverse)
import Helper.TH (input)
import Helper.Util (eol, number, parseWith)
import Relude.Unsafe qualified as U
import Text.ParserCombinators.Parsec (Parser, eof, many1, noneOf, string, try, (<|>))
import Prelude hiding ((<|>))

data Cmd = Cd String | Ls [Node]

data Node = Dir String [Node] | File String Integer deriving (Eq)

parser :: Parser [Cmd]
parser = many1 (try cd <|> try ls) <* eof
  where
    cd = Cd <$> (string "$ cd " *> many1 (noneOf "\n") <* eol)
    ls = do
      string "$ ls" >> eol
      nodes <- many1 (node <* eol)
      return $ Ls nodes
    node = dir <|> file
    dir = Dir <$> (string "dir " *> many1 (noneOf "\n")) <*> pure []
    file = do
      size <- number
      string " "
      name <- many1 (noneOf "\n")
      return $ File name size

insertNode :: [String] -> Node -> Node -> Node
insertNode [] node (Dir name nodes) = Dir name (node : nodes)
insertNode [] _ f@(File _ _) = f
insertNode (p : path) node (Dir name nodes) =
  Dir name (insertNode path node parent : delete parent nodes)
  where
    nodeName (Dir s _) = s
    nodeName (File s _) = s
    parent = U.head $ filter ((== p) . nodeName) nodes

mkFs :: [Cmd] -> Node
mkFs cmds = go ["/"] (Dir "/" []) (drop 1 cmds)
  where
    go _ tree [] = tree
    go path tree ((Cd "..") : cmds) = go (drop 1 path) tree cmds
    go path tree ((Cd s) : cmds) = go (s : path) tree cmds
    go path tree ((Ls nodes) : cmds) =
      go path (foldl' (flip $ insertNode (drop 1 $ reverse path)) tree nodes) cmds

nodeSize :: Node -> Integer
nodeSize (Dir _ nodes) = sum $ nodeSize <$> nodes
nodeSize (File _ size) = size

dirSizes :: Node -> [Integer]
dirSizes = catMaybes . go
  where
    go d@(Dir _ nodes) = Just (nodeSize d) : (go =<< nodes)
    go (File _ _) = pure Nothing

part1 :: Integer
part1 =
  $(input 7)
    & parseWith parser
    & mkFs
    & dirSizes
    & filter (<= 100000)
    & sum

part2 :: Integer
part2 =
  let fs = $(input 7) & parseWith parser & mkFs
      toFree = (nodeSize fs - 40000000)
   in dirSizes fs & filter (>= toFree) & minimum
