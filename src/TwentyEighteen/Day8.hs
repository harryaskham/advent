module TwentyEighteen.Day8 where

-- A tree where each node has multiple children and some integer metadata
type Metadata = Int

data Tree = Tree [Tree] [Metadata] deriving (Show)

-- Parse a single tree and return the remainder
parseTree :: [Int] -> (Tree, [Int])
parseTree (0 : numMeta : xs) = (Tree [] (take numMeta xs), drop numMeta xs)
parseTree (numChild : numMeta : xs) = (Tree (reverse children) (take numMeta rem), drop numMeta rem)
  where
    nTrees 0 acc rem = (acc, rem)
    nTrees numChild acc xs = nTrees (numChild - 1) (tree : acc) rem
      where
        (tree, rem) = parseTree xs
    (children, rem) = nTrees numChild [] xs

sumMeta :: Tree -> Int
sumMeta (Tree [] meta) = sum meta
sumMeta (Tree children meta) = sum meta + sum (sumMeta <$> children)

metaValue :: Tree -> Int
metaValue (Tree [] meta) = sum meta
metaValue (Tree children meta) = sum (metaValue <$> [children !! (m - 1) | m <- meta, m > 0, m <= length children])

part12 :: IO ()
part12 = do
  input <- fmap read . words . head . lines <$> readFile "input/2018/8.txt"
  let tree = fst . parseTree $ input
   in do
        print $ sumMeta tree
        print $ metaValue tree
