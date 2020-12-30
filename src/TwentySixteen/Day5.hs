module TwentySixteen.Day5 where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import Data.Char (digitToInt)
import qualified Data.Map.Strict as M
import Data.Tuple.Extra (first)
import Util (toTuple2)

input :: String
input = "ugkcyxxp"

hashes :: [String]
hashes =
  BC.unpack
    . B16.encode
    . MD5.hash
    . BC.pack
    . (input ++)
    . show
    <$> [0 ..]

part1 :: String
part1 = take 8 [hash !! 5 | hash <- hashes, take 5 hash == "00000"]

part2 :: String
part2 =
  let m =
        M.fromListWith
          (flip const)
          [ first digitToInt . toTuple2 . take 2 . drop 5 $ hash
            | hash <- take 50000000 hashes,
              take 5 hash == "00000",
              digitToInt (hash !! 5) < 8
          ]
   in (m M.!) <$> [0 .. 7]
