module Adlude
  ( module X,
    Seq(..),
    treverse
  )
where

import Control.Monad.Memo as X hiding (isNothing)
import Relude as X hiding (optional, many, (<|>))
import Helper.Bits as X
import Helper.Collection as X
import Helper.Grid as X hiding (find)
import Helper.Coord as X
import Helper.TH as X
import Helper.Tracers as X
import Helper.Util as X hiding (count)
import Text.ParserCombinators.Parsec as X hiding (State)
import Data.Sequence (Seq(..))
import Data.Distributive as X

treverse :: (Traversable t, Monad f) => (t a -> b) -> t (f a) -> f b
treverse f = fmap f . sequence xs
