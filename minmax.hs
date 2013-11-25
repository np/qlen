{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

data P = P !T.Text !T.Text

minMax (P x y) (P z t) = P (x `min` z) (y `max` t)

showEntry (x,P y z) = [y <> " >" <> x, z <> " <" <> x]

interactT f = T.putStr . f =<< T.getContents

main = interactT $ T.unlines
                 . concatMap showEntry
                 . Map.toList
                 . Map.fromListWith minMax
                 . map ((\[x,y] -> (y,P x x)) . T.words)
                 . T.lines
