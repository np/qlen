{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Arrow
import Data.Functor
import Data.Monoid
import Data.Function
import Data.List
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set -- .Strict as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T

showEntry (t,n) = t <> " =S " <> T.pack (show n)

--cutSeconds = (<> "0:00") . T.init . T.init . fst . T.breakOnEnd ":"
cutSeconds = (<> ":00") . T.init . fst . T.breakOnEnd ":"
--cutSeconds = id

interactT f = T.putStr . f =<< T.getContents

main = interactT $ T.unlines
                 . map showEntry
                 . sortBy (compare `on` fst)
                 . map (second Set.size)
                 . Map.toList
                 . Map.fromListWith Set.union
                 . map (\[x,y] -> (cutSeconds x,Set.singleton y))
                 . map T.words
                 . T.lines
