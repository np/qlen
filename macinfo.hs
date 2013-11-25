{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Arrow
import Control.Applicative
import Data.Functor
import Data.Monoid
import Data.Function
import Data.List
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson
import Data.Hashable
import qualified Data.ByteString.Lazy.Char8 as B

data P = P {minP , maxP :: !Int}

groupWith :: (Eq a, Hashable a) => (b -> b -> b) -> [(a,b)] -> [(a,b)]
groupWith op = Map.toList . Map.fromListWith op

instance ToJSON P where
   toJSON (P mi ma) = object ["min" .= mi, "max" .= ma]

showEntry (mac, v) = object ["mac" .= mac, "info" .= v]

dist x y = abs (x - y)

extract = liftA2 P minimum maximum . map (uncurry dist) . (zip`ap`tail) . sort . Set.toList

interactT f = T.putStr . f =<< T.getContents

toTime :: T.Text -> Int
toTime = (\[h,m,s] -> (h * 60 + m) * 60 + s) . map (read . T.unpack) . T.splitOn ":"

main = interactT $ T.pack
                 . B.unpack
                 . encode
                 . toJSON
                 . map showEntry
                 . map (second extract)
                 . filter ((> 1) . Set.size . snd)
                 . groupWith Set.union
                 . map (\[time,mac] -> (mac,Set.singleton (toTime time)))
                 . map T.words
                 . T.lines
