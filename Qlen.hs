{-# LANGUAGE BangPatterns, OverloadedStrings #-}
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Aeson
import Data.Function
import Data.Functor
import Data.Hashable
import Data.List
import Data.Monoid
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Text as T
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.IO as T

ε = 60

class CloseTo a where
  closeTo :: a -> a -> Bool
  -- x == y ==> x `closeTo` y

instance CloseTo Int where
  x `closeTo` y = dist x y < ε

-- Input list should be ordered
intervals :: (CloseTo a, Ord a) => [a] -> [(a,a)]
intervals [] = []
intervals (x0:xs0) = go (x0,x0) xs0
 where
  go !p               []     = [p]
  go !p@(start, stop) (x:xs)
    | stop > x         = error $ "assert failure" -- ++ ": " ++ show stop ++ " > " ++ show x
    | x `closeTo` stop = go (start, x) xs
    | otherwise        = p : go (x, x) xs

dist :: Num a => a -> a -> a
dist x y = abs (x - y)

interactT :: (T.Text -> T.Text) -> IO ()
interactT f = T.putStr . f =<< T.getContents

-- TODO use strftime
to_seconds :: T.Text -> Int
to_seconds = (\[h,m,s] -> (h * 60 + m) * 60 + s) . map (read . T.unpack) . T.splitOn ":"

from_seconds :: Int -> T.Text
from_seconds x = mconcat [showN h, ":", showN m, ":", showN s]
  where
    (y,s) = divMod x 60
    (h,m) = divMod y 60
    showN = leading0 . showT
    leading0 x | T.length x < 2 = "0" <> x
               | otherwise      = x

groupWith :: (Eq a, Hashable a) => (b -> b -> b) -> [(a,b)] -> [(a,b)]
groupWith op = Map.toList . Map.fromListWith op

data MinMax a = MinMax {minP , maxP :: !a}

instance ToJSON a => ToJSON (MinMax a) where
   toJSON (MinMax mi ma) = object ["min" .= mi, "max" .= ma]

data StartStop a = StartStop {start , stop :: !a}

instance Functor StartStop where fmap f (StartStop x y) = StartStop (f x) (f y)

instance ToJSON a => ToJSON (StartStop a) where
   toJSON (StartStop start stop) = object ["start" .= start, "stop" .= stop]

--show_mac_info (mac, v) = object ["mac" .= mac, "info" .= v]

--extract = liftA2 MinMax minimum maximum . map (uncurry dist) . (zip`ap`tail) . sort . Set.toList
extract = map (uncurry StartStop) . intervals . sort . Set.toList

encodeT = T.pack . B.unpack . encode

showT = T.pack . show

macinfo = interactT $ -- encodeT
                 T.unlines
                 . concatMap (show_mac_start_stop [] . second (fmap from_seconds))
                 . sortBy (compare `on` start . snd)
                 . (\xys -> [(x,y) | (x,ys) <- xys, y <- ys])
                 . map (second extract)
               --  . filter ((> 1) . Set.size . snd)
                 . groupWith Set.union
                 . map (\[time,mac] -> (mac,Set.singleton (to_seconds time)))
                 . map T.words
                 . T.lines


merge_start_stop (StartStop x y) (StartStop z t) = StartStop (x `min` z) (y `max` t)

colorSchm macs x
  | x `elem` macs = "bright"
  | otherwise     = "pale"

show_mac_start_stop macs (mac,StartStop start stop)
  = [start <> " >" <> mac <> " /" <> colorSchm macs mac <> "/" <> mac
    ,stop  <> " <" <> mac
    ]

minmax = do
       macs <- T.lines <$> T.readFile "macs"
       interactT $ T.unlines
                 . concatMap (show_mac_start_stop macs)
                 . sortBy (compare `on` start . snd)
                 . groupWith merge_start_stop
                 . map (\[time,mac] -> (mac,StartStop time time))
                 . map T.words
                 . T.lines

show_maccount (t,n) = t <> " =S " <> showT n

--approx_time = (<> "0:00") . T.init . T.init . fst . T.breakOnEnd ":"
approx_time = (<> ":00") . T.init . fst . T.breakOnEnd ":"
--approx_time = id

count_macs = interactT $ T.unlines
                 . map show_maccount
                 . sortBy (compare `on` fst)
                 . map (second Set.size)
                 . groupWith Set.union
                 . map (\[time,mac] -> (approx_time time,Set.singleton mac))
                 . map T.words
                 . T.lines

show_mac_times (mac, p) = object ["mac" .= mac, "times" .= p]

xpose = interactT $ encodeT
                 . map show_mac_times
                 . groupWith Set.union
                 . map (\[time,mac] -> (mac,Set.singleton time))
                 . map T.words
                 . T.lines

interaction ["macinfo"] = macinfo
interaction ["minmax"]  = minmax
interaction ["sum"]     = count_macs
interaction ["xpose"]   = xpose
interaction args = error . unlines $ ["Unpexcted arguments: " ++ show args
                                     ,"Usage: qlen [macinfo|minmax|sum|xpose]"]


main :: IO ()
main = interaction =<< getArgs

