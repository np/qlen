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
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Crypto.Hash.SHA256 as H

data Entry time signal mac sensor = Entry { time :: time, signal :: signal, mac :: mac, sensor :: sensor }

unpackS :: T.Text -> S.ByteString
unpackS = S.pack . T.unpack

packS :: S.ByteString -> T.Text
packS = T.pack . S.unpack

anonymizer :: S.ByteString -> T.Text -> T.Text
anonymizer secretKey x = T.intercalate ":" . T.chunksOf 2 . packS . B16.encode . S.take 6 $ H.hash (secretKey <> unpackS x)

-- Input list should be ordered
intervals :: (Ord a, Num a) => a -> [a] -> [(a,a)]
intervals ε [] = []
intervals ε (x0:xs0) = go (x0,x0) xs0
 where
  go !p               []     = [p]
  go !p@(start, stop) (x:xs)
    | stop > x        = error $ "assert failure" -- ++ ": " ++ show stop ++ " > " ++ show x
    | dist x stop < ε = go (start, x) xs
    | otherwise       = p : go (x, x) xs

dist :: Num a => a -> a -> a
dist x y = abs (x - y)

interactT :: (T.Text -> T.Text) -> IO ()
interactT f = T.putStr . f =<< T.getContents

-- TODO use strftime
to_seconds :: T.Text -> Int
to_seconds = (\[h,m,s] -> (h * 60 + m) * 60 + s) . map (read . T.unpack) . T.splitOn ":" . fst . T.breakOn "."

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

extract_distances :: [Int] -> MinMax Int
extract_distances = liftA2 MinMax minimum maximum . map (uncurry dist) . (zip`ap`tail) . sort

extract_distances' :: [Int] -> [MinMax Int]
extract_distances' []  = []
extract_distances' [_] = []
extract_intervals' xs  = [extract_distances xs]

extract_intervals = map (uncurry StartStop) . intervals 60 . sort . Set.toList

showT = T.pack . show

merge_start_stop (StartStop x y) (StartStop z t) = StartStop (x `min` z) (y `max` t)

color_scheme macs x
  | x `elem` macs = "bright"
  | otherwise     = "pale"

show_mac_start_stop macs (mac,StartStop start stop)
  = [[start, ">" <> mac, "/" <> color_scheme macs mac <> "/" <> mac]
    ,[stop,  "<" <> mac]
    ]

show_maccount (t,n) = [t, "=S", showT n]

approx_time_10min = (<> "0:00") . T.init . T.init . fst . T.breakOnEnd ":"
approx_time_1min  = (<> ":00") . T.init . fst . T.breakOnEnd ":"
approx_time_1sec  = id

read_entries = map (\[time,signal,mac,sensor] -> Entry time signal mac sensor) . map T.words . T.lines

interact_with_entries f = interactT $ T.unlines . map T.unwords . f . read_entries

interact_with_entries_json lbl f = interactT $ T.pack . B.unpack . encode . map mac_info_json . f . read_entries
  where mac_info_json (mac, p) = object ["mac" .= mac, lbl .= p]

extract_time_info :: (Eq mac, Hashable mac)
                  => (time -> info)
                  -> (info -> info -> info)
                  -> (info -> [extracted])
                  -> [Entry time signal mac sensor]
                  -> [(mac,extracted)]
extract_time_info ftime fappend fextract
  = concatMap (\(x,y) -> map ((,)x) (fextract y))
  . groupWith fappend
  . map (\e -> (mac e, ftime (time e)))

macinfo_json
  = interact_with_entries_json "info" $
      extract_time_info (Set.singleton . to_seconds) Set.union (extract_distances' . Set.toList)

macinfo
  = interact_with_entries $
                concatMap (show_mac_start_stop [] . second (fmap from_seconds))
              . sortBy (compare `on` start . snd)
              . extract_time_info (Set.singleton . to_seconds) Set.union extract_intervals

minmax =
 do macs <- T.lines <$> T.readFile "macs"
    interact_with_entries $
                concatMap (show_mac_start_stop macs)
              . sortBy (compare `on` start . snd)
              . extract_time_info (\time -> StartStop time time) merge_start_stop pure

{-
count_macs
  = interact_with_entries $
                map show_maccount
              . sortBy (compare `on` fst)
              . groupWith Set.union
              . map (approx_time *** Set.singleton)
-}
count_macs approx_time
  = interact_with_entries $
                map show_maccount
              . sortBy (compare `on` fst)
              . map (second Set.size)
              . groupWith Set.union
              . map (\e -> (approx_time (time e), Set.singleton (mac e)))

xpose
  = interact_with_entries_json "times" $
      extract_time_info Set.singleton Set.union pure

map_entries ftime fmac
  = interact_with_entries $ map (\e -> [ftime (time e), signal e, fmac (mac e), sensor e])

anon = map_entries id . anonymizer

interaction ["macinfo"] = macinfo
interaction ["minmax"]  = minmax
interaction ["sum"]     = count_macs approx_time_1min
interaction ["xpose"]   = xpose
interaction ["anon",key]= anon (S.pack key)
interaction args = error . unlines $ ["Unpexcted arguments: " ++ show args
                                     ,"Usage: qlen [macinfo|minmax|sum|xpose]"]


main :: IO ()
main = interaction =<< getArgs

