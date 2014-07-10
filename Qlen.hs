{-# LANGUAGE BangPatterns, OverloadedStrings, GeneralizedNewtypeDeriving #-}
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Aeson
import Data.Function
--import Data.Functor
import Data.String
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

type Time = T.Text
newtype Mac = Mac { unMac :: T.Text }
  deriving (Eq, Hashable)
type Key = S.ByteString
data Entry time signal mac sensor = Entry { time :: time, signal :: signal, mac :: mac, sensor :: sensor }

unpackS :: T.Text -> S.ByteString
unpackS = S.pack . T.unpack

packS :: S.ByteString -> T.Text
packS = T.pack . S.unpack

-- Take the 6 first bytes and present them as a MAC address (00:01:02:03:04:05)
takeMAC :: S.ByteString -> Mac
takeMAC = Mac . T.intercalate ":" . T.chunksOf 2 . packS . B16.encode . S.take 6

anonymizer :: Key -> Mac -> Mac
anonymizer secretKey = takeMAC . H.hash . (secretKey <>) . unpackS . unMac

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
to_seconds :: Time -> Int
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
extract_distances' xs  = [extract_distances xs]

extract_intervals :: Set.HashSet Int -> [StartStop Int]
extract_intervals = map (uncurry StartStop) . intervals 60 . sort . Set.toList

showT :: Int -> T.Text
showT = T.pack . show

merge_start_stop :: Ord a => StartStop a -> StartStop a -> StartStop a
merge_start_stop (StartStop x y) (StartStop z t) = StartStop (x `min` z) (y `max` t)

color_scheme :: (Eq a, IsString b) => [a] -> a -> b
color_scheme macs x
  | x `elem` macs = "bright"
  | otherwise     = "pale"

show_mac_start_stop :: [Mac] -> (Mac, StartStop Time) -> [[T.Text]]
show_mac_start_stop macs (mac,StartStop start stop)
  = [[start, ">" <> unMac mac, "/" <> color_scheme macs mac <> "/" <> unMac mac]
    ,[stop,  "<" <> unMac mac]
    ]

show_maccount :: (T.Text, Int) -> [T.Text]
show_maccount (t,n) = [t, "=S", showT n]

approx_time_10min :: Time -> Time
approx_time_1min  :: Time -> Time
approx_time_1sec  :: Time -> Time
approx_time_10min = (<> "0:00") . T.init . T.init . fst . T.breakOnEnd ":"
approx_time_1min  = (<> "00") . fst . T.breakOnEnd ":"
approx_time_1sec  = id

read_entries :: T.Text -> [Entry T.Text T.Text Mac T.Text]
read_entries = map (\[time,signal,mac,sensor] -> Entry time signal (Mac mac) sensor) . map T.words . T.lines

interact_with_entries :: ([Entry T.Text T.Text Mac T.Text] -> [[T.Text]]) -> IO ()
interact_with_entries f = interactT $ T.unlines . map T.unwords . f . read_entries

interact_with_entries_json :: (ToJSON a) => T.Text -> ([Entry T.Text T.Text Mac T.Text] -> [(Mac, a)]) -> IO ()
interact_with_entries_json lbl f = interactT $ T.pack . B.unpack . encode . map mac_info_json . f . read_entries
  where mac_info_json (Mac mac, p) = object ["mac" .= mac, lbl .= p]

extract_time_info :: (Eq mac, Hashable mac)
                  => (time -> info)
                  -> (info -> info -> info)
                  -> (info -> [extracted])
                  -> [Entry time signal mac sensor]
                  -> [(mac,extracted)]
extract_time_info ftime fappend fextract
  = distr
  . groupWith fappend
  . map (\e -> (mac e, ftime (time e)))
  where distr inp = [ (m,e) | (m,i) <- inp, e <- fextract i ]

macinfo_json :: IO ()
macinfo_json
  = interact_with_entries_json "info" $
      extract_time_info (Set.singleton . to_seconds) Set.union (extract_distances' . Set.toList)

macinfo :: IO ()
macinfo
  = interact_with_entries $
                concatMap (show_mac_start_stop [] . second (fmap from_seconds))
              . sortBy (compare `on` start . snd)
              . extract_time_info (Set.singleton . to_seconds) Set.union extract_intervals

minmax :: IO ()
minmax =
 do macs <- map Mac . T.lines <$> T.readFile "macs"
    interact_with_entries $
                concatMap (show_mac_start_stop macs)
              . sortBy (compare `on` start . snd)
              . extract_time_info (\time -> StartStop time time) merge_start_stop pure

count_macs :: (T.Text -> T.Text) -> IO ()
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

xpose :: IO ()
xpose
  = interact_with_entries_json "times" $
      extract_time_info Set.singleton Set.union pure

map_entries :: (Time -> Time) -> (Mac -> Mac) -> IO ()
map_entries ftime fmac
  = interact_with_entries $ map (\e -> [ftime (time e), signal e, unMac (fmac (mac e)), sensor e])

anon :: Key -> IO ()
anon = map_entries id . anonymizer

interaction :: [String] -> IO ()
interaction ["macinfo"] = macinfo
interaction ["macinfo-json"] = macinfo_json
interaction ["minmax"]  = minmax
interaction ["sum"]     = count_macs approx_time_1min
interaction ["xpose"]   = xpose
interaction ["anon",key]= anon (S.pack key)
interaction args = error . unlines $ ["Unpexcted arguments: " ++ show args
                                     ,"Usage: qlen [macinfo|macinfo-json|minmax|sum|xpose]"]

main :: IO ()
main = interaction =<< getArgs

