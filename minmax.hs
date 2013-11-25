{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Data.Functor
import Data.Monoid
import Data.Function
import Data.List
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

data P = P {minP , maxP :: !T.Text}

minMax (P x y) (P z t) = P (x `min` z) (y `max` t)

colorSchm knownMacs x
  | x `elem` knownMacs = "bright"
  | otherwise          = "pale"

showEntry knownMacs (mac,P minP maxP)
  = [minP <> " >" <> mac <> " /" <> colorSchm knownMacs mac <> "/" <> mac
    ,maxP <> " <" <> mac
    ]

interactT f = T.putStr . f =<< T.getContents

main = do
       knownMacs <- T.lines <$> T.readFile "macs"
       interactT $ T.unlines
                 . concatMap (showEntry knownMacs)
                 . sortBy (compare `on` minP . snd)
                 . Map.toList
                 . Map.fromListWith minMax
                 . map (\[time,mac] -> (mac,P time time))
                 . map T.words
                 . T.lines
