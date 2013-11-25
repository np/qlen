import qualified Data.Map as Map

data P = P !String !String

minMax (P x y) (P z t) = P (x `min` z) (y `max` t)

showEntry (x,P y z) = [y ++ " >" ++ x, z ++ " <" ++ x]

main = interact $ unlines . concatMap showEntry
                          . Map.toList
                          . Map.fromListWith minMax
                          . map ((\[x,y] -> (y,P x x)) . words)
                          . lines
