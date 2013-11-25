import qualified Data.Map as Map

minMax (x,y) (z,t) = (x `min` z, y `max` t)

main = interact $ unlines . concatMap (\(x,(y,z)) -> [y ++ " >" ++ x, z ++ " <" ++ x])
                          . Map.toList
                          . Map.fromListWith minMax
                          . map ((\[x,y] -> (y,(x,x))) . words)
                          . lines
