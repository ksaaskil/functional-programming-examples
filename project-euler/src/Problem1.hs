module Problem1 where

resultForBelow :: (Int -> Int)
resultForBelow maxValue =
    let array    = takeWhile (< maxValue) [0 ..]
        filtered = filter (\n -> n `mod` 3 == 0 && n `mod` 5 == 0) array
    in  sum filtered

result :: Int
result = resultForBelow 1000
