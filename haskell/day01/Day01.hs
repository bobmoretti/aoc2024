import Data.List (sort, transpose)

parseFile :: String -> IO [[Int]]
parseFile fname = do
  let parseLine = map read . words
  contents <- readFile fname
  let allLines = lines contents
  return $ map parseLine allLines

part1 :: [[Int]] -> Int
part1 pairs =
  let lists = transpose pairs
      list0 = sort (lists !! 0)
      list1 = sort (lists !! 1)
   in sum $ map abs $ zipWith (-) list0 list1

part2 :: [[Int]] -> Int
part2 pairs =
  let lists = transpose pairs
      list0 = lists !! 0
      list1 = lists !! 1
      countOccurrences n = length . filter (n ==)
   in sum [n * countOccurrences n list1 | n <- list0]

main :: IO ()
main = do
  pairs <- parseFile "input.txt"
  print $ part1 pairs
  print $ part2 pairs
