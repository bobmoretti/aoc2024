parseFile :: String -> IO [[Int]]
parseFile fname = do
  let parseLine = map read . words
  contents <- readFile fname
  let allLines = lines contents
  return $ map parseLine allLines

diff = zipWith (-) <*> tail

removeNth :: Int -> [a] -> [a]
removeNth n xs =
  let (before, after) = splitAt n xs
   in before ++ drop 1 after

isSafe :: [Int] -> Bool
isSafe report =
  let deltas = diff report
      isMonotonic = all (> 0) deltas || all (< 0) deltas
      absDeltas = map abs deltas
   in isMonotonic && minimum absDeltas >= 1 && maximum absDeltas <= 3

isSafeWithRemoval :: [Int] -> Bool
isSafeWithRemoval report =
  let allModifiedReports report = [removeNth n report | n <- [0 .. length report]]
   in isSafe report || any isSafe (allModifiedReports report)

part1 :: [[Int]] -> Int
part1 reports = sum $ map (fromEnum . isSafe) reports

part2 :: [[Int]] -> Int
part2 reports = sum $ map (fromEnum . isSafeWithRemoval) reports

main :: IO ()
main = do
  pairs <- parseFile "input.txt"
  print $ part1 pairs
  print $ part2 pairs
