
rleHelper :: [(Char, Char)] -> Int -> String -> String
rleHelper [] _ outputString = outputString
rleHelper ((a, b) : ys) count outputString
  | a == b = rleHelper ys (count + 1) outputString
  | otherwise = rleHelper ys 1 updatedOutputString
    where updatedOutputString = outputString ++ [a] ++ (show count)

rle :: String -> String
rle x = rleHelper y 1 ""
  where
    x' = x ++ "$"
    y = zip x' $ tail x'

main = do
  let x = "aaaabbdceeedcca"

  print $ rle x