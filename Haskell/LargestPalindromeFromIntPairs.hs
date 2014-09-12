

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x
  | mod len 2 == 0 && take (div len 2) x == reverse (drop (div len 2) x) = True
  | otherwise                                                            = False
  where len = length x

largestPalidromeOfSize :: Int -> Int
largestPalidromeOfSize n = foldr max 0 . map (uncurry (*)) . filter (isPalindrome . show . uncurry (*)) $ [ (x,y) | x <- [s,s-1..t], y <- [s,s-1..t] ]
  where s = (10 ^ n) - 1
        t = s `quot` 2
        
