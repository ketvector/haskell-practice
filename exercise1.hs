myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength(xs)

myMeanHelper :: [Int] -> (Float,Int)
myMeanHelper [] = (0.0, 0)
myMeanHelper (x:xs) = let y = myMeanHelper xs 
                          mean = fst y
                          l = snd y
                          lf = fromIntegral l
                          xf = fromIntegral x
                      in ((xf + mean * lf)/(lf+1) , l+1)
myMean :: [Int] -> Float                    
myMean l = fst (myMeanHelper l)

myPallindromeCreate :: [a] -> [a]
myPallindromeCreate [] = []
myPallindromeCreate (x:xs) = [x] ++ myPallindromeCreate xs ++ [x]

myReverse [] = []
myReverse (x:xs) = reverse(xs) ++ [x]

isSame :: (Eq x) => [x] -> [x] -> Bool
isSame [] [] = True
isSame [] _ = False
isSame _ [] = False
isSame (x:xs) (y:ys) = (x == y) && isSame xs ys

isPallindrome :: (Eq a) => [a] -> Bool
isPallindrome [] = True
isPallindrome l = isSame l (myReverse l)
 

 