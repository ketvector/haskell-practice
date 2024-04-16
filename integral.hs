import Text.Printf (printf)

-- My solution to https://www.hackerrank.com/challenges/area-under-curves-and-volume-of-revolving-a-curv/problem

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
f :: [Int] -> [Int] -> Double -> Double
f a b x = if null a
            then 0 
            else let c = head a
                     cs = tail a
                     p = head b
                     ps = tail b
                     cd = fromIntegral c
                     pd = fromIntegral p
                 in  (cd * ( x ** pd)) + (f cs ps x)

area :: Double -> Double -> Double
area v s = v * s

volume :: Double -> Double -> Double
volume v s = let r = v 
                 h = s
             in pi * r * r * h

solveh :: Double -> Double -> [Int] -> [Int] -> Double -> (Double,Double)
solveh c r a b s  = if c >= r
                    then (0.0 , 0.0) 
                    else let v = f a b c
                             rest = solveh (c + s) r a b s
                             resta = fst rest
                             restv = snd rest
                         in ((area v s) + (resta), (volume v s) + (restv))

solveh2 l r a b = if l < r then solveh l r a b 0.001 else solveh r l a b 0.001

solve l r a b = let ld = fromIntegral l
                    rd = fromIntegral r
                    s = solveh2 ld rd a b 
                in [fst s, snd s]

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines