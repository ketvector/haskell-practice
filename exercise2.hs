{-# LANGUAGE InstanceSigs #-}
import Text.Printf
import Data.Maybe
import Data.List
import Debug.Trace

debug = flip trace

data Point = Point {
                x :: Int,
                y :: Int
            } deriving (Show,Eq)

instance Ord Point where
    compare :: Point -> Point -> Ordering
    compare (Point x1 y1) (Point x2 y2) = if x1 == x2 then compare y1 y2 else compare x1 x2
             
                        
data Line = Line {
                a :: Point,
                b :: Point
            } deriving (Show,Eq,Ord)
                     

mySolve :: [Point] -> Double
solve :: [(Int, Int)] -> Double

makeOrderedLine p1 p2 = if p1 <= p2 then Line p1 p2 else Line p2 p1 
      
makeLineHelp e l = foldr (\x a -> (makeOrderedLine e x):a) [] l

makeLines points = case points of 
                    [] -> []
                    (p:ps) -> (makeLineHelp p ps) ++ (makeLines ps)
                    
getY (Point _ y) = y
getX (Point x _) = x
isHorizontal (Line a b) = (getY a == getY b)
isVertical (Line a b) = (getX a == getX b)
getParity d = if d == 0.0 
                then 0 
              else if d > 0.0
                then 1
              else (-1)

doubles (Line (Point x1 y1) (Point x2 y2)) = (fromIntegral x1, fromIntegral y1, fromIntegral x2, fromIntegral y2)

throughOrigin (Line (Point x1 y1) (Point x2 y2)) = (x1*y2 == y1*x2)   
            
lineEquation (Line a b) = if not (throughOrigin (Line a b))
                            then let (x1,y1,x2,y2) = doubles (Line a b)
                                 in ( (y1 - y2) / ((x1*y2) - (x2*y1)), (x1 - x2) / ((y1*x2) - (y2*x1)), (1.0))
                          else if isHorizontal (Line a b)
                            then (1.0, 0.0, 0.0)
                          else if isVertical (Line a b)
                            then (0.0, 1.0, 0.0)
                          else
                            let (x1,y1,x2,y2) = doubles (Line a b)
                            in  (1.0, -1.0 * (y1/x1), 0.0)  

evaluate (p,q,r) (Point x y) =  let xd = fromIntegral x
                                    yd = fromIntegral y
                                    v = (p*xd) + (q*yd) + r
                                in v -- `debug` show (p,q,r) `debug` show (x,y)
                                          
findParity (Line a b) (Point x y) = let eq = lineEquation (Line a b)
                                    in getParity (evaluate eq (Point x y))

reducer line point (z,p,n,c) = case findParity line point of
                                0 -> (z+1,p,n,c+1) -- `debug` show line `debug` show point `debug` show "zero"
                                1 -> (z,p+1,n,c+1) -- `debug` show line `debug` show point `debug` show "positive"
                                (-1) -> (z,p,n+1,c+1) -- `debug` show line `debug` show point `debug` show "minus"

 
                                
                                    
isGoodLineHelper points line =  foldr (reducer line) (0,0,0,0) points

isGoodResult (z,p,n,c) = if (z + p == c) || (z + n == c) then True else False

isGoodLine points line = let inter = isGoodLineHelper points line
                         in isGoodResult inter -- `debug` show line `debug` show inter

getLineLength (Line a b) = let x1 = fromIntegral (getX a)
                               x2 = fromIntegral (getX b)
                               y1 = fromIntegral (getY a)
                               y2 = fromIntegral (getY b)
                            in ((x1-x2)**2 + (y1-y2)**2)**(0.5)
                            
sumLineLengths lines = foldr (\ line sum -> sum + (getLineLength line)) 0.0 lines
                                               
solve points = mySolve (map (\p -> Point (fst p) (snd p)) points)


findSlope :: Line -> Maybe Double
findSlope (Line a b) = if (getX a) == (getX b)
                        then Nothing
                        else let x1 = fromIntegral (getX a)
                                 x2 = fromIntegral (getX b)
                                 y1 = fromIntegral (getY a)
                                 y2 = fromIntegral (getY b)
                             in Just ((y2 - y1) / (x1 - x2))
                        
hasSameSlope :: Line -> Line -> Bool
hasSameSlope l1 l2 =  let s1 = findSlope l1
                          s2 = findSlope l2
                      in case s1 of 
                        Nothing -> case s2 of
                                    Nothing -> True
                                    Just x -> False
                        Just y -> case s2 of
                                    Nothing -> False
                                    Just x -> (x == y)

isDependent (Line a b) (Line c d) = if (a == c) || (a == d) || ( b == c) || (b == d)
                                        then hasSameSlope (Line a b) (Line c d)
                                        else False 
isInGroup lines line = if null lines 
                        then False
                        else let matches = filter (isDependent line) lines
                             in not (null matches)
insertInRightGroupHelper alines line = foldr (\ lines acc -> if isInGroup lines line then ((line:lines):(fst acc), True) else (lines:(fst acc), snd acc) ) ([],False) alines

insertInRightGroup alines line = let inter = insertInRightGroupHelper alines line
                                 in if (snd inter) == True
                                        then (fst inter)
                                        else [line]:(fst inter) 

groupLines lines = foldr (\ line acc -> insertInRightGroup acc line) [] lines

sortLines lines = sortBy (\ (Line a b) (Line c d) -> if  a == c then compare b d else compare a c) lines

findLast (x:[]) = x
findLast (x:xs) = findLast xs

collapseLines lines = let (Line a b) = head lines
                          (Line c d) = findLast lines
                      in (Line a d)
mySolve points = let lines = makeLines points
                     goodLines = filter (isGoodLine points) lines 
                     groupedLines = groupLines goodLines -- `debug` show goodLines
                     sortedLines = map sortLines groupedLines -- `debug` show groupedLines
                     collapsedLines = map collapseLines sortedLines -- `debug` show sortedLines
                 in sumLineLengths collapsedLines  --`debug` show collapsedLines

main :: IO ()
main = do
  n <- readLn :: IO Int
  content <- getContents
  let  
    points = map (\[x, y] -> (x, y)). map (map (read::String->Int)). map words. lines $ content
    ans = solve points
  printf "%.1f\n" ans
