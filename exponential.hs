{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- My Solution to https://www.hackerrank.com/challenges/eval-ex/problem

eh :: Double -> Int -> (Double, Double)
eh x l = if l == 0
            then (1.0, 1.0) 
            else let a = (eh x (l-1))
                     s = fst a
                     h = snd a
                     n = (h * x) / (fromIntegral l) 
                 in (s + n, n)
e x l = fst (eh x l)
main :: IO()
main = do
    n <- readLn :: IO Int

    forM_ [1..n] $ \n_itr -> do
        x <- readLn :: IO Double
        print (e x 9)
