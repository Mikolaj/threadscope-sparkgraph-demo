{-# LANGUAGE RecordWildCards #-}
module Main where

import GHC.RTS.Events as Log
import System.Environment
import Text.Printf
import Data.List
import Data.Function
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.Maybe
import System.IO
import System.Exit
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import Data.Ratio


die s = do hPutStrLn stderr s; exitWith (ExitFailure 1)

-- A quick hack, copy-pasted from List.words.
tokenize :: String -> [String]
tokenize s = case dropWhile (not . Char.isAlphaNum) s of
               "" -> []
               s' -> w : tokenize s''
                 where (w, s'') = break (not . Char.isAlphaNum) s'

-- sample input:
-- 1398000: cap 0: spark stats: 0 created, 0 converted, 0 remaining
-- (0 overflowed, 0 dud, 0 GC'd, 0 fizzled)
parse :: [String] -> Maybe [String]
parse [time, "cap", "0", "spark", "stats",
       created, "created", converted, "converted",
       rem, "remaining", overflowed, "overflowed",
       dud, "dud", gcd, "GC", "d", fizzled, "fizzled"] =
  Just [time, created, dud, overflowed, converted, gcd, fizzled, rem]
parse _ = Nothing

-- Differential quotients. TODO: Int really too small?
diffQuot :: ([Integer], [Integer]) -> [Rational]
diffQuot
  ([time1, created1, dud1, overflowed1, converted1, gcd1, fizzled1, rem1],
   [time2, created2, dud2, overflowed2, converted2, gcd2, fizzled2, rem2]) =
    let delta = time2 - time1
    in [time2 % 1000000,
        1000000 * (created2 - created1) % delta,
        1000000 * (dud2 - dud1) % delta,
        1000000 * (overflowed2 - overflowed1) % delta,
        1000000 * (converted2 - converted1) % delta,
        1000000 * (gcd2 - gcd1) % delta,
        1000000 * (fizzled2 - fizzled1) % delta]

-- Aggregates data within consecutive intervals of length i
-- according to function f. All values at sample points within a single
-- interval are overwriiten with the aggregate produced by f. Intervals with
-- no sample points inside don't contribute anything to the result.
-- Hence, the input and output lists have the same length.
-- This setup preserves information about density of sample points,
-- but avoids solid blobs at extreme zoom out. When visualized at zoom levels
-- where intervals do not collapse to a single point, the data is smoothed
-- (e.g. locally averaged out, if f is mean).
-- TODO: clean up the time units chaos
aggregatedRem :: Integer ->
                 ([(Integer, Integer)] -> [Rational]) ->
                  [(Integer, Integer)] -> [Rational]
aggregatedRem i f l =
  let agg within end [] = []
      agg within end l@((time, rem) : trs)
        | time < end = agg ((time, rem) : within) end trs
        | otherwise  = f within ++ agg [] (end + i) l
  in agg [] 0 l

i0 = 10000000  -- no aggregation
f0 = map (toRational . snd)
i1 = 10000000  -- max
f1 l = replicate (length l) $ toRational $ maximum (map snd l)
i2 = 10000000  -- min
f2 l = replicate (length l) $ toRational $ minimum (map snd l)
i3 = 10000000  -- unweghted mean
f3 l = replicate (length l) $ sum (map snd l) % genericLength l
-- For weighted mean, we'd like the following property:
-- the area under the graph, for each interval,
-- is equal to the area of the original, unaggregated graph
-- (both graphs with points connected with straight lines).
-- However, this property is hard to reconcile with property we already keep:
-- that the distribution of sample points is the same as in the original data.
-- A compromise might be to consider unequal intervals: from the first point
-- of a given unit interval to the first point of the next unempty interval,
-- and then duplicate data points at interval boundaries, one copy
-- with the mean of values of the left interval, the other copy --- the right.
-- In this way we'd get a graph in the form of unequal rectangle steps.
-- which is simple concepturally and computationally.
-- TODO after aggregating spark transitions gives extra insights.

transform :: [[Integer]] -> [[Rational]]
transform l =
  let raw =
        -- for simple math in gnuplot (TODO: the very last sample point is lost)
        map (map toRational) l
      differentialQuotient =
        -- one element shorter than l
        map diffQuot $ zip l (tail l)
      lRem = map (\ r -> (head r, last r)) l
      aggregatedRemaining =
        zipWith4 (\ v0 v1 v2 v3 -> [v0, v1, v2, v3])
        (aggregatedRem i0 f0 lRem)
        (aggregatedRem i1 f1 lRem)
        (aggregatedRem i2 f2 lRem)
        (aggregatedRem i3 f3 lRem)
  in zipWith3 (\ l1 l2 l3 -> l1 ++ l2 ++ l3)
       raw differentialQuotient aggregatedRemaining


main = do
  [file] <- getArgs

  log <- do e <- readEventLogFromFile file
            case e of
               Left  s   -> die ("Failed to parse " ++ file ++ ": " ++ s)
               Right log -> return log

  putStrLn $ unlines $ map unwords $
    map (map ((show :: Double -> String) . fromRational)) $
    transform $ map (map read) $ Maybe.catMaybes $
    map parse $ map tokenize $ lines $ ppEventLog log
