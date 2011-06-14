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

-- Ordinary difference quotients.
-- Bad, because data not continuous, so no derivative, so infinities and NaNs.
diffQuot :: ([Integer], [Integer]) -> [Rational]
diffQuot (start, end)
  | head start == head end = map toRational [head start, 0, 0, 0, 0, 0, 0]
diffQuot
  ([time1, created1, dud1, overflowed1, converted1, gcd1, fizzled1, rem1],
   [time2, created2, dud2, overflowed2, converted2, gcd2, fizzled2, rem2]) =
    let delta = time2 - time1
    in [toRational time2,
        (created2 - created1) % delta,
        (dud2 - dud1) % delta,
        (overflowed2 - overflowed1) % delta,
        (converted2 - converted1) % delta,
        (gcd2 - gcd1) % delta,
        (fizzled2 - fizzled1) % delta]

-- Averaged difference quotients.
-- For an interval of length 2*i, centered around a sampling point,
-- takes the closest samples around the interval
-- and makes difference quotient out of them.
-- (Incidentally, the result is equal to taking a weighted average
-- of difference quotients of all consecutive samples involved.)
-- Considering samples from outside the interval solves the difficulty
-- of intervals containing only a single sample point and averages out
-- alternations of plateaus and peaks in areas with low sample density
-- (showing even less information that the scarce sampling already affords,
-- but in a more readable form).
diffQuotAverage :: Integer -> [[Integer]] -> [[Rational]]
diffQuotAverage i l =
  let agg lRev [] = []
      agg lRev (current : rest) =
        apply current lRev rest : agg (current : lRev) rest
      apply current@(t:_) lRev l =
        let start = head $ dropWhile (\ (t1:_) -> t - t1 < i) lRev ++ [current]
            end   = head $ dropWhile (\ (t1:_) -> t1 - t < i) l    ++ [current]
        in toRational t : tail (diffQuot (start, end))
  in agg [] l

-- Aggregates data within an interval of length 2*i, centered around
-- a sampling point, according to function f. The value at the sampling point
-- is then be overwriten with the aggregate produced by f. No intervals
-- with no sample points inside contribute anything to the result,
-- hence the input and output lists have the same length.
-- This setup preserves information about the location of sampling points,
-- but avoids solid blobs at extreme zoom out. When visualized at zoom levels
-- where intervals do not collapse to a single point, the data is smoothed
-- (e.g. locally averaged out, if f is mean).
aggregatedRem :: Integer ->
                 (Integer -> [(Integer, Integer)] -> Rational) ->
                 [(Integer, Integer)] -> [Rational]
aggregatedRem i f l =
  let agg lRev [] = []
      agg lRev (current : rest) =
        apply current lRev rest : agg (current : lRev) rest
      apply current@(t, v) lRev l =
        let lRevI = takeWhile (\ (t1, _) -> t - t1 < i) lRev
            lI    = takeWhile (\ (t1, _) -> t1 - t < i) l
        in f v $ reverse lRevI ++ [current] ++ lI
  in agg [] l

i0 = 10000000  -- no aggregation
f0 v l = toRational v
i1 = 10000000  -- max
f1 v l = toRational $ maximum (map snd l)
i2 = 10000000  -- min
f2 v l = toRational $ minimum (map snd l)
i3 = 10000000  -- mean
f3 v l = sum (map snd l) % genericLength l
-- Warning: this mean implementation is simple, readable and preserves
-- information about local variations, but we don't have the following property:
-- the area under the graph is equal to the area of the original,
-- unaggregated graph (both graphs with points connected with straight lines).
-- The reason is that we don't take time spans between sample points
-- into account (see "snd"). In particular we ignore time spans
-- in, possibly big, empty areas without any sample points.
-- The more homogeneous the sampling points distribution,
-- the closer we are to keeping the property.

transform :: [[Integer]] -> [[Rational]]
transform l =
  let rescale (time : values) =
        -- from nano to milliseconds
        time * (1 % 1000000) : map (* 1000000) values
      raw =
        -- for simple math in gnuplot
        map (map toRational) l
      differenceQuotient =
        -- one element shorter than l
        map diffQuot $ zip l (tail l)
      differenceQuotient2 =
        let interval = 10000000
        in diffQuotAverage interval l
      lRem = map (\ r -> (head r, last r)) l
      aggregatedRemaining =
        zipWith4 (\ v0 v1 v2 v3 -> [v0, v1, v2, v3])
        (aggregatedRem i0 f0 lRem)
        (aggregatedRem i1 f1 lRem)
        (aggregatedRem i2 f2 lRem)
        (aggregatedRem i3 f3 lRem)
  in zipWith4 (\ l1 l2 l3 l4 -> l1 ++ l2 ++ l3 ++ l4)
       raw
       (map rescale differenceQuotient)
       aggregatedRemaining
       (map rescale differenceQuotient2)

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
