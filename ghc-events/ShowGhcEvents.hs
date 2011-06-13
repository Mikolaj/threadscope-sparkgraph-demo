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
-- For an interval centered around a sampling point, take the closest
-- samples around the interval and make difference quotient out of them.
-- (Incidentally, the result is equal to taking a weighted average
-- of difference quotients of all consecutive samples involved.)
-- Considering samples from outside the interval solved the difficulty
-- of intervals with only a single sample point insde and averages out
-- alternations of plateaus and peaks in areas with low sample density
-- (showing even less information that the scarce sampling already affords,
-- but in a more readable form).
-- TODO: reimplement according to the new spec
-- TODO: Int really too small?
diffQuot2 :: ([Integer], [Integer]) -> [Rational]
diffQuot2
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

-- Aggregates data within an interval of length 2*i, centered around
-- a sampling point, according to function f. The value at the sampling point
-- is then be overwriiten with the aggregate produced by f. No intervals
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
      agg lRev l@((t, v) : tvs) = apply t v lRev tvs : agg ((t, v) : lRev) tvs
      apply t v lRev l =
        let lRevI = takeWhile (\ (t1, _) -> t - t1 < i) lRev
            lI    = takeWhile (\ (t1, _) -> t1 - t < i) l
        in f v $ reverse lRevI ++ [(t, v)] ++ lI
  in agg [] l

i0 = 5000000  -- no aggregation
f0 v l = toRational v
i1 = 5000000  -- max
f1 v l = toRational $ maximum (map snd l)
i2 = 5000000  -- min
f2 v l = toRational $ minimum (map snd l)
i3 = 5000000  -- mean
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
  let raw =
        -- for simple math in gnuplot (TODO: the very last sample point is lost)
        map (map toRational) l
      differenceQuotient =
        -- one element shorter than l
        map diffQuot $ zip l (tail l)
      differenceQuotient2 =
        -- one element shorter than l
        map diffQuot2 $ zip l (tail l)
      lRem = map (\ r -> (head r, last r)) l
      aggregatedRemaining =
        zipWith4 (\ v0 v1 v2 v3 -> [v0, v1, v2, v3])
        (aggregatedRem i0 f0 lRem)
        (aggregatedRem i1 f1 lRem)
        (aggregatedRem i2 f2 lRem)
        (aggregatedRem i3 f3 lRem)
  in zipWith4 (\ l1 l2 l3 l4 -> l1 ++ l2 ++ l3 ++ l4)
       raw differenceQuotient aggregatedRemaining differenceQuotient2


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
