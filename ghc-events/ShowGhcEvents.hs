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
aggregatedRem :: Int ->
                 ([Integer] -> Rational) ->
                 [[Integer]] -> [[Rational]]
aggregatedRem _i _f l =
  let aggr = -- TODO: nothing aggregated yet
        map (\ r -> [toRational $ last r]) l
  in aggr

i1 = 10
f1 = undefined

transform :: [[Integer]] -> [[Rational]]
transform l =
  let raw =
        -- for simple math in gnuplot (TODO: the very last sample point is lost)
        map (map toRational) l
      differentialQuotient =
        -- one element shorter than l
        map diffQuot $ zip l (tail l)
      aggregatedRemaining =
        aggregatedRem i1 f1 l -- TODO
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
