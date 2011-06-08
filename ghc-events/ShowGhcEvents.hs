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

main = do
  [file] <- getArgs

  log <- do e <- readEventLogFromFile file
            case e of
               Left  s   -> die ("Failed to parse " ++ file ++ ": " ++ s)
               Right log -> return log

  let l = map tokenize $ lines $ ppEventLog log

      -- A quick hack, copy-pasted from List.words.
      tokenize :: String -> [String]
      tokenize s = case dropWhile (not . Char.isAlphaNum) s of
                     "" -> []
                     s' -> w : tokenize s''
                       where (w, s'') = break (not . Char.isAlphaNum) s'

      -- 1398000: cap 0: spark stats: 0 created, 0 converted, 0 remaining
      -- (0 overflowed, 0 dud, 0 GC'd, 0 fizzled)
      parse :: [String] -> Maybe [String]
      parse [time, "cap", "0", "spark", "stats",
             created, "created", converted, "converted",
             rem, "remaining", overflowed, "overflowed",
             dud, "dud", gcd, "GC", "d", fizzled, "fizzled"] =
        Just $
          [time, created, dud, overflowed, converted, gcd, fizzled, rem]
      parse _ = Nothing

      -- Differential quotients. TODO: Int too small?
      diffQuot :: ([Integer], [Integer]) -> [Rational]
      diffQuot
        ([time1, created1, dud1, overflowed1, converted1, gcd1, fizzled1, rem1],
         [time2, created2, dud2, overflowed2, converted2, gcd2, fizzled2, rem2])
        =
        let delta = time2 - time1
        in [time2 % 1000000,
            1000000 * (created2 - created1) % delta,
            1000000 * (dud2 - dud1) % delta,
            1000000 * (overflowed2 - overflowed1) % delta,
            1000000 * (converted2 - converted1) % delta,
            1000000 * (gcd2 - gcd1) % delta,
            1000000 * (fizzled2 - fizzled1) % delta,
            rem2 % 1]

      dQ :: [[Integer]] -> [[Rational]]
      dQ l = map diffQuot $ zip l (tail l)

  putStrLn $ unlines $ map unwords $
    map (map ((show :: Double -> String) . fromRational)) $
    dQ $ map (map read) $ Maybe.catMaybes $ map parse l

die s = do hPutStrLn stderr s; exitWith (ExitFailure 1)
