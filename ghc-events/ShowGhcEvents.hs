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
             remaining, "remaining", overflowed, "overflowed",
             dud, "dud", gcd, "GC", "d", fizzled, "fizzled"] =
        Just
          [time, created, dud, overflowed, converted, gcd, fizzled, remaining]
      parse _ = Nothing

  putStrLn $ unlines $ map unwords $ Maybe.catMaybes $ map parse l

die s = do hPutStrLn stderr s; exitWith (ExitFailure 1)
