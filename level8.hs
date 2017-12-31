{-# LANGUAGE FlexibleContexts #-}

import qualified Data.Map.Strict as M
import           Data.List        (scanl')
import           Text.Regex.Posix ((=~),getAllTextSubmatches)
import           Control.Arrow    ((&&&))

main = interact $ format . scanl' solve M.empty . parse

parse = map f . lines where
  f l = ( (ir,readArith io (read ii))
        , (cr,readRel   co (read ci)) ) where
    [ir,io,ii,cr,co,ci] = tail $ getAllTextSubmatches $
      l =~ "([a-z]+) (inc|dec) (-?[0-9]+) if ([a-z]+) ([<>=!]+) (-?[0-9]+)"
  readArith "inc" = (+)
  readArith "dec" = subtract
  readRel ">"  = (< ) -- I'm blatantly sparing a `flip` here too ^^
  readRel ">=" = (<=)
  readRel "==" = (==)
  readRel "!=" = (/=)
  readRel "<"  = (> )
  readRel "<=" = (>=)
        
solve m (instr,cond) | evalOp m cond = M.insert (fst instr) (evalOp m instr) m
                     | otherwise = m
  where evalOp m (r,o) = o (maybe 0 id (M.lookup r m))

format = show . (last &&& maximum) . map maximum . dropWhile M.null
