{-# LANGUAGE FlexibleContexts #-}

import           Text.Regex.Posix
import           Data.Maybe
import           Data.List
import           Data.Graph
import qualified Data.Map.Strict as M
import           Control.Applicative

main = interact $ show . solve . parse

parse :: String -> [(String,Int,[String])]
parse = map go . lines where
  go l = (p,w,ps) where
    (p:ps) = getAllTextMatches (l =~ "[a-z]+")
    w = read $ mrMatch $ l =~ "[0-9]+"

solve :: [(String, Int, [String])] -> (String, Int)
solve ns = (i,weight) where
  base@((i,_),_,_) = v2n $ head $ topSort g
  (g,v2n,k2v) = graphFromEdges $ snd $ mapAccumL makeNode M.empty ns
  makeNode m (i,w,is) = (m',((i,w),k,ks))
    where (m',k:ks) = mapAccumL intern m (i:is)

  Left weight = search base
  search ((_,w),_,ks) = do
    ws <- mapM (search . v2n) $ mapMaybe k2v ks
    case intruder ws of
      Nothing      -> pure (w + sum ws)
      Just (nw,iw) -> Left (inw + nw - iw)
        where Just ((_,inw),_,_) = fmap v2n . k2v . fst =<<
                                   find ((== iw) . snd) (zip ks ws)

intern :: M.Map String Int -> String -> (M.Map String Int,Int)
intern m i | Just k <- M.lookup i m = (m,k)
           | otherwise = (M.insert i k m,k) where k = M.size m

intruder :: Eq a => [a] -> Maybe (a,a)
intruder (x1:xs@(_:x3:_))
  | Just x' <- find (/= x1) xs =
      if x' /= x3 then Just (x1,x') else Just (x',x1)
  | otherwise = Nothing
intruder _ = Nothing
