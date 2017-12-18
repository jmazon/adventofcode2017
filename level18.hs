{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Char
import Data.IORef
import Control.Monad.RWS
import Control.Monad.Except
import Control.Monad.Writer
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import qualified Data.Map.Strict as M

main = do
  i <- getContents
  mainPure1 i
  mainIO1 i
  main2 i

data Rcv = Rcv Int deriving Show
instance Exception Rcv

mainPure1 i = do
  putStrLn "Part 1, pure (ExceptT)"
  let prg = parse False i
  print $ getLast $ snd $ runWriter $
    runExceptT (runRWST prg ( throwError ()
                            , lift . tell . Last . Just )
                        M.empty)

mainIO1 i = do
  putStrLn "Part 1, IO (Exception)"
  let prg = parse False i
  freq <- newIORef 0
  void (runRWST prg ( liftIO $ throw . Rcv =<< readIORef freq
                    , liftIO . writeIORef freq)
                M.empty )
    `catch` \(Rcv n) -> print n

main2 i = do
  putStrLn "Part 2, IO (multithread)"
  let prg = parse True i
  zeroToOne <- newChan
  oneToZero <- newChan
  counter <- newIORef 0
  forkIO $ void (runRWST prg ( liftIO $ readChan oneToZero
                             , liftIO . writeChan zeroToOne )
                         (M.singleton 'p' 0))
  void (runRWST prg ( liftIO $ readChan zeroToOne
                    , liftIO . (modifyIORef' counter succ >>) .
                      writeChan oneToZero )
                (M.singleton 'p' 1))
    `catch` (\BlockedIndefinitelyOnMVar -> return ())
  print =<< readIORef counter

type Chans m = (m Int, Int -> m ())
type Regs = M.Map Char Int

parse :: Monad m => Bool -> String -> RWST (Chans m) () Regs m ()
parse p2 i = head is where
  is = zipWith decode [0..] . map words $ lines i
  decode ip ["snd",x] = do
    lift =<< (snd <$> ask) <*> (load x <$> get)
    adv ip
  decode ip ["set",x,y] = alu x    const   y >> adv ip
  decode ip ["add",x,y] = alu x     (+)    y >> adv ip
  decode ip ["mul",x,y] = alu x     (*)    y >> adv ip
  decode ip ["mod",x,y] = alu x (flip mod) y >> adv ip
  decode ip ["rcv",x] = do
    prev <- M.findWithDefault 0 (head x) <$> get
    when (p2 || prev > 0) $
      put =<< M.insert (head x) <$> (lift . fst =<< ask) <*> get
    adv ip
  decode ip ["jgz",x,y] = do
    prev <- load x <$> get
    if prev > 0 then jmp . (+ ip) . load y =<< get
                else adv ip
  adv ip = jmp (ip+1)
  jmp ip | ip < 0 || ip >= length is = return ()
         | otherwise = is !! ip

  alu x op y = modify $ \ds ->
    let v' = op (load y ds) (M.findWithDefault 0 (head x) ds)
    in M.insert (head x) v' ds

  load v | isAlpha (head v) = M.findWithDefault 0 (head v)
         | otherwise = const (read v)
