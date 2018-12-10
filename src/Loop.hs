module Loop (body, stop) where



import Util
import System.Clock
import Env (Env(..))
import qualified Env
--import qualified SDL
import Control.Monad (unless)
import Control.Concurrent (threadDelay)



body :: Env e s -> IO (Env e s)
body env = do
  env' <- _keepAtMost_CLOCK (Env.fpsCap env) $
    do
      Env.render env
      events <- Env.fetch env
      pure (Env.update env events)
  Env.run env'

stop :: Env e s -> IO (Env e s)
stop = pure



_keepAtMost_CLOCK :: FPS -> IO r -> IO r
_keepAtMost_CLOCK cap action
  | cap <= 0  = error "FPS cap <= 0"
  | otherwise = do
    beg <- timestamp
    res <- action
    end <- timestamp
    let
      mcsPerFrame = 1000000 `div` cap
      gap = mcsPerFrame - (diffMcs beg end)
    --putStrLn $ "tpf: " ++ show mcsPerFrame ++ "mcs; gap: " ++ show gap ++ " mcs"
    unless (gap > mcsPerFrame) $
      threadDelay gap
    pure res
  where
    timestamp :: IO TimeSpec
    timestamp = getTime Monotonic

    diffMcs :: TimeSpec -> TimeSpec -> Int
    diffMcs beg end =
      let
        _diff = diffTimeSpec beg end
        mcs   = fromIntegral (nsec _diff) `div` 1000
        ss    = fromIntegral (sec _diff)
      in
        ss * 1000000 + mcs

{-
_keepAtMost_SDL :: FPS -> IO r -> IO r
_keepAtMost_SDL cap action
  | cap <= 0  = error "FPS cap <= 0"
  | otherwise = do
    beg <- SDL.ticks
    res <- action
    end <- SDL.ticks
    let
      tpf = 1000 `div` fromIntegral cap
      gap = tpf - (beg - end)
    putStrLn $ "gap: " ++ show gap ++ " ms"
    unless (gap > tpf) $
      threadDelay $ 1000 * fromIntegral gap
    pure res
-}
