module Main (main) where

import Lib
import qualified System.CPUTime as CPU 

newTiming :: Timing
newTiming = Timing{tStart=Nothing,tEnd=Nothing}

envFrom :: Timing -> Environment
envFrom t = Environment{cpuTime=t}

newTimer :: TimerState Timing
newTimer = TimerState
    { runState = \env -> do
        currTime <- CPU.getCPUTime 
        pure $ case env of
            (Environment{cpuTime=Timing{tStart=Nothing}}) -> do
                let cpuTime' = Timing{tStart=(Just currTime), tEnd=Nothing}
                let env' = Environment{cpuTime=cpuTime'}
                (cpuTime', env')

            Environment{cpuTime=Timing{tStart=tStartVal@(Just _), tEnd=Nothing}} -> do
                let cpuTime' = Timing{tStart=tStartVal, tEnd=(Just currTime)}
                let env' = Environment{cpuTime=cpuTime'}
                (cpuTime', env')
                
            Environment{cpuTime=Timing{tStart=(Just _), tEnd=(Just _)}} -> do
                let cpuTime' = Timing{tStart=Nothing, tEnd=Nothing}
                let env' = Environment{cpuTime=cpuTime'}
                (cpuTime', env')

    }

main :: IO ()
main = do
    let timing_0 = newTiming
    (timing_1, timer) <- do
        (runState newTimer) envFrom timing_0

    putStrLn $ show timing_1
