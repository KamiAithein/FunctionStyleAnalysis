module Main (main) where

import Lib
import qualified System.CPUTime as CPU 
import qualified System.Environment as Env
import Data.Maybe
import Control.Exception
import System.Process
import System.Exit

newTiming :: String -> Timing
newTiming label = Timing{tStart=Nothing,tEnd=Nothing,label=(Just label)}

envFrom :: Timing -> Environment
envFrom t = Environment{cpuTime=t}

newTimer :: TimerState Timing
newTimer = TimerState
    { runState = \env -> do
        currTime <- CPU.getCPUTime 
        return $ case env of
            (Environment{cpuTime=Timing{tStart=Nothing, label=label}}) -> do
                let cpuTime' = Timing{tStart=(Just currTime), tEnd=Nothing, label=label}
                let env' = Environment{cpuTime=cpuTime'}
                (cpuTime', env')

            Environment{cpuTime=Timing{tStart=tStartVal@(Just _), tEnd=Nothing, label=label}} -> do
                let cpuTime' = Timing{tStart=tStartVal, tEnd=(Just currTime), label=label}
                let env' = Environment{cpuTime=cpuTime'}
                (cpuTime', env')
                
            Environment{cpuTime=Timing{tStart=(Just _), tEnd=(Just _), label=label}} -> do
                let cpuTime' = Timing{tStart=Nothing, tEnd=Nothing, label=label}
                let env' = Environment{cpuTime=cpuTime'}
                (cpuTime', env')

    }


data BenchError = InvalidArgs String
                | ProcessError Integer
                deriving (Show)

instance Exception BenchError

timeMain :: String -> (() -> IO (ExitCode, String, String)) -> IO (Either BenchError Timing)
timeMain label toTime = do
    let timing_0 = newTiming label
    let timer = newTimer
    (timing_1, env) <- do
        (runState timer) $ envFrom timing_0

    (exitCode, stdout, stdin) <- (toTime ())

    case exitCode of
        ExitFailure code -> throw $ ProcessError (toInteger code)
        _ -> do 
                (timing_n, timer') <- (runState timer) env
                return $ Right timing_n


validateArgs :: [String] -> Either BenchError [String]
validateArgs [] = Left $ InvalidArgs "empty!"
validateArgs args = Right args

main :: IO ()
main = do
    args <- Env.getArgs 
    case validateArgs args of
            (Left err) -> throw err
            (Right (bin:rest)) -> do
                    timing <- timeMain "Process Name" (\() -> 
                            readProcessWithExitCode bin rest []
                            )
                    case timing of
                        (Left err) -> throw err
                        (Right time) -> putStrLn $ show $ time
        

        
