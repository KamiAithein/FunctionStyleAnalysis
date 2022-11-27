module Lib where
    
import System.CPUTime

data Timing = Timing
    { tStart :: Maybe Integer
    , tEnd   :: Maybe Integer
    , label  :: Maybe String
    }

instance Show Timing where
    show timing@(Timing{label=tLabel}) = 
        ( header tLabel
        . formatter) timing
        where   header label s =
                    let labelstr = 
                            case label of
                                Just sLabel -> 
                                    sLabel ++ "\n"
                                    ++ "----------\n"
                                _ -> "" 
                    in
                        "**********\n" 
                        ++ labelstr
                        ++ s 
                        ++ "**********\n"

                formatter Timing{tStart=(Just tStartPS), tEnd=(Just tEndPS)} =
                    "Run Completed!\n"
                    ++ "start: " ++ show tStartPS                            ++ "ps\n"
                    ++ "end: "   ++ show tEndPS                              ++ "ps\n"
                    ++ "dtime: " ++ ((show . ps_to_ms) (tEndPS - tStartPS))  ++ "ms\n"
                formatter Timing{tStart=tStartPS, tEnd=tEndPS} = 
                    "Run Incomplete!\n"
                    ++ "start: " ++ show tStartPS ++ "ps\n"
                    ++ "end: "   ++ show tEndPS   ++ "ps\n"
                    ++ "dtime: " ++      "N/A"    ++ "\n"
                ps_to_ms :: Integer -> Float
                ps_to_ms ps = (fromIntegral ps :: Float) / (1000000000 :: Float) --divide
            

data Environment = Environment
    { cpuTime :: Timing
    }
    deriving (Show)

newtype TimerState a = TimerState 
    { runState :: Environment -> IO (a, Environment) 
    }

-- Functor on a state will get the current state and then apply f to the val of the state
instance Functor (TimerState) where
    fmap f enva = TimerState $ \env -> do
        (a, env') <- runState enva env
        pure (f a, env')

-- TODO I dont get this???????
instance Applicative (TimerState) where
    pure a = TimerState $ \env -> pure (a, env)
    TimerState env'a2b <*> TimerState env'a =
        TimerState $ \env -> do
            (f, env' ) <- env'a2b env
            (a, env'') <- env'a   env'
            pure (f a, env'')

instance Monad (TimerState) where
    return = pure
    enva >>= f =
        TimerState $ \env -> do
            (a, env')  <- runState enva env
            let envb   =  f a
            (b, env'') <- runState envb env'
            return (b, env'')