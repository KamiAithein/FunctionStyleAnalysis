module Lib where

{-

https://www.reddit.com/r/haskellquestions/comments/5dinqr/creating_my_own_monad_to_hide_a_global_variable/

newtype TimerState s a = TimerState { runState :: s -> (a, s) }

-- Functor on a state will get the current state and then apply f to the val of the state
instance Functor (TimerState s) where
    fmap f sa = TimerState $ \s -> 
        let (a, s') = runState sa s
        in (f a, s')

-- TODO I dont get this???????
instance Applicative (TimerState s) where
    pure a = TimerState $ \s -> (a, s)
    (<*>) :: TimerState s (a -> b) -> TimerState s a -> TimerState s b
    TimerState s'a2b <*> TimerState s'a =
        TimerState $ \s ->
            let (f, s' ) = s'a2b s
                (a, s'') = s'a   s'
            in  (f a, s'')

instance Monad (TimerState s) where
    return = pure
    sa >>= f =
        TimerState $ \s ->
            let (a, s')  = runState sa s
                sb       = f a
                (b, s'') = runState sb s'
            in  (b, s'')

-}

import System.CPUTime

data Timing = Timing
    { tStart :: Maybe Integer
    , tEnd   :: Maybe Integer
    }
    deriving (Show)

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
            pure (b, env'')