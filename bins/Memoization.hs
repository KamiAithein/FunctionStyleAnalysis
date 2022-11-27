import qualified System.Environment as Env

data FibState = FibState 
    { n :: Integer,
      
    }


fib :: Integer -> Integer
fib n = fib' FibState{n=n, a=0, b=1}

fib' :: FibState -> Integer
fib' FibState{n=0, a=a} = a
fib' FibState{n=1, b=b} = b
fib' FibState{n=n, a=a, b=b} = fib' (FibState{n=(n-1), a=b, b=a+b}) 

main = do
    [nString] <- Env.getArgs
    let n = read nString :: Integer

    putStrLn $ show $ fib n



