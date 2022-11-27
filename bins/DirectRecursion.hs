import qualified System.Environment as Env


fib :: Integer -> Integer
fib 1 = 1
fib 0 = 0
fib n = fib (n-1) + fib (n-2)

main = do
    [nString] <- Env.getArgs
    let n = read nString :: Integer

    putStrLn $ show $ fib n



