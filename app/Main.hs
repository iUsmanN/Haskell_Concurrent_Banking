module Main where

import Customer
import Control.Concurrent
import Control.Monad (forever)
import Control.Monad.Par
import Control.Parallel
import Control.Parallel.Strategies
import Control.Concurrent.ParallelIO.Global

main :: IO ()
main = do
    --Get total turns
    turns <- initializeTurns

    --Get all the customers
    customers <- initializeCustomers

    --Start all the threads
    startThreads customers turns

    --Keep checking turns to stop the main thread when turns are completed
    checkMainTurns turns

    --Read the final customers array
    finalCustomers <- readMVar customers

    --Print the customers
    putStrLn "Final Customer Data:"
    print finalCustomers

    --Stop the global thread pool
    stopGlobalPool
    
-- | Initializes the Turns MVar containing the Integer value, 100. This variable represents the total turns the banking system takes.
initializeTurns :: IO (MVar Integer)
initializeTurns = do
    turns <- newMVar 100
    return turns

-- | Initializes the Customers MVar. This returns an MVar of an array of 10 Customers.
initializeCustomers :: IO (MVar [Customer])
initializeCustomers = do
    customers <- newMVar [Customer "A" 0 1000, Customer "B" 1 1000, Customer "C" 2 1000, Customer "D" 3 1000, Customer "E" 4 1000, Customer "F" 5 1000, Customer "G" 6 1000, Customer "H" 7 1000, Customer "I" 8 1000, Customer "J" 9 1000]
    return customers

-- | Starts the Threads for each Customer in parallel
startThreads :: MVar [Customer] -> MVar Integer -> IO[()]
startThreads inputCustomers turns = do
    print "Banking Started"
    parallel allCustomers --Parallel automatically assigns each customer a separate task.
    where
        a = doTransaction (0) inputCustomers turns
        b = doTransaction (1) inputCustomers turns
        c = doTransaction (2) inputCustomers turns
        d = doTransaction (3) inputCustomers turns
        e = doTransaction (4) inputCustomers turns
        f = doTransaction (5) inputCustomers turns
        g = doTransaction (6) inputCustomers turns
        h = doTransaction (7) inputCustomers turns
        i = doTransaction (8) inputCustomers turns
        j = doTransaction (9) inputCustomers turns
        allCustomers = [a,b,c,d,e,f,g,h,i,j]

-- | Checks the turns. Recurssively calls itself till the turns are still remaining. Quits once the turns are over.
checkMainTurns :: MVar Integer -> IO()
checkMainTurns turns= do
    t <- readMVar turns
    if t > 0 then
        checkMainTurns turns
    else
        putStrLn ""