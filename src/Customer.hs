module Customer (
    Customer(..),
    doTransaction,
    getAccountNumber, 
    replaceAtIndex,
    getName,
    getRandomAccountNumber,
    cannotMakeTransaction,
    deductAmount,
    addAmount,
    checkTurns,
    displayTurn
) where

import Control.Concurrent
import System.Random
import Data.List

type Name = String
type AccountNumber = Int
type AccountBalance = Int
type TransactionAmount = Int

data Customer = Customer Name AccountNumber AccountBalance deriving (Show)

-- | Replaces the value at a particular index in a list.
replaceAtIndex :: Int -> a -> [a] -> [a]    
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i+1) xs

-- | Returns the account number of the customer
getAccountNumber :: Customer -> AccountNumber
getAccountNumber (Customer name accNum balance) = accNum

-- | Returns the name of the customer
getName :: Customer -> Name
getName (Customer name accNum balance) = name

-- | Generates and returns a random account number different than that of the current customer.
getRandomAccountNumber :: AccountNumber -> IO AccountNumber
getRandomAccountNumber selfnum = do
    number <- randomRIO (0 :: Int, 9 :: Int)

    if number == selfnum then do
        num <- getRandomAccountNumber selfnum
        return num
    else
        return number

-- | Returs a boolean value. True if the user cannot make a transaction. False otherwise.
cannotMakeTransaction :: Customer -> TransactionAmount -> IO Bool
cannotMakeTransaction (Customer name accNum balance) amount = do
    if (balance - amount) >= 0 then
        return False
    else
        return True

-- | Deducts the amount from a customer
deductAmount :: Customer -> TransactionAmount -> Customer
deductAmount (Customer name id money) amount = Customer name id (money - amount)

-- | Adds the amount to a customer
addAmount :: Customer -> TransactionAmount -> Customer
addAmount (Customer name id money) amount = Customer name id (money + amount)

-- | Checks if turns are remaining for a transaction to take place
checkTurns :: MVar Integer -> IO Bool
checkTurns turns= do
    t <- readMVar turns
    if t < 1 then
        return True
    else
        return False

-- | Prints the summary of the transaction that has taken place
displayTurn :: Integer -> Customer -> Customer -> TransactionAmount -> IO()
displayTurn turn from to amount = do
    putStrLn $ "Turn " ++ (show $ turn+1) ++ ": " ++ (getName from) ++ " transferred " ++ (show amount) ++ " to " ++ (getName to)

-- | A Recurssively called function simulating the transactions done by the customers.
doTransaction :: Int -> MVar [Customer] -> MVar Integer -> IO()
doTransaction selfAccNum customers turns = do
    
    -- Delay
    delay <- randomRIO (10000 :: Int, 50000 :: Int)
    threadDelay delay

    -- Get updated data for self customer
    c <- readMVar customers
    let self = c!!selfAccNum

    -- Get random value
    transferAmount <- randomRIO (10 :: Int, 50 :: Int)
    noTransaction <- cannotMakeTransaction self transferAmount
    turnsCheck <- checkTurns turns

    if noTransaction || turnsCheck then
        putStrLn "Stopped Banking."
    else do

        -- Count turn
        takeTurns <- takeMVar turns
        -- print takeTurns
        putMVar turns (takeTurns - 1)

        -- Get random account number
        randomAccountNumber <- getRandomAccountNumber selfAccNum

        --Updated customers
        let other = c!!randomAccountNumber
        let updatedSelf = deductAmount self transferAmount
        let updatedOther = addAmount other transferAmount

        -- Updating Customers MVar
        takeCustomers <- takeMVar customers
        let customers2 = replaceAtIndex selfAccNum updatedSelf takeCustomers
        let customers3 = replaceAtIndex randomAccountNumber updatedOther customers2
        putMVar customers customers3
        
        -- Display the Turn Summary
        displayTurn (100-takeTurns) self other transferAmount

        -- Recurssive call to continue
        doTransaction selfAccNum customers turns