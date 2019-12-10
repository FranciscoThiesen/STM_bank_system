module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import System.IO

type Account = TVar Integer

credit :: Integer -> Account -> STM ()
credit amount account = do 
    current <- readTVar account;
    writeTVar account (current + amount)

debit :: Integer -> Account -> STM ()
debit amount account = do
    current <- readTVar account
    check $ amount <= current
    writeTVar account (current - amount)


transfer :: Integer -> Account -> Account -> STM ()
transfer amount from to = do
    fromVal <- readTVar from
    check $ fromVal >= amount
    debit amount from
    credit amount to

custom_transfer :: Integer -> Account -> Account -> Account -> STM ()
custom_transfer amount from1 from2 to = do
    orElse (transfer amount from1 to) $ (transfer amount from2 to)

showAcc name acc = do
    bal <- atomically (readTVar acc)
    hPutStr stdout (name ++ ": $")
    hPutStr stdout (show bal ++ "\n")

showAccs x y z = do
    showAcc "Conta1" x 
    showAcc "Conta2" y
    showAcc "Conta3" z
    hPutStr stdout "\n"

main = do
    acc1 <- atomically (newTVar 40)
    acc2 <- atomically (newTVar 50)
    acc3 <- atomically (newTVar 0)
    showAccs acc1 acc2 acc3
    forkIO $ (atomically $ custom_transfer 49 acc1 acc2 acc3)
    atomically $ custom_transfer 20 acc3 acc1 acc2
    showAccs acc1 acc2 acc3
