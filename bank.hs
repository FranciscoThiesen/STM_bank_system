module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import System.IO

type Account = TVar Integer


-- Para compilar: ghc -dynamic bank.hs -package stm -o bank

-- To-do
    -- Adicionar concorrencia


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

cascade_transfer :: Integer -> Account -> Account -> Account -> STM ()
cascade_transfer amount from1 from2 to = do
    orElse (transfer amount from1 to) $ (transfer amount from2 to)

showAcc name acc = do
    bal <- atomically (readTVar acc)
    hPutStr stdout (name ++ ": $")
    hPutStr stdout (show bal ++ "\n")

main = do
    acc1 <- atomically (newTVar 40)
    acc2 <- atomically (newTVar 50)
    acc3 <- atomically (newTVar 0)
    showAcc "Conta1" acc1
    showAcc "Conta2" acc2
    showAcc "Conta3" acc3
    atomically $ cascade_transfer 49 acc1 acc2 acc3
    showAcc "Conta1" acc1
    showAcc "Conta2" acc2
    showAcc "Conta3" acc3

