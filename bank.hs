module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import System.IO

type Account = TVar Integer


-- Para compilar: ghc -dynamic bank.hs -package stm -o bank

-- To-do
    -- Adicionar concorrencia
    -- Ler de um arquivo .txt as operacoes a serem feitas!

credit :: Integer -> Account -> STM ()
credit amount account = do
    current <- readTVar account
    writeTVar account (current + amount)

debit :: Integer -> Account -> STM ()
debit amount account = do
    current <- readTVar account
    writeTVar account (current - amount )

transfer :: Integer -> Account -> Account -> STM ()
transfer amount from to = do
    fromVal <- readTVar from
    if (fromVal - amount) >= 0
        then do
               debit amount from
               credit amount to
        else retry

showAcc name acc = do
    bal <- atomically (readTVar acc)
    hPutStr stdout (name ++ ": $")
    hPutStr stdout (show bal ++ "\n")

main = do
    acc1 <- atomically (newTVar 100)
    acc2 <- atomically (newTVar 100) 
    showAcc "Conta1" acc1
    showAcc "Conta2" acc2
    atomically (debit 50 acc1)
    showAcc "Conta1" acc1
    showAcc "Conta2" acc2
    atomically (transfer 50 acc2 acc1)
    showAcc "Conta1" acc1
    showAcc "Conta2" acc2

