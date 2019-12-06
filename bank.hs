module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import System.IO

type Account = TVar Integer


-- Para compilar: ghc -dynamic bank.hs -package stm -o bank

-- To-do
    -- Adicionar transferencia entre contas ( idealmente )
    -- Adicionar concorrencia
    -- Ler de um arquivo .txt as operacoes a serem feitas!



-- Essa funcao so deve deixar sacar se existir saldo suficiente
limitedWithdraw :: Account -> Integer -> STM ()
limitedWithdraw acc amount = do
    bal <- readTVar acc
    check (amount <= 0 || amount <= bal)
    writeTVar acc (bal - amount)

showAcc name acc = do
    bal <- atomically (readTVar acc)
    hPutStr stdout (name ++ ": $")
    hPutStr stdout (show bal ++ "\n")

main = do
    acc <- atomically (newTVar 100)
    showAcc "Conta corrente" acc
    atomically (limitedWithdraw acc 50)
    showAcc "Conta corrente" acc
    
