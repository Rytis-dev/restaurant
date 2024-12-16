{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.IO.Class(liftIO)
import Data.String.Conversions
import Web.Scotty
import Control.Concurrent.Chan (Chan, newChan)
import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Maybe (fromMaybe)
import GHC.Conc (forkIO)
import qualified Lib2
import qualified Lib3

main :: IO ()
main = do
  chan <- newChan :: IO (Chan Lib3.StorageOp)
  state <- newTVarIO Lib2.emptyState
  _ <- forkIO $ Lib3.storageOpLoop chan
  scotty 3000 $
    post "/" $ do
      b <- body
      liftIO $ putStrLn ("Request was:" ++ cs b)
      response <- liftIO $ process state chan $ cs b
      text $ cs response
      
process :: TVar Lib2.StateD -> Chan Lib3.StorageOp -> String -> IO String
process state storageChan input = case Lib3.parseCommand input of
  Left e -> do
    putStrLn $ "Error parsing request: " ++ e
    return $ "Error: " ++ e
  Right (cmd, _) -> do
    info <- Lib3.stateTransition state cmd storageChan
    case info of
      Left e -> do
        putStrLn $ "Error parsing request: " ++ e
        return $ "Error: " ++ e
      Right output -> do
        let response = fromMaybe "No response" output
        putStrLn $ "Response: " ++ response
        return response