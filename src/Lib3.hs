{-# LANGUAGE InstanceSigs #-}

module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    Statements(..),
    renderQuery
    ) where

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.STM (STM, TVar, atomically, readTVar, readTVarIO, writeTVar)
import Control.Monad (forever)
import qualified Lib2
import Lib2 (Parser, parse, parseLiteral, parseWord)
import Control.Applicative (Alternative (many), (<|>))
import System.Directory (doesFileExist)
import Data.Maybe ( fromJust, isNothing )
import Data.List (intercalate)

data StorageOp = Save String (Chan ()) | Load (Chan (Maybe String))

storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop opChan = forever $ do
  op <- readChan opChan
  case op of
    Save s chan -> do
      writeFile "state.txt" s
      writeChan chan ()
    Load chan -> do
      exists <- doesFileExist "state.txt"
      if exists
        then do
          s' <- readFile "state.txt"
          writeChan chan $ Just s'
        else writeChan chan Nothing

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Eq)

instance Show Statements where
  show :: Statements -> String
  show (Single q) = show q
  show (Batch qs) = "BEGIN\n" ++ concatMap ((++ ";\n") . renderQuery) qs ++ "END\n"

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand str = case parse (StatementCommand <$> statements <|> parseLoad <|> parseSave) str of
  (Left e, _) -> Left e
  (Right c, r) -> Right (c, r)
  
parseLoad :: Parser Command
parseLoad = do
  _ <- parseWord "load"
  return LoadCommand

parseSave :: Parser Command
parseSave = do
  _ <- parseWord "save"
  return SaveCommand

parseStatements :: String -> Either String (Statements, String)
parseStatements str = case parse statements str of
  (Left e, _) -> Left e
  (Right s, r) -> Right (s, r)
  
marshallState :: Lib2.StateD -> Statements
marshallState state = Batch queries
  where
    orderQueries = map (\(customerName, items) -> Lib2.TakeOrder customerName items) (Lib2.orderList state)
    kitchenQueries = concatMap (\(orderId, status) -> 
                                case status of
                                  "Order placed"   -> []
                                  "Order prepared" -> [Lib2.PrepareOrder orderId]
                                  "Order served"   -> [Lib2.ServeOrder orderId]
                                  "Done"           -> 
                                    let paymentMethod = Lib2.Cash  -- Default
                                        amount = (20, 0) -- Default
                                    in [Lib2.HandlePayment orderId paymentMethod amount]
                                  _                -> error $ "Unknown status: " ++ status
                               ) (Lib2.kitchenList state)
  
    queries = orderQueries ++ kitchenQueries


renderQuery :: Lib2.Query -> String
renderQuery (Lib2.TakeOrder name items) =
  "take_order(" ++ name ++ renderItems items ++ ")"
renderQuery (Lib2.PrepareOrder id ) =
  "prepare_order(" ++ show id ++ ")"
renderQuery (Lib2.ServeOrder id) =
  "serve_order(" ++ show id ++ ")"
renderQuery (Lib2.HandlePayment id method num) =
  "handle_payment(" ++ show id ++ "," ++ show method ++ "," ++ renderTuple num ++ ")"
renderQuery (Lib2.SeatCustomer name seat) =
  "seat_customer(" ++ name ++ "," ++ show seat ++ ")"
renderQuery Lib2.Debug = "Debug"
renderQuery (Lib2.RestaurantOperation queries) =
  "wine_factory(" ++ renderQueries queries ++ ")"

renderItems :: [(String, Integer)] -> String
renderItems [] = "" 
renderItems items = concatMap (\(item, qty) -> "," ++ item ++ "," ++ show qty) items

renderTuple :: (Integer, Integer) -> String
renderTuple (whole, ptr) = show whole ++ "." ++ show ptr

renderQueries :: [Lib2.Query] -> String
renderQueries = intercalate ", " . map renderQuery

renderStatements :: Statements -> String
renderStatements (Single q) = renderQuery q
renderStatements (Batch qs) = "BEGIN\n" ++ concatMap ((++ ";\n") . renderQuery) qs ++ "END\n"

stateTransition :: TVar Lib2.StateD -> Command -> Chan StorageOp ->
                   IO (Either String (Maybe String))
stateTransition s SaveCommand ioChan = do
  s' <- readTVarIO s
  chan <- newChan :: IO (Chan ())
  writeChan ioChan (Save (renderStatements $ marshallState s') chan)
  readChan chan
  return $ Right $ Just "State saved successfully"
stateTransition s LoadCommand ioChan = do
  chan <- newChan :: IO (Chan (Maybe String))
  writeChan ioChan (Load chan)
  qs <- readChan chan
  if isNothing qs
    then return (Left "No state file found")
    else case parseStatements $ fromJust qs of
      Left e -> do
        return $ Left $ "Failed to load from file:\n" ++ e
      Right (qs', _) -> stateTransition s (StatementCommand qs') ioChan
stateTransition s (StatementCommand sts) _ = atomically $ atomicStatemets s sts

transitionThroughList :: Lib2.StateD -> [Lib2.Query] -> Either String (Maybe String, Lib2.StateD)
transitionThroughList _ [] = Left "Empty query list"
transitionThroughList s (q : qs) = case Lib2.stateTransition s q of
  Left e -> Left e
  Right (msg, ns) ->
    if null qs
      then Right (msg, ns)
      else case transitionThroughList ns qs of
        Left e -> Left e
        Right (msg', ns') -> Right ((\x y -> x ++ "\n" ++ y) <$> msg <*> msg', ns')

atomicStatemets :: TVar Lib2.StateD -> Statements -> STM (Either String (Maybe String))
atomicStatemets s (Batch qs) = do
  s' <- readTVar s
  case transitionThroughList s' qs of
    Left e -> return $ Left e
    Right (msg, ns) -> do
      writeTVar s ns
      return $ Right msg
atomicStatemets s (Single q) = do
  s' <- readTVar s
  case Lib2.stateTransition s' q of
    Left e -> return $ Left e
    Right (msg, ns) -> do
      writeTVar s ns
      return $ Right msg

statements :: Parser Statements
statements =
  ( do
      _ <- parseLiteral "BEGIN\n"
      q <-
        many
          ( do
              q <- Lib2.parseTask
              _ <- parseLiteral ";\n"
              return q
          )
      _ <- parseLiteral "END\n"
      return $ Batch q
  )
    <|> (Single <$> Lib2.parseTask)