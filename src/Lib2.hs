{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE LambdaCase #-}

module Lib2
    ( Query(..),
      Parser(..),
      PaymentMethod(..),
      Seat(..),
      parseQuery,
      StateD(..),
      emptyState,
      stateTransition,
      parseLiteral,
      parseTask,
      parseWord,
      parse
    ) where

import qualified Data.Char as C
import Control.Monad.Trans.Except ( runExceptT, ExceptT )
import Control.Monad.Trans.State
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (MonadTrans (..), lift)
import Control.Applicative (optional, Alternative (..))


--newtype Parser a = P {parse :: String -> Either String (a, String)}

type Parser a = ExceptT String (State String) a

parse :: Parser a -> String -> (Either String a, String)
parse parser = runState (runExceptT parser)

data Query
    = TakeOrder String [(String, Integer)]
    | PrepareOrder Integer
    | ServeOrder Integer
    | HandlePayment Integer PaymentMethod (Integer, Integer)
    | SeatCustomer String Seat
    | Debug 
    | RestaurantOperation [Query]
  deriving(Eq, Show)

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery s =
  case parse parseTaskList s of
    (Left e, _) -> Left e
    (Right qs, r) ->
      if null r
        then case qs of
          [q] -> Right q
          _ -> Right (RestaurantOperation qs)
        else Left ("Unrecognized characters: " ++ r)
  
data PaymentMethod = Cash | Card | MobilePayment
   deriving (Eq, Show)

data Seat = Bar | Outdoor | DiningTable
  deriving (Eq, Show)

data StateD = StateD
  { orderList :: [(String, [(String, Integer)])], -- customer name and their items
    kitchenList :: [(Integer, String)], -- order ID and status
    revenue :: Integer, -- total revenue
    nextOrderId :: Integer -- next order ID to be assigned
  }
  deriving (Eq, Show)

emptyState :: StateD
emptyState = StateD
  { orderList = [],
    kitchenList = [],
    revenue = 0,
    nextOrderId = 1
  }

stateTransition :: StateD -> Query -> Either String (Maybe String, StateD)
stateTransition state query = case query of
  TakeOrder customerName items ->
    let newOrderId = nextOrderId state
        newOrderList = (customerName, items) : orderList state
        newKitchenList = (newOrderId, "Order placed") : kitchenList state
        newNextOrderId = newOrderId + 1
        msg = "Order taken for " ++ customerName ++ ". Your order ID is " ++ show newOrderId ++ "."
    in Right (Just msg, state { orderList = newOrderList, kitchenList = newKitchenList, nextOrderId = newNextOrderId })
  
  PrepareOrder orderId ->
    case lookup orderId (kitchenList state) of
      Just status ->
            let updatedKitchenList = map (\(id, s) -> if id == orderId then (id, "Order prepared") else (id, s)) (kitchenList state)
                msg = "Order " ++ show orderId ++ " is now prepared."
            in Right (Just msg, state { kitchenList = updatedKitchenList })
      Nothing -> Left "Order not found"

  ServeOrder orderId ->
    case lookup orderId (kitchenList state) of
      Just status ->
        let updatedKitchenList = map (\(id, s) -> if id == orderId then (id, "Order served") else (id, s)) (kitchenList state)
            msg = "Order " ++ show orderId ++ " served successfully."
        in Right (Just msg, state { kitchenList = updatedKitchenList })
      Nothing -> Left "Order not found"

  HandlePayment orderId method amount ->
    case lookup orderId (kitchenList state) of
      Just status ->
        let updatedKitchenList = map (\(id, s) -> if id == orderId then (id, "Done") else (id, s)) (kitchenList state)
            newRevenue = revenue state + 20
            msg = "Payment of " ++ "20" ++ " processed for order " ++ show orderId ++ "."
        in Right (Just msg, state { kitchenList = updatedKitchenList, revenue = newRevenue })

  -- SeatCustomer customerName seat -> 
  --   let msg = "Seated " ++ customerName ++ " at " ++ show seat ++ "."
  --   in Right (Just msg, state)

  Debug -> 
    let msg = show state
    in Right (Just msg, state)

parseWord :: String -> Parser String
parseWord = parseLiteral

parseNumber :: Parser Integer
parseNumber = do
  digits <- some (sat C.isDigit)
  return $ read digits

-- <string> ::= <letter> | <letter> <string>
parseString :: Parser String
parseString = do
  input <- lift get
  case input of
    [] -> throwError "Input is empty. No string found."
    _ -> do
      letters <- some (sat C.isLetter)
      return letters

-- <quantity> ::= <number>
parseQuantity :: Parser Integer
parseQuantity = parseNumber

-- <item_name> ::= <string>
parseItemName :: Parser String
parseItemName = parseString

-- <amount> ::= <number> "." <number>
parseAmount :: Parser (Integer, Integer)
parseAmount = do
  wholePart <- parseNumber
  _ <- parseChar '.'
  fractionalPart <- parseNumber
  return (wholePart, fractionalPart)

-- <payment_method> ::= "Cash" | "Card" | "MobilePayment"
parsePaymentMethod :: Parser PaymentMethod
parsePaymentMethod =
  (parseWord "Cash" >> return Cash)
    <|> (parseWord "Card" >> return Card)
    <|> (parseWord "MobilePayment" >> return MobilePayment)

-- <order_id> ::= <number>
parseOrderID :: Parser Integer
parseOrderID = parseNumber

-- <item> ::= <item_name> "," <quantity>
parseItem :: Parser (String, Integer)
parseItem = do 
  name <- parseItemName
  _ <- parseChar ','
  q <- parseQuantity
  return (name, q)

-- <order_items> ::= <item> | <item> "," <order_items>
parseOrderItems :: Parser [(String, Integer)]
parseOrderItems = do 
  firstQuery <- parseItem
  rest <- optional (parseChar ',' >> parseOrderItems)
  return $ case rest of
    Just otherQueries -> firstQuery : otherQueries
    Nothing -> [firstQuery] 

-- <customer_name> ::= <string>
parseCustomerName :: Parser String
parseCustomerName = parseString

-- <seat> ::= "Bar" | "Outdoor" | "DiningTable"
parseSeat :: Parser Seat
parseSeat =
  (parseWord "Bar" >> return Bar)
    <|> (parseWord "Outdoor" >> return Outdoor)
    <|> (parseWord "DiningTable" >> return DiningTable)

-- <seat_customer> ::= "seat_customer" "(" <customer_name> "," <seat> ")"
parseSeatCustomer :: Parser Query
parseSeatCustomer = do 
  _ <- parseWord "seat_customer"
  _ <- parseChar '('
  name <- parseCustomerName
  _ <- parseChar ','
  seat <- parseSeat
  _ <- parseChar ')'
  return $ SeatCustomer name seat

-- <handle_payment> ::= "handle_payment" "(" <order_id> "," <payment_method> "," <amount> ")"
parseHandlePayment :: Parser Query
parseHandlePayment = do 
  _ <- parseWord "handle_payment"
  _ <- parseChar '('
  orderid <- parseOrderID
  _ <- parseChar ','
  pmethod <- parsePaymentMethod
  _ <- parseChar ','
  amount <- parseAmount
  _ <- parseChar ')'
  return $ HandlePayment orderid pmethod amount

-- <serve_order> ::= "serve_order" "(" <order_id> ")"
parseServeOrder :: Parser Query
parseServeOrder = do 
  _ <- parseWord "serve_order"
  _ <- parseChar '('
  orderid <- parseOrderID
  _ <- parseChar ')'
  return $ ServeOrder orderid

-- <prepare_order> ::= "prepare_order" "(" <order_id> ")"
parsePrepareOrder :: Parser Query
parsePrepareOrder = do 
  _ <- parseWord "prepare_order"
  _ <- parseChar '('
  orderid <- parseOrderID
  _ <- parseChar ')'
  return $ PrepareOrder orderid

-- <take_order> ::= "take_order" "(" <customer_name> "," <order_items> ")"
parseTakeOrder :: Parser Query
parseTakeOrder = do
  _ <- parseWord "take_order"
  _ <- parseChar '('
  name <- parseCustomerName
  _ <- parseChar ','
  items <- parseOrderItems
  _ <- parseChar ')'
  return $ TakeOrder name items

-- <task> ::= <take_order> | <prepare_order> | <serve_order> | <handle_payment> | <seat_customer>
parseTask :: Parser Query
parseTask = parseTakeOrder <|> parsePrepareOrder <|> parseServeOrder <|> parseHandlePayment <|> parseSeatCustomer <|> parseDebug

-- <task_list> ::= <task> | <task> ";" <task_list>
parseTaskList :: Parser [Query]
parseTaskList = do
  firstQuery <- parseTask
  rest <- optional (parseChar ';' >> parseTaskList)
  return $ case rest of
    Just otherQueries -> firstQuery : otherQueries
    Nothing -> [firstQuery]

-- <restaurant_operation> ::= <task_list>
parseRestaurantOperation :: Parser Query
parseRestaurantOperation = do
  queryList <- parseTaskList
  return $ RestaurantOperation queryList

parseDebug :: Parser Query
parseDebug = do 
  _ <- parseWord"Debug"
  return Debug

 --new for lib3
sat :: (Char -> Bool) -> Parser Char
sat p = do
  input <- lift get
  case input of
    [] -> throwError "Empty String"
    (x : xs) ->
      if p x
        then lift (put xs) >> return x
        else throwError $ "Could not recognize: " ++ [x]

char :: Char -> Parser Char
char c = sat (== c)

parseChar :: Char -> Parser Char
parseChar = char

parseLiteral :: String -> Parser String
parseLiteral [] = return []
parseLiteral (x:xs) = do
  _ <- parseChar x
  parseLiteral xs
  return (x:xs)