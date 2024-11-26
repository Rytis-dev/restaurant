{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE LambdaCase #-}

module Lib2
    ( Query(..),
      Parser(..),
      PaymentMethod(..),
      Seat(..),
      parseQuery,
      State(..),
      emptyState,
      stateTransition,
      parseLiteral,
      parseTask,
    ) where

import qualified Data.Char as C
import Control.Applicative (Alternative (empty), optional, (<|>))

newtype Parser a = P {parse :: String -> Either String (a, String)}

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
parseQuery input = case parse parseTaskList input of
  --Right (query, _) -> Right query
  Right (qs, r) ->
      if null r
        then case qs of
          [q] -> Right q
          _ -> Right (RestaurantOperation qs)
        else Left ("Unrecognized characters: " ++ r)
  _ -> Left "Failed to parse query: Unknown command"
  
data PaymentMethod = Cash | Card | MobilePayment
   deriving (Eq, Show)

data Seat = Bar | Outdoor | DiningTable
  deriving (Eq, Show)

data State = State
  { orderList :: [(String, [(String, Integer)])], -- customer name and their items
    kitchenList :: [(Integer, String)], -- order ID and status
    revenue :: Integer, -- total revenue
    nextOrderId :: Integer -- next order ID to be assigned
  }
  deriving (Eq, Show)

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = do
    a <- p
    return $ f a

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = P $ \str -> Right (x, str)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = do
    f <- pf
    f <$> pa

instance Alternative Parser where
  empty :: Parser a
  empty = P $ \_ -> Left "Failed to parse"
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) p1 p2 = P $ \str -> case parse p1 str of
    Right (v, r) -> Right (v, r)
    Left _ -> parse p2 str

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) pa f = P $ \str -> case parse pa str of
    Left e -> Left e
    Right (a, r) -> parse (f a) r

emptyState :: State
emptyState = State
  { orderList = [],
    kitchenList = [],
    revenue = 0,
    nextOrderId = 1
  }

stateTransition :: State -> Query -> Either String (Maybe String, State)
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

  SeatCustomer customerName seat -> 
    let msg = "Seated " ++ customerName ++ " at " ++ show seat ++ "."
    in Right (Just msg, state)

  Debug -> 
    let msg = show state
    in Right (Just msg, state)


skipSpaces :: String -> String
skipSpaces = dropWhile (== ' ')

parseChar :: Char -> Parser Char
parseChar c = P $ \input ->
    case input of
        [] ->  Left ("Cannot find " ++ [c] ++ " in an empty input")
        s@(h:t) -> if c == h then Right (c, t) else Left (c : " is not found in " ++ s)

parseWord :: String -> Parser String
parseWord [] = P $ \input -> Right ([], input)
parseWord (w:ws) = P $ \input ->
    case parse (parseChar w) input of
        Right (_, rest) -> 
            case parse (parseWord ws) rest of 
                Right (matched, finalRest) -> Right (w : matched, finalRest)
                Left err -> Left err
        Left _ -> Left ("Input does not match: expected '" ++ (w:ws) ++ "', but found '" ++ input ++ "'")

-- <number> ::= "0" | "1" | "2" | ... | "9" 
parseNumber :: Parser Integer
parseNumber = P $ \input ->
  let (digits, rest) = span C.isDigit (skipSpaces input)
   in if null digits
        then Left "Not a number"
        else Right (read digits, rest)

-- <letter> ::= "a" | "b" | "c" | ... | "z" | "A" | "B" | ... | "Z"
parseLetter :: Parser Char
parseLetter = P $ \input ->
    case input of
      [] -> Left "Cannot find any letter in an empty input"
      s@(h:t) -> if C.isLetter h then Right (h, t) else Left (s ++ " does not start with a letter")

-- <string> ::= <letter> | <letter> <string>
parseString :: Parser String
parseString = P $ \input ->
    case parse parseLetter input of
        Right (c, rest) ->
            case parse parseString rest of
                Right (cs, remaining) -> Right (c:cs, remaining)
                Left _ -> Right ([c], rest)  -- Stop after first letter if no more letters can be parsed
        Left err -> Left err 

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
sat p = P $ \case
  [] -> Left "Empty String"
  s@(x : xs) -> if p x then Right (x, xs) else Left $ "Could not recognize: " ++ s

char :: Char -> Parser Char
char c = sat (== c)

parseChar' :: Char -> Parser Char
parseChar' = char

parseLiteral :: String -> Parser String
parseLiteral [] = return []
parseLiteral (x : xs) = do
  _ <- parseChar' x
  parseLiteral xs