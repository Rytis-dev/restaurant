{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Redundant lambda" #-}

module Lib2
    ( Query(..),
      PaymentMethod(..),
      Seat(..),
      parseQuery,
      State(..),
      emptyState,
      stateTransition
    ) where

import qualified Data.Char as C
import qualified Data.List as L

type Parser a = String -> Either String (a, String)

data Query
    = TakeOrder String [(String, Integer)]
    | PrepareOrder Integer
    | ServeOrder Integer
    | HandlePayment Integer PaymentMethod (Integer, Integer)
    | SeatCustomer String Seat
    | Debug 
  deriving(Eq, Show)


-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery input = case or6 parseTakeOrder parsePrepareOrder parseServeOrder parseHandlePayment parseSeatCustomer parseDebug input of
  Right (query, _) -> Right query
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
        let msg = "Order " ++ show orderId ++ " is prepared!"
        in Right ( Just msg, state)
      Nothing -> Left "Order not found"

  ServeOrder orderId ->
    case lookup orderId (kitchenList state) of
      Just status ->
        let updatedKitchenList = filter (\(id, _) -> id /= orderId) (kitchenList state)
            msg = "Order " ++ show orderId ++ " served successfully."
        in Right (Just msg, state { kitchenList = updatedKitchenList })
      Nothing -> Left "Order not found"

  HandlePayment orderId method amount ->
    let updatedKitchenList = filter (\(id, _) -> id /= orderId) (kitchenList state)
        newRevenue = revenue state + 20
        msg = "Payment of " ++ "20" ++ " processed for order " ++ show orderId ++ "."
    in Right (Just msg, state { kitchenList = updatedKitchenList, revenue = newRevenue })

  SeatCustomer customerName seat -> 
    let msg = "Seated " ++ customerName ++ " at " ++ show seat ++ "."
    in Right (Just msg, state)

  Debug -> 
    let msg = show state
    in Right (Just msg, state)


-- helpers
or2 :: Parser a -> Parser a -> Parser a
or2 a b = \input ->
    case a input of
        Right r1 -> Right r1
        Left e1 -> case b input of
                Right r2 -> Right r2
                Left e2 -> Left (e1 ++ ", " ++ e2)

or2' :: (a -> c) -> (b -> c) -> Parser a -> Parser b -> Parser c
or2' fa fb a b = \input ->
    case a input of
        Right (r1, rest1) -> Right(fa r1, rest1)
        Left e1 -> case b input of
                Right (r2, rest2) -> Right(fb r2, rest2)
                Left e2 -> Left (e1 ++ ", " ++ e2)

or5 :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or5 a b c d e = \input ->
  case a input of
    Right r1 -> Right r1
    Left e1 -> case b input of
      Right r2 -> Right r2
      Left e2 -> case c input of
        Right r3 -> Right r3
        Left e3 -> case d input of
          Right r4 -> Right r4
          Left e4 -> case e input of
            Right r5 -> Right r5
            Left e5 -> Left (e1 ++ "; " ++ e2 ++ "; " ++ e3 ++ "; " ++ e4 ++ "; " ++ e5)

or6 :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or6 a b c d e f = \input ->
  case a input of
    Right r1 -> Right r1
    Left e1 -> case b input of
      Right r2 -> Right r2
      Left e2 -> case c input of
        Right r3 -> Right r3
        Left e3 -> case d input of
          Right r4 -> Right r4
          Left e4 -> case e input of
            Right r5 -> Right r5
            Left e5 -> case f input of
              Right r6 -> Right r6
              Left e6 -> Left (e1 ++ "; " ++ e2 ++ "; " ++ e3 ++ "; " ++ e4 ++ "; " ++ e5 ++ "; " ++ e6)

and2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2 f a b input =
  case a input of
    Right (v1, r1) ->
      case b r1 of
        Right (v2, r2) -> Right (f v1 v2, r2)
        Left e2 -> Left e2
    Left e1 -> Left e1

and3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3 f a b c input =
  case a input of
    Right (v1, r1) ->
      case b r1 of
        Right (v2, r2) ->
          case c r2 of
            Right (v3, r3) -> Right (f v1 v2 v3, r3)
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1

and4 :: (a -> b -> c -> d -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser f
and4 x a b c d input =
  case a input of
    Right (v1, r1) ->
      case b r1 of
        Right (v2, r2) ->
          case c r2 of
            Right (v3, r3) -> 
              case d r3 of
                Right (v4,r4) -> Right (x v1 v2 v3 v4, r3)
                Left e4 -> Left e4
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1

and6 :: (a -> b -> c -> d -> e -> f -> g) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g
and6 x a b c d e f input =
  case a input of
    Right (v1, r1) ->
      case b r1 of
        Right (v2, r2) ->
          case c r2 of
            Right (v3, r3) ->
              case d r3 of
                Right (v4, r4) ->
                  case e r4 of
                    Right (v5, r5) ->
                      case f r5 of
                        Right (v6, r6) -> Right (x v1 v2 v3 v4 v5 v6, r6)
                        Left e6 -> Left e6
                    Left e5 -> Left e5
                Left e4 -> Left e4
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1

and8 ::(a -> b -> c -> d -> e -> f -> g -> h -> i) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e ->
  Parser f -> Parser g -> Parser h -> Parser i
and8 x a b c d e f g h input =
  case a input of
    Right (v1, r1) ->
      case b r1 of
        Right (v2, r2) ->
          case c r2 of
            Right (v3, r3) ->
              case d r3 of
                Right (v4, r4) ->
                  case e r4 of
                    Right (v5, r5) ->
                      case f r5 of
                        Right (v6, r6) ->
                          case g r6 of
                            Right (v7, r7) ->
                              case h r7 of
                                Right (v8, r8) ->
                                  Right (x v1 v2 v3 v4 v5 v6 v7 v8, r8)
                                Left e8 -> Left e8
                            Left e7 -> Left e7
                        Left e6 -> Left e6
                    Left e5 -> Left e5
                Left e4 -> Left e4
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1

parseChar :: Char -> Parser Char
parseChar c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
parseChar c s@(h : t) = if c == h then Right (c, t) else Left (c : " is not found in " ++ s)

parseWord :: String -> Parser String
parseWord [] = \input -> Right ([], input)
parseWord (w:ws) = \input -> 
    case parseChar w input of
        Right (_, rest) -> 
            case parseWord ws rest of 
                Right (matched, finalRest) -> Right (w : matched, finalRest)
                Left err -> Left err
        _ -> Left ("Input does not match: expected '" ++ (w:ws) ++ "', but found '" ++ input ++ "'")


-- <number> ::= "0" | "1" | "2" | ... | "9" 
parseNumber :: Parser Integer
parseNumber [] = Left "empty input, cannot parse a number"
parseNumber str =
    let
        digits = L.takeWhile C.isDigit str
        rest = drop (length digits) str
    in
        case digits of
            [] -> Left "not a number"
            _ -> Right (read digits, rest)

-- <letter> ::= "a" | "b" | "c" | ... | "z" | "A" | "B" | ... | "Z"
parseLetter :: Parser Char
parseLetter [] = Left "Cannot find any letter in an empty input"
parseLetter s@(h:t) = if C.isLetter h then Right (h, t) else Left (s ++ " does not start with a letter")

-- <string> ::= <letter> | <letter> <string>
parseString :: Parser String
parseString [] = Left "Cannot parse an empty input a word"
parseString input = case parseLetter input of
    Right (c, r1) -> case parseString r1 of
        Right (v2, r2) -> Right (c:v2, r2)
        Left _ -> Right ([c], r1)
    Left err -> Left err 

-- <quantity> ::= <number>
parseQuantity :: Parser Integer
parseQuantity input = parseNumber input

-- <item_name> ::= <string>
parseItemName :: Parser String
parseItemName input = parseString input

-- <amount> ::= <number> "." <number>
parseAmount :: Parser (Integer, Integer)
parseAmount = and3 (\a _ b -> (a, b)) parseNumber (parseChar '.') parseNumber

-- <payment_method> ::= "Cash" | "Card" | "MobilePayment"
parsePaymentMethod :: Parser PaymentMethod
parsePaymentMethod input = case parseString input of
    Right ("Cash", rest) -> Right (Cash, rest)
    Right ("Card", rest) -> Right (Card, rest)
    Right ("MobilePayment", rest) -> Right (MobilePayment, rest)
    Left err -> Left err
    _ -> Left "Failed to parse PaymentMethod"

-- <order_id> ::= <number>
parseOrderID :: Parser Integer
parseOrderID input = parseNumber input

-- <item> ::= <item_name> "," <quantity>
parseItem :: Parser (String, Integer)
parseItem = and3 (\a _ b -> (a, b)) parseItemName (parseChar ',') parseQuantity

-- <order_items> ::= <item> | <item> "," <order_items>
parseOrderItems :: Parser [(String, Integer)]
parseOrderItems = or2' (\a -> a:[]) id parseItem (and3 (\c _ d -> (c : d)) parseItem (parseChar ',') parseOrderItems)

-- <customer_name> ::= <string>
parseCustomerName :: Parser String
parseCustomerName input = parseString input

-- <seat> ::= "Bar" | "Outdoor" | "DiningTable"
parseSeat :: Parser Seat
parseSeat input = case parseString input of
    Right ("Bar", rest) -> Right (Bar, rest)
    Right ("Outdoor", rest) -> Right (Outdoor, rest)
    Right ("DiningTable", rest) -> Right (DiningTable, rest)
    Left err -> Left err
    _ -> Left "Failed to parse Seat"

-- <seat_customer> ::= "seat_customer" "(" <customer_name> "," <seat> ")"
parseSeatCustomer :: Parser Query
parseSeatCustomer = and6 (\_ _ a _ b _-> SeatCustomer a b) (parseWord "seat_customer") (parseChar '(') parseCustomerName (parseChar ',') parseSeat (parseChar ')')

-- <handle_payment> ::= "handle_payment" "(" <order_id> "," <payment_method> "," <amount> ")"
parseHandlePayment :: Parser Query
parseHandlePayment = and8 (\_ _ a _ b _ c _-> HandlePayment a b c) (parseWord "handle_payment") (parseChar '(') parseOrderID (parseChar ',') parsePaymentMethod (parseChar ',') parseAmount (parseChar ')')

-- <serve_order> ::= "serve_order" "(" <order_id> ")"
parseServeOrder :: Parser Query
parseServeOrder = and4 (\_ _ a _ -> ServeOrder a) (parseWord "serve_order") (parseChar '(') parseOrderID (parseChar ')')

-- <prepare_order> ::= "prepare_order" "(" <order_id> ")"
parsePrepareOrder :: Parser Query
parsePrepareOrder = and4 (\_ _ a _ -> PrepareOrder a) (parseWord "prepare_order") (parseChar '(') parseOrderID (parseChar ')')

-- <take_order> ::= "take_order" "(" <customer_name> "," <order_items> ")"
parseTakeOrder :: Parser Query
parseTakeOrder = and6 (\_ _ a _ b _-> TakeOrder a b) (parseWord "take_order") (parseChar '(') parseCustomerName (parseChar ',') parseOrderItems (parseChar ')')

-- <task> ::= <take_order> | <prepare_order> | <serve_order> | <handle_payment> | <seat_customer>
parseTask :: Parser Query
parseTask = or5 parseTakeOrder parsePrepareOrder parseServeOrder parseHandlePayment parseSeatCustomer

-- <task_list> ::= <task> | <task> ";" <task_list>
parseTaskList :: Parser [Query]
parseTaskList = or2' (\a -> a:[]) id parseTask (and3 (\c _ d -> (c : d)) parseTask (parseChar ';') parseTaskList)

-- <restaurant_operation> ::= <task_list>
parseRestaurantOperation :: Parser [Query]
parseRestaurantOperation = parseTaskList

parseDebug :: Parser Query
parseDebug input = case parseString input of
    Right ("Debug", rest) -> Right (Debug, rest)
    Left err -> Left err
    _ -> Left "Failed to parse "