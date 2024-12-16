{-# LANGUAGE DeriveFunctor #-}
module Main (main) where

import Control.Lens
import Control.Monad.Free (Free (..), liftF)
import Data.ByteString
import Data.String.Conversions
import qualified Lib3
import Network.Wreq
import qualified Lib2

data MyDomainAlgebra next
  = TakeOrder String [(String, Integer)] (() -> next)
    | PrepareOrder Integer (() -> next)
    | ServeOrder Integer (() -> next)
    | HandlePayment Integer Lib2.PaymentMethod (Integer, Integer) (() -> next)
    | SeatCustomer String Lib2.Seat (() -> next)
    | Debug (String -> next)
    | Save (() -> next)
    | Load (() -> next)
    deriving (Functor)

type RestaurantProgram = Free MyDomainAlgebra

takeOrder ::  String -> [(String, Integer)] -> RestaurantProgram ()
takeOrder name items = liftF $ TakeOrder name items id

prepareOrder :: Integer -> RestaurantProgram ()
prepareOrder idd = liftF $ PrepareOrder idd id

serveOrder :: Integer -> RestaurantProgram ()
serveOrder idd = liftF $ ServeOrder idd id

handlePayment :: Integer -> Lib2.PaymentMethod -> (Integer, Integer) -> RestaurantProgram ()
handlePayment idd method num = liftF $ HandlePayment idd method num id

seatCustomer :: String -> Lib2.Seat -> RestaurantProgram ()
seatCustomer name seat = liftF $ SeatCustomer name seat id

debug :: RestaurantProgram String
debug = liftF $ Debug id 

save :: RestaurantProgram ()
save = liftF $ Save id

load :: RestaurantProgram ()
load = liftF $ Load id

interpretSingle :: RestaurantProgram a -> IO a
interpretSingle (Pure a) = return a
interpretSingle (Free step) = do
  next <- runStepSingle step
  interpretSingle next
  where
    runStepSingle :: MyDomainAlgebra a -> IO a
    runStepSingle (TakeOrder name items next) = sendSingleStatement (Lib2.TakeOrder name items) >> return (next ())
    runStepSingle (PrepareOrder id next) = sendSingleStatement (Lib2.PrepareOrder id) >> return (next ())
    runStepSingle (ServeOrder id next) = sendSingleStatement (Lib2.ServeOrder id) >> return (next ())
    runStepSingle (HandlePayment id method num next) = sendSingleStatement (Lib2.HandlePayment id method num) >> return (next ())
    runStepSingle (SeatCustomer name seat next) = sendSingleStatement (Lib2.SeatCustomer name seat) >> return (next ())
    runStepSingle (Debug next) = do
      str <- sendSingleStatement Lib2.Debug
      return $ next str
    runStepSingle (Save next) = postAsString "save" >> return (next ())
    runStepSingle (Load next) = postAsString "load" >> return (next ())

sendSingleStatement :: Lib2.Query -> IO String
sendSingleStatement = postAsString . Lib3.renderStatements . Lib3.Single

interpretBatch :: RestaurantProgram a -> IO a
interpretBatch prog = interpretBatch' prog []

interpretBatch' :: RestaurantProgram a -> [Lib2.Query] -> IO a
interpretBatch' (Pure a) batch = dumpBatch batch >> return a
interpretBatch' (Free step) batch = do
  case step of
    TakeOrder name items next -> interpretBatch' (next ()) (batch ++ [Lib2.TakeOrder name items])
    PrepareOrder id next -> interpretBatch' (next ()) (batch ++ [Lib2.PrepareOrder id])
    ServeOrder id next -> interpretBatch' (next ()) (batch ++ [Lib2.ServeOrder id])
    HandlePayment id method num next -> interpretBatch' (next ()) (batch ++ [Lib2.HandlePayment id method num])
    SeatCustomer name seat next -> interpretBatch' (next ()) (batch ++ [Lib2.SeatCustomer name seat])
    Debug next -> do
      _ <- dumpBatch batch
      str <- sendSingleStatement Lib2.Debug
      interpretBatch' (next str) []
    Save next -> dumpBatch batch >> postAsString "save" >> interpretBatch' (next ()) []
    Load next -> dumpBatch batch >> postAsString "load" >> interpretBatch' (next ()) []

dumpBatch :: [Lib2.Query] -> IO (Maybe String)
dumpBatch [] = return Nothing
dumpBatch [single] = Just <$> sendSingleStatement single
dumpBatch batch = Just <$> sendAsBatch batch

sendAsBatch :: [Lib2.Query] -> IO String
sendAsBatch = postAsString . Lib3.renderStatements . Lib3.Batch

postAsString :: String -> IO String
postAsString s = do
  let rawRequest = cs s :: ByteString
  putStrLn $ "Sending request:\n" ++ cs rawRequest
  resp <- post "http://localhost:3000" rawRequest
  return $ cs $ resp ^. responseBody

program :: RestaurantProgram (String, String)
program = do
    -- takeOrder "Sam" [("soup",2)]
    -- prepareOrder 1
    -- serveOrder 1
    -- handlePayment 1 Lib2.Cash (20,0)
    -- debug
    save
    -- load
    -- debug
    return ("Books and operations executed", "Success")

main :: IO ()
main = do
     _ <- interpretSingle program
     return()