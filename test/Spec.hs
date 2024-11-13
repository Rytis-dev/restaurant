{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

import Lib1 qualified
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib1 tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False,
    testCase "Parsing TakeOrder" $
      Lib2.parseQuery "take_order(John,banana,4)" @?= Right (Lib2.TakeOrder "John" [("banana",4)]),
    testCase "Parsing PrepareOrder" $
      Lib2.parseQuery "prepare_order(1)" @?= Right (Lib2.PrepareOrder 1),
    testCase "Parsing ServeOrder" $
      Lib2.parseQuery "serve_order(2)" @?= Right (Lib2.ServeOrder 2),
    testCase "Parsing HandlePayment" $
      Lib2.parseQuery "handle_payment(1,Cash,10.50)" @?= Right (Lib2.HandlePayment 1 Lib2.Cash (10, 50)),
    testCase "Parsing SeatCustomer" $
      Lib2.parseQuery "seat_customer(Mary,Outdoor)" @?= Right (Lib2.SeatCustomer "Mary" Lib2.Outdoor),
    testCase "Transition with TakeOrder" $
      let initialState = Lib2.emptyState
          query = Lib2.TakeOrder "John" [("banana", 4)]
          expectedState = initialState
            { Lib2.orderList = [("John", [("banana", 4)])],
              Lib2.kitchenList = [(1, "Order placed")],
              Lib2.nextOrderId = 2
            }
      in Lib2.stateTransition initialState query @?= Right (Just "Order taken for John. Your order ID is 1.", expectedState),
    testCase "Transition with PrepareOrder" $
      let initialState = Lib2.emptyState
          order = Lib2.TakeOrder "John" [("banana", 4)]
          (Right (_, stateAfterOrder)) = Lib2.stateTransition initialState order
          prepareQuery = Lib2.PrepareOrder 1
      in Lib2.stateTransition stateAfterOrder prepareQuery @?= Right (Just "Order 1 is prepared!", stateAfterOrder),
    testCase "Transition with ServeOrder" $
      let initialState = Lib2.emptyState
          order = Lib2.TakeOrder "John" [("banana", 4)]
          (Right (_, stateAfterOrder)) = Lib2.stateTransition initialState order
          serveQuery = Lib2.ServeOrder 1
          expectedKitchenList = [] -- The order should be removed from the kitchen list
      in Lib2.stateTransition stateAfterOrder serveQuery @?= Right (Just "Order 1 served successfully.", stateAfterOrder { Lib2.kitchenList = expectedKitchenList }),
    testCase "Transition with HandlePayment" $
      let initialState = Lib2.emptyState
          order = Lib2.TakeOrder "John" [("banana", 4)]
          (Right (_, stateAfterOrder)) = Lib2.stateTransition initialState order
          paymentQuery = Lib2.HandlePayment 1 Lib2.Cash (20, 0)
      in Lib2.stateTransition stateAfterOrder paymentQuery @?= Right (Just "Payment of 20 processed for order 1.", stateAfterOrder { Lib2.revenue = 20, Lib2.kitchenList = []}),
    testCase "Transition with SeatCustomer" $
      let initialState = Lib2.emptyState
          seatQuery = Lib2.SeatCustomer "Mary" Lib2.Outdoor
      in Lib2.stateTransition initialState seatQuery @?= Right (Just "Seated Mary at Outdoor.", initialState)
  ]