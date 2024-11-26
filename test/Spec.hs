{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC
    ( Gen,
      Arbitrary(arbitrary),
      elements,
      oneof,
      listOf, testProperty, listOf1, suchThat)

import Lib1 qualified
import Lib2 qualified
import Lib3 qualified
import Control.Concurrent (Chan)

instance Arbitrary Lib2.PaymentMethod where
    arbitrary :: Gen Lib2.PaymentMethod
    arbitrary = elements [Lib2.Cash, Lib2.Card, Lib2.MobilePayment]

instance Arbitrary Lib2.Seat where
    arbitrary :: Gen Lib2.Seat
    arbitrary = elements [Lib2.Bar, Lib2.Outdoor, Lib2.DiningTable]

onlyLetters :: Gen Char
onlyLetters = elements ['a'..'z']

positiveInt :: Gen Integer
positiveInt = arbitrary `suchThat` (> 0) 

goodItems :: Gen [(String, Integer)]
goodItems = listOf1 $ (,) <$> listOf1 onlyLetters <*> positiveInt

positiveTuple :: Gen (Integer, Integer)
positiveTuple = do
  wholePart <- positiveInt
  fractionalPart <- positiveInt
  return (wholePart, fractionalPart)

instance Arbitrary Lib2.Query where
  arbitrary =
    oneof
      [ Lib2.TakeOrder <$> listOf1 onlyLetters <*> goodItems,
        Lib2.PrepareOrder <$> positiveInt,
        Lib2.ServeOrder <$> positiveInt,
        Lib2.HandlePayment <$> positiveInt <*> arbitrary <*> positiveTuple,
        Lib2.SeatCustomer <$> listOf1 onlyLetters <*> arbitrary,
        pure Lib2.Debug
      ]

instance Arbitrary Lib3.Statements where
    arbitrary :: Gen Lib3.Statements
    arbitrary = oneof [Lib3.Single <$> arbitrary, Lib3.Batch <$> listOf arbitrary]

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
          expectedKitchenList = [(1,"Order prepared")]
      in Lib2.stateTransition stateAfterOrder prepareQuery @?= Right (Just "Order 1 is now prepared.", stateAfterOrder { Lib2.kitchenList = expectedKitchenList }),
    testCase "Transition with ServeOrder" $
      let initialState = Lib2.emptyState
          order = Lib2.TakeOrder "John" [("banana", 4)]
          (Right (_, stateAfterOrder)) = Lib2.stateTransition initialState order
          serveQuery = Lib2.ServeOrder 1
          expectedKitchenList = [(1,"Order served")]
      in Lib2.stateTransition stateAfterOrder serveQuery @?= Right (Just "Order 1 served successfully.", stateAfterOrder { Lib2.kitchenList = expectedKitchenList }),
    testCase "Transition with HandlePayment" $
      let initialState = Lib2.emptyState
          order = Lib2.TakeOrder "John" [("banana", 4)]
          (Right (_, stateAfterOrder)) = Lib2.stateTransition initialState order
          paymentQuery = Lib2.HandlePayment 1 Lib2.Cash (20, 0)
          expectedKitchenList = [(1,"Done")]
      in Lib2.stateTransition stateAfterOrder paymentQuery @?= Right (Just "Payment of 20 processed for order 1.", stateAfterOrder { Lib2.revenue = 20, Lib2.kitchenList = expectedKitchenList}),
    testCase "Transition with SeatCustomer" $
      let initialState = Lib2.emptyState
          seatQuery = Lib2.SeatCustomer "Mary" Lib2.Outdoor
      in Lib2.stateTransition initialState seatQuery @?= Right (Just "Seated Mary at Outdoor.", initialState)
  ]
propertyTests :: TestTree
propertyTests =
  testGroup "Lib3 tests"

    [ testCase "Test single" $
        let s = Lib3.Single (Lib2.SeatCustomer "Mary" Lib2.Outdoor) 
         in Lib3.parseStatements (Lib3.renderStatements s) @?= Right (s, ""),

      testCase "Test batch" $
        let s1 = Lib2.TakeOrder "Sam" [("soup", 2)]
            s2 = Lib2.PrepareOrder 1
            s3 = Lib2.ServeOrder 1
            b = Lib3.Batch [s1, s2, s3]
         in Lib3.parseStatements (Lib3.renderStatements b) @?= Right (b, ""),

      QC.testProperty "rendered and parsed" $
        \s -> Lib3.parseStatements (Lib3.renderStatements s) == Right (s, "")
    ]

