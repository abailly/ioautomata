module ATMTest where

import           ATM
import           Control.Monad.State hiding (state)
import qualified Data.Map            as M
import           IOAutomaton         hiding (state)
import           Test.HUnit

atmTransitionsUpdateState :: Test
atmTransitionsUpdateState =
  TestList [ snd  (enterCard myCard initAtm)                                    ~?= Atm EnteringPin (Just myCard) (Bank M.empty)
           , fst  (enterCard myCard waitingForPin)                              ~?= Nothing
           , enterCard myCard''  initAtm                                        ~?= (Just CardRetained, Atm Init Nothing (bank initAtm))
           , enterPin (Pin "1234")  waitingForPin                               ~?= (Just OK, Atm SelectingAction (Just myCard) myBank)
           , enterPin (Pin "2345")  waitingForPin                               ~?= (Just FailedCode, Atm EnteringPin (Just myCard')  myBank)
           , enterPin (Pin "2345") (waitingForPin    {card = Just myCard''})    ~?= (Just CardRetained, Atm Init Nothing myBank)
           , enterPin (Pin "2345") (waitingForPin    {state = Init})            ~?= (Nothing, Atm Sink (Just myCard) myBank)
           , withdrawMoney         (waitingForPin    {state = SelectingAction}) ~?= (Just OK, Atm SelectingAmount (Just myCard) myBank)
           , withdrawMoney         (waitingForPin    {state = Init})            ~?= (Nothing, Atm Sink (Just myCard) myBank)
           , enterAmount 100        waitingForAmount                            ~?= (Just DeliverNotes, Atm SelectingAction (Just myCard) myBank')
           , enterAmount 100       (waitingForAction {bank = myBank'})          ~?= (Just NotEnoughBalance, Atm SelectingAction (Just myCard) myBank')
           , getBalance            (waitingForAction {bank = myBank'})          ~?= (Just$  Bal 0, Atm SelectingAction (Just myCard) myBank')
           , getBalance             waitingForAction                            ~?= (Just$  Bal 100, Atm SelectingAction (Just myCard) myBank)
           , getBalance            (waitingForAmount {card = Just otherCard})   ~?= (Nothing, Atm Sink (Just otherCard) myBank)
           , exit                   waitingForAmount                            ~?= (Just Bye, Atm Init Nothing myBank)
           ]
    where
      myCard            = Card { pin = Pin "1234" , accountNo = Acc "123456", failedCode = 0 }
      otherCard         = Card { pin = Pin "1234" , accountNo = Acc "234567", failedCode = 0 }
      myCard'           = Card { pin = Pin "1234" , accountNo = Acc "123456", failedCode = 1 }
      myCard''          = Card { pin = Pin "1234" , accountNo = Acc "123456", failedCode = 3 }
      waitingForPin     = Atm EnteringPin (Just myCard) myBank
      waitingForAmount  = waitingForPin { state = SelectingAmount }
      waitingForAction  = waitingForPin { state = SelectingAction }
      myBank            = Bank $ M.singleton (Acc "123456") 100
      myBank'           = Bank $ M.singleton (Acc "123456") 0

testATMMachineValidator :: Test
testATMMachineValidator = "ATM validator" ~: TestList [
                           "check empty trace " ~: runAutomaton (T []) initAtm >>= assertEqual "incorrect state" (Nothing, initAtm)
                          ,runAutomaton (T [ (Init, EnterCard aCard, OK, EnteringPin )]) initAtm ~?= Just (Just OK,initAtm {state = EnteringPin, card = Just aCard})
                          ,runAutomaton (T [(Init,EnterCard aCard,OK,EnteringPin),(EnteringPin,EnterPinCode (Pin "5678"),FailedCode,EnteringPin),(EnteringPin,EnterPinCode (Pin "1234"),OK,SelectingAction)]) initAtm ~?= Just (Just OK, initAtm { state = SelectingAction, card = Just (aCard {failedCode = 1})})
                          ]
    where
      aCard = Card { pin = Pin "1234" , accountNo = Acc "123456", failedCode = 0 }

testATMStateT :: Test
testATMStateT = "ATM State transformer" ~:
                TestList [
                  "test valid sequence" ~: runStateT actions initAtm >>=
                                            assertEqual "state is SelectingAction" SelectingAction . state . snd
                , "test invalid sequence" ~: runStateT failureActions initAtm >>=
                                              assertEqual "state is Sink" Sink . state . snd
                , "test evalATM" ~:
                evalState actionsWithExit initAtm ~?=
                              [ (Init,EnterCard Card {pin = Pin "1234", accountNo = Acc "234567", failedCode = 1}, OK, EnteringPin)
                              , (EnteringPin,Exit,Bye,Init)
                              , (Init,EnterCard Card {pin = Pin "2345", accountNo = Acc "123456", failedCode = 2}, OK, EnteringPin)
                              ]
                ]
                     where
                       actionsWithExit = mapM evalATM [ (Init,EnterCard Card {pin = Pin "1234", accountNo = Acc "234567", failedCode = 1},OK,EnteringPin)
                                                      , (EnteringPin,Exit,OK,Init)
                                                      , (Init,EnterCard Card {pin = Pin "2345", accountNo = Acc "123456", failedCode = 2},OK,EnteringPin)
                                                      ]
                       actions = mapM evalST [ (Init,EnterCard aCard,OK,EnteringPin)
                                                 , (EnteringPin,EnterPinCode (Pin "5678"),FailedCode,EnteringPin)
                                                 , (EnteringPin,EnterPinCode (Pin "1234"),OK,SelectingAction)
                                                 ]
                       failureActions = mapM evalST [ (Init,EnterCard aCard,OK,EnteringPin)
                                                        , (EnteringPin,EnterPinCode (Pin "5678"),FailedCode,EnteringPin)
                                                        , (EnteringPin,GetBalance,FailedCode,Sink)
                                                        ]
                       aCard = Card { pin = Pin "1234" , accountNo = Acc "123456", failedCode = 0 }
