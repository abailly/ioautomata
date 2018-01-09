{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ViewPatterns               #-}
module ATMTest where

import           ATM
import           Control.Monad.Identity
import           Control.Monad.State    hiding (state)
import qualified Data.Map               as M
import           Data.Monoid            ((<>))
import           IOAutomaton            hiding (state)
import           IOAutomaton.Model      ()
import           Test.HUnit
import           Test.QuickCheck

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
testATMMachineValidator =
  "ATM validator" ~:
  TestList [ "check empty trace " ~: runAutomaton (T []) initAtm >>= assertEqual "incorrect state" (Nothing, initAtm)
           , runAutomaton (T [ (Init, EnterCard aCard, OK, EnteringPin )]) initAtm
             ~?= Just (Just OK,initAtm {state = EnteringPin, card = Just aCard})
           , runAutomaton (T [ (Init,EnterCard aCard,OK,EnteringPin)
                             , (EnteringPin,EnterPinCode (Pin "5678"),FailedCode,EnteringPin)
                             , (EnteringPin,EnterPinCode (Pin "1234"),OK,SelectingAction)
                             ]) initAtm
             ~?= Just (Just OK, initAtm { state = SelectingAction, card = Just (aCard {failedCode = 1})})
           ]
    where
      aCard = Card { pin = Pin "1234" , accountNo = Acc "123456", failedCode = 0 }

testATMStateT :: Test
testATMStateT = "ATM State transformer" ~:
                TestList [
                  "test valid sequence" ~: fmap (state . snd) (runStateT actions initAtm) ~?= Just SelectingAction
                , "test invalid sequence" ~: fmap (state . snd) (runStateT failureActions initAtm) ~?=  Just Sink
                , "test evalATM" ~:
                evalStateT actionsWithExit initAtm ~?=
                              Just [ (Init,EnterCard Card {pin = Pin "1234", accountNo = Acc "234567", failedCode = 1}, OK, EnteringPin)
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


newtype MockATM m a = MockATM { runMock :: StateT [ ATMInput ] m a }
  deriving (Functor, Applicative, Monad, MonadState [ATMInput])

instance (Monad m) => Interactive (MockATM m) ATMInput ATMOutput where
  request = do
    reqs <- get
    case reqs of
      (req:reqs') -> put reqs' >> return (Just req)
      []          -> return Nothing

  reply _ = pure ()


testATMMock :: Test
testATMMock = "Quickcheck MockClient" ~: quickCheck prop_mockATMClientRunsAgainstMockRunner

myBank'' :: Bank
myBank'' = Bank $ M.fromList [(Acc "123456",100),(Acc "234567",1000),(Acc "567890",12345)]

initAtm' :: AtmState
initAtm' = initAtm {bank = myBank'' }

newtype ValidATM = ValidATM (Valid AtmState ATMState ATMInput ATMOutput)
  deriving (Eq, Show)

instance Arbitrary ValidATM where
  arbitrary = ValidATM <$> validTraces initAtm'

prop_mockATMClientRunsAgainstMockRunner :: ValidATM -> Property
prop_mockATMClientRunsAgainstMockRunner (ValidATM (validTransitions -> trs)) =
    let res = (runIdentity . flip evalStateT (fmap ATM.input trs) . runMock) $ mockModel initAtm'
        msg = "result :" <> show res
    in  counterexample msg $ isSuccessful res
