{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ATM.ATM where

import           Control.Monad.State (StateT (..))
import qualified Data.Map            as M
import qualified IOAutomaton         as A

type CardRetained = String

type Note = (Int,Int)

data Card = Card { pin        :: Pincode,
                   accountNo  :: AccountNo,
                   failedCode :: Int }
          deriving (Eq, Show)

newtype Pincode = Pin String deriving (Eq, Show, Read)

newtype AccountNo = Acc String deriving (Eq, Show, Read, Ord)

data ATMState = Init
              | EnteringPin
              | SelectingAction
              | SelectingAmount
              | Sink
                deriving (Eq, Show, Ord)

data ATMInput = EnterCard Card
              | EnterPinCode Pincode
              | WithdrawMoney
              | EnterAmount Int
              | GetBalance
              | Exit
                deriving (Eq, Show)

data ATMOutput = FailedCode
               | CardRetained
               | SelectOperation
               | NotEnoughBalance
               | DeliverNotes
               | Bal Int
               | OK
               | Bye
                 deriving (Eq, Show)

data AtmState = Atm { state :: ATMState
                    , card  :: Maybe Card
                    , bank  :: Bank }
                deriving (Eq, Show)

newtype Bank = Bank (M.Map AccountNo Int) deriving (Eq, Show)

type Trans = (ATMState, ATMInput, ATMOutput, ATMState)

type Path  = [ Trans ]

type ATMMachine = A.Trace ATMState ATMInput ATMOutput

exit :: AtmState -> (Maybe ATMOutput, AtmState)
exit (Atm _ _ b) =  (Just Bye, Atm Init Nothing b)

enterCard :: Card -> AtmState -> (Maybe ATMOutput, AtmState)
enterCard c (Atm Init Nothing b) | failedCode c <= 2 = (Just OK, Atm  EnteringPin (Just c) b)
                                 | otherwise         = (Just CardRetained, Atm Init Nothing b)
enterCard _  s                                       = (Nothing, s { state = Sink} )

enterPin :: Pincode -> AtmState -> (Maybe ATMOutput, AtmState)
enterPin p (Atm EnteringPin (Just c@(Card p' _  _)) b) | p == p'   = (Just OK, Atm SelectingAction (Just c) b)
enterPin _ (Atm EnteringPin (Just c@(Card _  _  f)) b) | f < 2     = (Just FailedCode, Atm EnteringPin (Just c') b)
                                                       | otherwise = (Just CardRetained, Atm Init Nothing b)
                                                       where
                                                         c' = c { failedCode = failedCode c + 1}
enterPin _ s                                                       = (Nothing, s { state = Sink} )

withdrawMoney :: AtmState -> (Maybe ATMOutput, AtmState)
withdrawMoney (Atm SelectingAction c b) = (Just OK, Atm SelectingAmount c b)
withdrawMoney s                         = (Nothing, s { state = Sink} )

enterAmount :: Int -> AtmState -> (Maybe ATMOutput, AtmState)
enterAmount amount (Atm _ (Just c@(Card  _ accountN _)) b@(Bank m))
    | M.findWithDefault 0 accountN m >= amount  = (Just DeliverNotes, Atm SelectingAction (Just c) (Bank $ M.adjust (amount -) accountN m))
    | otherwise                                 = (Just NotEnoughBalance, Atm SelectingAction (Just c) b)
enterAmount  _ s                                = (Nothing, s {state = Sink} )

getBalance :: AtmState -> (Maybe ATMOutput, AtmState)
getBalance (Atm SelectingAction (Just c@(Card  _ accountN _)) b@(Bank m)) = (Bal `fmap` M.lookup accountN m, Atm SelectingAction (Just c) b)
getBalance s                                                              = (Nothing, s { state = Sink} )

initAtm :: AtmState
initAtm = Atm Init Nothing (Bank M.empty)

input :: Trans -> ATMInput
input (_,i,_,_) = i

output :: Trans -> ATMOutput
output (_,_,o,_) = o

action :: ATMInput -> AtmState -> (Maybe ATMOutput, AtmState)
action (EnterCard c)    = enterCard c
action (EnterPinCode p) = enterPin p
action (EnterAmount m)  = enterAmount m
action WithdrawMoney    = withdrawMoney
action GetBalance       = getBalance
action Exit             = exit

instance A.IOAutomaton AtmState ATMState ATMInput ATMOutput where
  init                = initAtm
  sink    _           = Sink
  input               = input
  output              = output
  action              = action
  state  (Atm s _ _)  = s
  update a q          = a { state = q }

type StateOfAtm m = StateT AtmState m (Maybe ATMOutput)

class (Monad m) => ATM m a where
    startATM'      :: m a
    exit'          :: m a
    enterCard'     :: Card -> m a
    enterPin'      :: Pincode -> m a
    withdrawMoney' :: m a
    enterAmount'   :: Int -> m a
    getBalance'    :: m a

instance (Monad m) => ATM (StateT AtmState m) (Maybe ATMOutput) where
    startATM'        = StateT (\ s -> return (Just OK, s))
    exit'            = StateT (return . exit)
    enterPin'      p = StateT (return . enterPin p)
    withdrawMoney'   = StateT (return . withdrawMoney)
    enterAmount'   a = StateT (return . enterAmount a)
    getBalance'      = StateT (return . getBalance)
    enterCard'     c = StateT (return . enterCard c)


evalATM :: (Monad m) => Trans -> StateT AtmState m Trans
evalATM t = StateT (return . A.eval t)

isValidPath :: [Trans] -> Bool
isValidPath []               = True
isValidPath ((_,_,_,Sink):_) = False
isValidPath (_:xs)           = isValidPath xs
