# Testing ATM simulator

## The System

We are simulating a *Cash Dispenser* whose interface is command-line
based. The application is very simple, here is a transcript of some
session:

```
nono@oqube:~/projets/beyond-tdd/java/atm/dab-app$ java -cp .:target/dab-app-1.2-executable.jar oqube.dab.app.Main -e
Language is set to English
Lauching CLI application...
log4j:WARN No appenders could be found for logger (org.springframework.context.support.ClassPathXmlApplicationContext).
log4j:WARN Please initialize the log4j system properly.
Insert your card
> card

Enter pincode
> 1234
Pincode OK
Select your operation:
1 - Account's balance
2 - Withdraw money
0 - Exit

> 1
Your account balance is 100
> 2

Enter amount requested
> 50
delivering: 10 -> 0, 20 -> 0, 50 -> 1, 100 -> 0
Retrieve money from dispenser
Select your operation:
1 - Account's balance
2 - Withdraw money
0 - Exit

> 1
Your account balance is 50
> 0

Insert your card
> \q
Exiting

```


The user is first asked to insert a card, which is done by typing
`card`: this is the reference to a file containing card data, simulating
a real card. This file is a simple properties-like text file that
contains the card's pincode, a reference to the account's number and
the number of failed pincode:

```
card.account=234567
card.pincode=1234
card.failedcode=0
```

The system uses two other properties files: One for the *Bank database*,
containing the balance for each account, one for the *Cash dispenser*
defining the number of banknotes contained in the dispenser.

Here is the bank database, after the previous transcript:
```
account.234567=50
account.123456=100
account.345678=100
```

and here is the dispenser content:
```
note.10=10
note.20=10
note.50=10
note.100=10
```

If the pincode is right, then the user is presented with a menu of
possible actions:
```
Select your operation:
1 - Account's balance
2 - Withdraw money
0 - Exit
```

The bank and dispenser files are updated when the system is exited,
to reflect distributed notes and account's balance.

The system has some rules:
1. the user has 3 attempts to enter its pincode. After the 3rd failed
    attempt, the card is *retained* erased,
2. the cash dispenser delivers the minimum amount of notes,
3. nothing is delivered if the amount requested is greater than the
    account's balance, greater than the amount available in the
    dispenser, or if it cannot be divided among the available notes,
4. bank and dispenser files are updated to reflect what has been
    delivered and account's balance upon exit.

## Testing the ATM: First approach with FitNesse

Our first objective is to define *functional tests* for this ATM
simulator. Functional tests is a rather fuzzy concept which in this
context basically means: Tests that do what a mundane user would do,
or tests that use the standard UI.

[FitNesse](http://www.fitnesse.org) is a nice tool for designing and executing acceptance
tests. Fitnesse tests are **tables** (ie. represented in HTML as =<table>=
elements) the content of which is interpreted by an executor called
[Slim](http://www.fitnesse.org/FitNesse.SliM) and transformed into object constructions and method calls. Slim
uses a standard protocol to communicate with the prenseter (here
fitnesse).

What we want to do is:
 1. allow definition of scenarios that express sequences of
    interactions and queries on the SUT,
 2. interact with Slim using its custom protocol when running thoses
    scenarios. This means we have to start the Slim server, connect
    to it, send it packets for our requests and interpret its answers,
 3. display to the user the result of the scenarios.

## Interacting with the SUT: The Slim Protocol

All the low-level details of interacting with Slim backend are
described [here](slim.muse).

## Defining tests: An Imperative language

```
module ATM where
import Control.Monad
import Control.Monad.State
```

To define acceptance tests, we shall create a small combinators
language whose elements reprensent basic interactions with the
ATM. We can group these actions in a type class, thus defining the
interface of the system we want to state:

```
class (Monad m) => ATM m where
  enterCard      :: Card -> m ()
  enterPinCode   :: Int  -> m (Either Bool CardRetained)
  withdrawAction :: m ()
  enterAmount    :: Int  -> m [Note]
  getBalance     :: m Int
  exit           :: m ()


type CardRetained = String
type Note = (Int,Int)
```

This type-class defines a *monadic* interface to our ATM so that:
 1. we can use special *do* syntax for monads to give our scenarios a
    more fluent aspect,
 2. we can encapsulate all possible necessary side-effects our
    language may have into an arbitrary context, ie. a *Monad*.

## Mocking test backend

To make this more concrete, we have a test module that will define a
est-only monad that will log all our actions into a stream of
events. This is greatly inspired by [Real World Haskell](http://book.realworldhaskell.org/read/programming-with-monads.html)'s chapter on
testing monadic code (see p.380).

```
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Test.HUnit
import Control.Monad.State
import Control.Monad.Writer
import IO (stderr)
import ATM

runTest :: Test -> IO Counts
runTest  t = do (counts, _) <- runTestText (putTextToHandle stderr False) t
		return counts

runUnitTests = runTest $ TestList [
			   logSampleScenario
			  ]

main = runUnitTests

```

Our type of =ATMEvent= represents what is input by the scenario. The
=ATMTester= is a *newtype* encapsulating a *Writer* monad (ie. something
that can be *written* to, for example to accumulate things, log data,
update an environment...).

```
data ATMEvent = EnterCard Card
              | EnterPinCode Int
              | WithdrawAction
              | EnterAmount Int
              | GetBalance
              | Exit
         deriving (Eq, Show)

newtype ATMTester a = T { runT :: Writer [ATMEvent] a }
    deriving (Monad, MonadWriter [ATMEvent])

runAtm = runWriter . runT
```

The implementation of the *ATM* interface is straightforward, each
function simply recording its event into the writer monad. We then
write a simple test for our tester !

```

instance ATM ATMTester where
  enterCard      card = tell [EnterCard card]
  enterPinCode    pin = tell [EnterPinCode pin] >> return (Left True)
  withdrawAction      = tell [WithdrawAction]
  enterAmount  amount = tell [EnterAmount amount] >> return [ (1, 10) ]
  getBalance          = tell [GetBalance] >> return 10
  exit                = tell [Exit]

logSampleScenario =
   (snd $ runAtm (do enterCard "mycard"
                     (Left isOK) <- enterPinCode 1234
              	     withdrawAction
              	     bal <- getBalance
              	     exit)) ~?= [EnterCard "mycard",EnterPinCode 1234,WithdrawAction,GetBalance,Exit]

```

This implementation is extremely simple of course. For example,
nothing prevents us from sequencing incorrectly the various actions,
eg. withdrawing money before entering the card. This is not however the role
of the tests we shall write to enforce this, but of course we need to
verify that the *state* of the underlying ATM is correct, something
which may only be possible by trying actions that should not be
possible in certain states. The underlying concrete test executor
should be able verify this.

## Real test backend

We now turn to the problem of executing our test cases on a real
SUT, which amounts to defining an =ATMTester= instance that will send
and receive messages from the SUT using the [Slim](slim.muse) protocol handler.

# Testing the ATM: Modelling and generating

Defining our test language at the *value level*, ie. with functional
combinators and some types defining manipulated data allows us to
write a lot of useful *acceptance* tests, following what we can do with
FitNesse.

Our goal, however, is to be able to automatically **generate** test cases
in order to cover a larger behavior space of the SUT. Generated test
cases should have the same *shape* than our primitive combinator
language. Actually, what we would like to do is generate scenarios as
sequence of such monadic combinators.

```

data Card = Card { pin :: String,
                   accountNo :: String,
                   failedCode :: Int }
          deriving (Eq, Show)

data ATMState = Init
              | EnteringPin
              | SelectingAction
              | SelectingAmount
                deriving (Eq, Show)


data ATMInput = EnterCard Card
              | EnterPinCode String
              | WithdrawMoney
              | EnterAmount Int
              | GetBalance
              | Exit
                deriving (Eq, Show)


data ATMOutput = FailedCode
                 | CardRetained
                 | SelectOperation
                 | NotEnoughBalance
                 | ATMDepleted
                 | DeliverNotes
                 | Bal Int
                 | OK
                 | Bye
                   deriving (Eq, Show)


type Trans = (ATMState, ATMInput, ATMOutput, ATMState)
type Path  = [ Trans ]
newtype ATMMachine = M Path
    deriving (Eq, Show)

```
