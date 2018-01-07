{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-| Provides way to check whether or not some code respects an @IOAutomaton@ model.

This modules provides mainly two things:

 * A high-level "model checker" that can be used to validate some implementation
   against an `IOAutomaton` model, given some `Interpreter`

 * A "mock" model that can be used to generate answers from an `IOAutomaton` given
   some inputs.

Given a client-server relationship, the former can be used to validate the server
and the latter to validate the client.
-}
module IOAutomaton.Model where

import           IOAutomaton.Generator
import           IOAutomaton.IOAutomaton
import           Prelude
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

class (IOAutomaton a q i o, Monad m) => Interpreter m a q i o | a -> q i o where
  interpret :: a -> i -> m (Maybe o, a)
  before    :: a -> m ()
  after     :: RunResult a q i o -> m ()


data RunResult a q i o = RunSuccessful a (Trace q i o)
                       | UnexpectedOutput a (q, i, o ,q) o
                       | UnexpectedInput a i
                       | CannotInterpretInput a (q, i, o, q)
                       deriving (Eq, Show)

isSuccessful :: RunResult a q i o -> Bool
isSuccessful (RunSuccessful _ _) = True
isSuccessful _                   = False

-- | Run a single transition against interpreter
testTrans :: (Interpreter m a q i o) => a -> (q, i, o ,q) -> m (Maybe o, a)
testTrans a (_s, i, _o, _e) = interpret a i

-- | Run a sequence of transitions against interpreter, checking the returned
-- output is valid.
--
testSUT :: (Monad m, Interpreter m a q i o) => a -> Trace q i o -> m (RunResult a q i o)
testSUT a (T trans) = do
  before a
  res <- checkSUT a trans []
  after res
  return res

-- | Main check function returns failing instruction or Nothing if SUT passes
-- check
checkSUT :: (Interpreter m a q i o) => a -> [ (q, i, o ,q) ] -> [ (q, i, o ,q) ] -> m (RunResult a q i o)
checkSUT at  []                   acc = pure (RunSuccessful at $ T $ reverse acc)
checkSUT at (t'@(_s,i,o,e):trans) acc = do
  (o', a') <- interpret at i
  case o' of
    Just t  -> if t == o
               then checkSUT (update a' e) trans (t':acc)
               else pure (UnexpectedOutput at t' t)
    Nothing -> pure (CannotInterpretInput at t')


-- | Returns empty machine if cannot generate a meaningful one
validMachines :: (IOAutomaton a q i o, TransitionGenerator q i o)
              => a -> ([(q, i, o, q)] -> Bool) -> Gen (Trace q i o)
validMachines initial isValidPath =
  do m  <- arbitrary
     let T m' = evalTrace m initial
     return (if isValidPath m'
             then T m'
             else T [])


-- | Check an @Interpreter@ against a given @IOAutomaton@ model
modelCheck :: ( IOAutomaton a q i o
              , TransitionGenerator q i o
              , Monad m
              , Interpreter m a q i o)
           => a
           -> ([(q, i, o, q)] -> Bool)
           -> PropertyM m ()
modelCheck initial validPath =
  forAllM (validMachines initial validPath) $ \m ->
    do pre $ nonEmpty m
       b' <- run $ testSUT initial m
       assert (isSuccessful b')
    where
      nonEmpty (T []) = False
      nonEmpty _      = True

class (Monad m) => Interactive m i o | o -> i, i -> o where
  request :: m (Maybe i)
  reply   :: o -> m ()

-- | Generates outputs respecting an `IOAutomaton` model given inputs in a monadic context
--
-- This is currently very simple and assumes direct request/response relationship,
-- there is one response sent for each request.
mockModel :: ( IOAutomaton a q i o, Interactive m i o )
          => a
          -> Trace q i o
          -> m (RunResult a q i o)
mockModel initial (T trans) = do
  req <- request
  case req of
    Nothing -> pure $ RunSuccessful initial (T $ reverse trans)
    Just i  -> do
      let (out, newState) = action i initial
      case out of
        Nothing -> pure $ UnexpectedInput initial i
        Just o  -> reply o >> mockModel newState (T $ (state initial, i , o, state newState):trans)
