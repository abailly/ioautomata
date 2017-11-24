{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
module IOAutomaton.IOAutomaton (
                                Trace(T),
                                IOAutomaton(..),
                                eval,
                                actionST,
                                evalST,
                                runAutomaton,
                                evalTrace, transition
                                ) where

import           Control.Monad.Identity
import           Control.Monad.State    hiding (state)

-- | An IOAutomaton is a special kind of finite state automata.
-- Formally, the kind of IOAutomaton that we use can be defined as a tuple
-- (Q,q0,Sink,I,O,\delta) where:
--   - Q is the set of states of the automaton
--   - q0 is the distinguished initial state
--   - Sink is the distingused terminal state
--   - I is the input alphabet, an arbitrary type
--   - O is the output alphatbet, another arbitrary type
--   - \delta \subseteq Q x I x O x (Q \cup Sink) the set of transitions of the automaton
--
-- A specific IOAutomaton instance is a model of the behavior of some
-- system under test that can be used to:
--  - generate traces, ie. specialized tests cases representing possible behavior
--    of the SUT
--  - validate output of the SUT against expectations (eg. play the role of an oracle)
--
-- Here we distinguish the set of formal states of the automaton (q) and the
-- state of the model (st), the latter usually being much more complex.
class (Eq q, Eq i, Eq o, Show q, Show i, Show o) =>
      IOAutomaton a q i o | a -> q i o where

  -- ^Initial state of the automaton
  init :: a

  -- ^Terminal state. Note that as we consider not only finite systems but one
  -- that can run forever and produce arbitrarily long traces, terminal state
  -- is a sink state. Reaching terminal state is considered a failure.
  sink :: a -> q

  -- ^Input accessor.
  input :: (q, i, o, q) -> i
  input (_,i,_,_) = i

  -- ^Output accessor.
  output :: (q, i, o, q) -> o
  output (_,_,o,_) = o

  -- ^Action of the automaton.
  -- This action is considered a failure, eg. an unacceptable transition in this
  -- automaton's model state if it returns Nothing as first member of returned.
  action :: i -> a -> (Maybe o, a)

  -- ^Associated formal state of a model state
  state :: a -> q

  -- ^Update current state of this automaton
  update :: a -> q -> a

-- |Evaluate a single transition given a certain Automaton
-- This function relies on the underlying 'action' function to apply
-- the transition's input to the current state.
eval :: (IOAutomaton a q i o) => (q, i, o, q) -> a -> ((q, i, o, q), a)
eval t@(s, _i, _o, _e) st | s == sink st = (t,st)
eval   (s, i, o, _e)   st                = eval' (action i st)
     where
       eval' v = case v of
         (Just o', st')  -> ((s, i ,o', state st'), st')
         (Nothing, st' ) -> ((s, i ,o, sink st'), st')

-- | Injects action into State transformer monad.
-- Given a transition, it is applied in the context of a StateT monad
-- whose state is the underlying automaton and whose result type is thus
-- an output or a failure.
actionST :: (Monad m, IOAutomaton a q i o) =>
            (q, i, o, q)  ->
            StateT a m (Maybe o)
actionST (_s, i, _o ,_e) = StateT (return . action i)

-- | Evaluate a transition within a State transformer.
-- The state of the StateT monad is the underlying automaton and
-- the result of running the eval inside the monad is another transition
evalST :: (Monad m, IOAutomaton a q i o) =>
          (q, i, o, q) ->
          StateT a  m (q, i, o, q)
evalST t = StateT (return . eval t)

transition :: q -> (i,o,q) -> (q,i,o,q)
transition st (i,o,f) = (st, i, o , f)

-- |Traces over an automaton.
-- A trace is simply a sequence of transitions.
newtype Trace q i o =  T [ (q, i, o ,q) ]
    deriving (Show, Eq)

-- | Executes a complete trace in a given automaton within the StateT transformer monad.
-- The trace is run over some initial state and produces a final state and a result which
-- is deduced from sequencing actions over the trace: It can be either Nothing, meaning that
-- at some point the action produced an incorrect output, or some output.
runAutomaton  :: (Monad m, IOAutomaton a q i o) =>
     Trace q i o ->
     a ->
     m (Maybe o, a)
runAutomaton (T transitions) =  runStateT $ mapM actionST transitions >>= makeLastState
    where
      makeLastState :: (Monad m) => [Maybe a] -> m (Maybe a)
      makeLastState []      = return Nothing
      makeLastState [x]     = return x
      makeLastState (_x:xs) = makeLastState xs

-- | Evaluates a trace and produces a new trace.
-- This function is similar to runAutomaton but produces a new trace instead of a result
-- and a new state. It is intended to be used as a way to filter candidate traces arising from
-- the use of generators.
evalTrace :: (IOAutomaton a q i o) =>
            Trace q i o  ->
            a ->
            Trace q i o
evalTrace (T transitions) = T . evalState (mapM (StateT . idEval) transitions)
                            where
                              idEval a = Identity . eval a
