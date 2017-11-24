{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
module IOAutomaton.Generator where

import           Data.Maybe              (isJust)
import qualified IOAutomaton.IOAutomaton as A
import           Test.QuickCheck

-- |Apply a given function over all elements of a list and select one of the
-- results.
selector :: (Arbitrary b) => [a] -> (a -> b) -> Gen b
selector list ctor = oneof (map (return . ctor) list)

-- |Specific IOAutomaton models provide instances of this class to generate
-- 'valid' traces.
class (Eq q) =>
      TransitionGenerator q i o  | q -> i o where

  -- | Generate one transition from the given state
  someTransition :: q -> Gen (q, i, o, q)
  someTransition st = sequence stateMachine >>= (oneof . map return . filter sameState)
    where
      sameState :: (Eq q) => (q, i, o, q) -> Bool
      sameState (s,_i,_o,_e) = st == s

  -- | Provide all transitions for a given model
  stateMachine :: [ Gen (q,i,o,q)]

  -- | Provide starting state of the automaton
  startState     :: q

  -- | Generate some number of transitions from given state
  transitions    :: q -> Int -> Gen [ (q,i,o,q) ]
  transitions _st 0 = return []
  transitions st  n = do t@(_,_,_,st') <- someTransition st
                         rest <- transitions st' (n-1)
                         return (t : rest)

-- | Given a transition generator and some atomic components generators,
-- we can define an arbitrary trace instance.
instance (TransitionGenerator q i o)
         => Arbitrary (A.Trace q i o) where
  arbitrary = A.T <$> sized (transitions startState)

-- |Generate valid traces to stdout.
-- This function is mainly used for testing purpose as it allows one to easily
-- generates a bunch of traces for a given model.
generate :: (A.IOAutomaton a q i o,
             TransitionGenerator q i o) => a -> IO ()
generate start = do candidates <- sample' arbitrary
                    putStrLn $ "generating " ++ show (length candidates) ++ " candidates"
                    let accepted = filter (select start) candidates
                    putStrLn $ "selecting " ++ show (length accepted) ++ " traces"
                    mapM_ print accepted
                    return ()
                      where
                        select :: (A.IOAutomaton a q i o) => a -> A.Trace q i o -> Bool
                        select st a = isJust (A.runAutomaton a st)