-- | Graph related functions for Automaton.
-- This module provides functions to transform an IOAutomaton into an inductive Graph
-- structure and to use that graph structure to output a Graphviz compatible representation
-- of the automaton.
module IOAutomaton.Graph where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import IOAutomaton.Generator
import qualified Data.Map as M
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.List
import Data.Maybe
import qualified Data.Text.Lazy as L

-- | Produces a representation of some automaton in GraphViz's dot format.
-- The output produced by this function can later on be passed to GraphViz's
-- to generate a visual representation of a model. This command should be called
-- using something like:
--  toGraphviz (stateMachine :: [ Gen ( ATMState, ATMInput, ATMOutput, ATMState)])
-- ie. using the stateMachine function provided by the transition generator class.
toGraphviz :: (TransitionGenerator q i o, Show q, Ord q, Show o, Show i) =>  [Gen (q,i,o,q)] -> String
toGraphviz = L.unpack . printDotGraph . toDotGraph

-- | Transform a directed graph into an annotated GraphViz graph.
-- We use the startState function to detect initial states in the system.
toDotGraph :: (TransitionGenerator l i o, Show l, Ord l, Show o, Show i) =>
              [Gen (l, i, o, l)] -> DotGraph Node
toDotGraph ioautoamata = graphToDot params (makeGraph ioautoamata)
    where nodeToAttr (_n, a) | a == startState = [Style [SItem Bold []]]
                             | otherwise       = []

          edgeToAttr :: (Show o, Show i) => (l,l,(i,o)) -> Attributes
          edgeToAttr (_from, _to, trans) = [toLabel $ show trans]

          params :: (TransitionGenerator l i o, Show l, Ord l, Show o, Show i) => GraphvizParams s l (i,o) () l
          params = nonClusteredParams { isDirected = True,
                                        globalAttributes = [GraphAttrs [Page (createPoint 11 8.5),
                                                                        RankDir FromLeft,
                                                                        Center True]],
                                        fmtNode = nodeToAttr,
                                        fmtEdge = edgeToAttr}

-- | Transform a list of transition generators into a Graph, instantiating each transition to some arbitrary value.
-- This function is intended to be called by passing it a stateMachine instance, but of course can be used
-- with any list of generators.
makeGraph :: (TransitionGenerator q i o, Show q, Ord q) =>
             [Gen (q,i,o,q)] -> Gr q (i,o)
makeGraph machine = mkGraph astates aedges
  where adelta  = unGen (sequence machine) (mkQCGen 13) 12
        astates = counter 0 (nub (map (\(q,_i,_o,_q') -> q) adelta
                                  ++ map (\(_q,_i,_o,q') -> q') adelta))
        statesMap = M.fromList (map (\(i,q) -> (q,i)) astates)
        aedges = map (toNumberedStates statesMap) (map (\(q,i,o,q') -> (q,q',(i,o))) adelta)

        counter _count []    = []
        counter count (x:xs) = (count,x) : counter (count+1) xs

        toNumberedStates m (q,q',(i,o)) = (fromJust $ M.lookup q m , fromJust $ M.lookup q' m, (i,o))
