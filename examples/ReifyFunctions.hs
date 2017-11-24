{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
-- | From <data-reify https://github.com/abailly/beyond-tdd/blob/master/reifyGraph.pdf> paper
module ReifyFunctions where

import           Data.Dynamic
import           Data.Reify


-- | A type to represent polymorphic graph structure
data Node a = Cons a a
            | Nil
            | Lambda a a
            | Var
            | Const Int
            | Add a a
  deriving (Show, Typeable)

instance ( Typeable a
         , MuRef a
         , DeRef [a] ~ DeRef a
         ) => MuRef [a] where

  type DeRef [a] = Node

  mapDeRef f (x:xs) = Cons <$> f x <*> f xs
  mapDeRef _ []     = pure Nil

class NewVar a where
  mkVar :: Dynamic -> a

capture :: (Typeable a, Typeable b, NewVar a)
        => (a -> b) -> (a,b)
capture f = (a, f a)
  where
    a = mkVar (toDyn f)

data Exp = ExpVar Dynamic
         | ExpLit Int
         | ExpAdd Exp Exp
         deriving (Show, Typeable)

instance NewVar Exp where
  mkVar = ExpVar

instance Num Exp where
  (+) = ExpAdd
  fromInteger = ExpLit . fromInteger
  -- other methods omitted for convenience and brevity

instance ( MuRef a, Typeable a, NewVar a
         , MuRef b, Typeable b
         , DeRef a ~ DeRef (a -> b)
         , DeRef b ~ DeRef (a -> b)
         ) => MuRef (a -> b) where
  type DeRef (a -> b) = Node

  mapDeRef f fn = let v = mkVar $ toDyn fn
                  in Lambda <$> f v <*> f (fn v)

instance MuRef Exp where
  type DeRef Exp = Node

  mapDeRef _ (ExpVar _)   = pure Var
  mapDeRef _ (ExpLit n)   = pure $ Const n
  mapDeRef f (ExpAdd x y) = Add <$> f x <*> f y


sample :: Exp -> Exp
sample = let f = (+ (1 :: Exp))
             g = (+ 2)
         in f . g
