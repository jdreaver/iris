-- | Investigation into general tree structures with nested Behaviors. This is
-- directly applicable to compiling and/or interpreting a scene graph
-- efficiently.

module BehaviorTree where

import Control.Monad (void)
import Reactive.Banana
import Reactive.Banana.Frameworks

data BTree a = BTree (Value a) [BTree a]
data Tree a = Tree a [Tree a]

data Value a = DynamicValue (Behavior a)
             | PureValue a

-- | Extracts pure tree from dynamic tree
reify :: BTree a -> Moment (Tree a)
reify (BTree val subs) =
  do pureVal <- reifyValue val
     pureSubs <- mapM reify subs
     return $ Tree pureVal pureSubs

reifyValue :: Value a -> Moment a
reifyValue (DynamicValue b) = valueB b
reifyValue (PureValue x) = return x

-- | Turns BTree into a Behavior of a pure tree
reify' :: BTree a -> Moment (Behavior (Tree a))
reify' (BTree val subs) =
  do let valB = reifyValue' val
     subsB <- mapM reify' subs
     return $ Tree <$> valB <*> sequenceA subsB


reifyValue' :: Value a -> Behavior a
reifyValue' (DynamicValue b) = b
reifyValue' (PureValue x) = pure x

reifyEvent :: Event () -> BTree a -> Moment (Event (Tree a))
reifyEvent e t =
  do bTree <- reify' t
     return $ bTree <@ e

drawTree :: Tree (IO ()) -> IO ()
drawTree (Tree f fs) = f >> mapM_ drawTree fs

drawTreeMoment :: Event () -> BTree (IO ()) -> MomentIO ()
drawTreeMoment e t =
  do treeE <- liftMoment $ reifyEvent e t
     reactimate $ drawTree <$> treeE

-- | Takes a nested list of Behaviors and computes a Behavior w/ just the list.
-- This is only possible because we are given an Event to sample on. In
-- general, a function of the following type is ill-defined:
--
-- f :: Behavior (Behavior a) -> Behavior a
--
-- Therefore, we construct a new Behavior that uses the given Event as an
-- indicator of when to sample all of the child Behaviors.
nestedB :: Event () -> Behavior [Behavior a] -> Moment (Behavior [a])
nestedB e bs =
  do let bs' = behaviorE e <$> bs
         bsE = switchE $ bs' <@ e
     currentB <- valueB bs
     currentBS <- valueB (sequenceA currentB)
     stepper currentBS bsE

behaviorE :: Event () -> [Behavior a] -> Event [a]
behaviorE e bs = sequenceA bs <@ e

-- | Version of BTree w/ a dynamic number of child nodes
data BTreeDyn a = BTreeDyn (Value a) (Behavior [BTreeDyn a])

reifyDyn :: Event () -> BTreeDyn a -> Moment (Behavior (Tree a))
reifyDyn e (BTreeDyn val subsB) =
  do pureVal <- reifyValue val
     childrenB <- dynSubs e subsB
     return $ Tree <$> pure pureVal <*> childrenB

-- | Version of nestedB for BTreeDyn
dynSubs :: Event () -> Behavior [BTreeDyn a] -> Moment (Behavior [Tree a])
dynSubs e b =
  do let cE = b <@ e
         cE' = dynSubs' e <$> cE
         csE = switchE $ (<@ e) <$> observeE cE'
     currTree <- valueB b >>= dynSubs' e >>= valueB
     stepper currTree csE


dynSubs' :: Event () -> [BTreeDyn a] -> Moment (Behavior [Tree a])
dynSubs' e bs =
  do bs' <- mapM (reifyDyn e) bs
     return $ sequenceA bs'


sampledBehavior :: Event () -> Behavior (Behavior a) -> Moment (Behavior a)
sampledBehavior e b =
  do valNow <- valueB b >>= valueB
     let e' = switchE (((<@ e) <$> b) <@ e)
     stepper valNow e'


-- Implement join for Observables
data Observable a = Observable (Behavior a) (Event a)

joinObs :: Observable (Observable a) -> Moment (Observable a)
joinObs (Observable oB oE) =
  do let e = switchE $ ((\(Observable b e') -> b <@ (oE `union` e')) <$> oB) <@ oE
     valNow <- valueB oB >>= (\(Observable b _) -> valueB b)
     b <- stepper valNow e
     return $ Observable b e

union :: Event a -> Event b -> Event ()
union e1 e2 = unionWith (\_ _ -> ()) (void e1) (void e2)
