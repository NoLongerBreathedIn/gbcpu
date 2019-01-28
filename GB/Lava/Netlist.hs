{-# LANGUAGE TypeFamilies, TupleSlices #-}
module GB.Lava.Netlist (LavaGen, netlist, sigs) where

import GB.Lava.Signal
import Data.Reify
import Data.Maybe
import Data.Functor
import Data.Foldable
import System.IO.Unsafe

class LavaGen g where
  type Fixup g
  struct :: g -> Fixup g -> Signal
  fixup :: g -> Fixup g
  sigs :: g -> [(String, Int)]
  destruct :: Graph Sig -> g -> [((String, Int), Int)]

netlist :: (LavaGen a, LavaGen b) => (Fixup a -> Fixup b) -> a -> b ->
           ([(Int, Sig Int)], [((String, Int), Int)])

netlist f a b = (graph, names) where
  Graph graph p = unsafePerformIO $ reifyGraph $ struct b $ f $ fixup a
  names = destruct (Graph graph p) b
  
get :: Graph Sig -> Sig Int
get (Graph g i) = fromJust $ lookup i g

instance LavaGen String where
  Fixup String = Signal
  struct _ = id
  fixup = var
  sigs = (:[]) . (, 1)
  destruct (Graph _ i) n = [((n, 0), i)]

instance LavaGen () where
  Fixup () = ()
  struct _ _ = high
  fixup _ = ()
  sigs _ = []
  destruct _ _ = []

instance LavaGen (String, Int) where
  Fixup (String, Int) = [Signal]
  struct _ = foldr (curry and2) high
  fixup = map varPosn . sigs
  sigs (n, i) = (n,) <$> [i - 1, i - 2 .. 0]
  destruct _ (_, 0) = []
  destruct g@(Graph h _) (n, i) = ((n, i - 1), a) :
                                  destruct (Graph h b) (n, i - 1) where
    (a, b) = case get g of
      And x y -> (x, y)

instance LavaGen a => LavaGen [a] where
  Fixup [a] = [Fixup a]
  struct = (foldr (curry and2) high .) . zipWith struct
  fixup = map fixup
  sigs = concatMap sigs
  destruct _ [] = []
  destruct g@(Graph h _) (x:xs) = destruct (Graph h a) x ++
                                  destruct (Graph h b) xs where
    (a, b) = case get g of
      And x y -> (x, y)

instance (LavaGen a, LavaGen b) => LavaGen (a, b) where
  Fixup (a, b) = (Fixup a, Fixup b)
  struct (a, b) (c, d) = and2 (struct a c, struct b d)
  fixup = fixup *** fixup
  sigs (a, b) = sigs a ++ sigs b
  destruct g@(Graph h _) (a, b) = destruct (Graph h x) a ++
                                  destruct (Graph h y) b where
    (x, y) = case get g of
      And p q -> (p, q)

foo3 :: (a, b, c) -> (a, (b, c))
foo3 (a, b, c) = (a, (b, c))
foo4 :: (a, b, c, d) -> (a, (b, c, d))
foo4 (a, b, c, d) = (a, (b, c, d))
foo5 :: (a, b, c, d, e) -> (a, (b, c, d, e))
foo5 (a, b, c, d, e) = (a, (b, c, d, e))
foo6 :: (a, b, c, d, e, f) -> (a, (b, c, d, e, f))
foo6 (a, b, c, d, e, f) = (a, (b, c, d, e, f))
foo7 :: (a, b, c, d, e, f, g) -> (a, (b, c, d, e, f, g))
foo7 (a, b, c, d, e, f, g) = (a, (b, c, d, e, f, g))


instance (LavaGen a, LavaGen b, LavaGen c) => LavaGen (a, b, c) where
  Fixup (a, b, c) = (Fixup a, Fixup b, Fixup c)
  struct = (. foo3) . struct . foo3
  fixup (a, b, c) = (fixup a, fixup b, fixup c)
  sigs = sigs . foo3
  destruct = (. foo3) . destruct

instance (LavaGen a, LavaGen b, LavaGen c, LavaGen d) =>
         LavaGen (a, b, c, d) where
  Fixup (a, b, c, d) = (Fixup a, Fixup b, Fixup c, Fixup d)
  struct = (. foo4) . struct . foo4
  fixup (a, b, c, d) = (fixup a, fixup b, fixup c, fixup d)
  sigs = sigs . foo4
  destruct = (. foo4) . destruct

instance (LavaGen a, LavaGen b, LavaGen c, LavaGen d, LavaGen e) =>
         LavaGen (a, b, c, d, e) where
  Fixup (a, b, c, d, e) = (Fixup a, Fixup b, Fixup c, Fixup d, Fixup e)
  struct = (. foo5) . struct . foo5
  fixup (a, b, c, d, e) = (fixup a, fixup b, fixup c, fixup d, fixup e)
  sigs = sigs . foo5
  destruct = (. foo5) . destruct

instance (LavaGen a, LavaGen b, LavaGen c, LavaGen d, LavaGen e, LavaGen f) =>
         LavaGen (a, b, c, d, e, f) where
  Fixup (a, b, c, d, e, f) = (Fixup a, Fixup b, Fixup c,
                              Fixup d, Fixup e, Fixup f)
  struct = (. foo6) . struct . foo6
  fixup (a, b, c, d, e, f) = (fixup a, fixup b, fixup c,
                              fixup d, fixup e, fixup f)
  sigs = sigs . foo6
  destruct = (. foo6) . destruct

instance (LavaGen a, LavaGen b, LavaGen c, LavaGen d,
          LavaGen e, LavaGen f, LavaGen g) =>
         LavaGen (a, b, c, d, e, f, g) where
  Fixup (a, b, c, d, e, f, g) = (Fixup a, Fixup b, Fixup c,
                                 Fixup d, Fixup e, Fixup f, Fixup g)
  struct = (. foo7) . struct . foo7
  fixup (a, b, c, d, e, f, g) = (fixup a, fixup b, fixup c,
                                 fixup d, fixup e, fixup f, fixup g)
  sigs = sigs . foo7
  destruct = (. foo7) . destruct
