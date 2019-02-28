{-# LANGUAGE TupleSections, LambdaCase, RankNTypes #-}
module GB.Util.SimState (SNL, simReadyNL, simulate, checkOutputs,
                         simulate_) where

import qualified Data.IntMap.Lazy as IM
import Data.IntMap.Lazy (IntMap)
import qualified Data.Map.Strict as M
import Data.Map (Map)
import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import Data.Array.IArray
import GB.Util.NetList
import Data.Maybe
import Control.Arrow
import Data.List (foldl', unfoldr)
import Control.DeepSeq
import Control.Monad
import Data.Monoid
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Data.Tuple
import Data.Function
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Function
import Data.Array.ST
import Data.Text (Text)
import Data.Array.Unsafe
import System.Random
import Data.Bits

type GateEval s = Bool -> ST s Bool

(<$$>) :: (Functor f) => f (a -> b) -> a -> f b
(<$$>) = flip (fmap . flip id)
infixl 4 <$$>
pamf :: (Functor f) => f (a -> b) -> a -> f b
pamf = flip (fmap . flip id)

data SNL s = SNL {
  snlGates :: Array Int (GateEval s),
  snlOutputs :: Map Text [ST s Bool],
  snlUsed :: Array Int (IntSet, IntSet),
  snlInputs :: Map Text (Array Int (IntSet, IntSet), STUArray s Int Bool),
  snlOuts :: STUArray s Int Bool
  }

randomBits :: (RandomGen g) => g -> [Bool]
randomBits = concatMap listIntBits . unfoldr (Just . random)
listIntBits :: Integer -> [Bool]
listIntBits =  (<$> enumFromTo 0 (finiteBitSize (0 :: Int) - 1)) . testBit

simReadyNL :: (RandomGen g) => g -> NetList -> ST s (SNL s)
simulate :: Map Text [Bool] -> SNL s -> ST s (Map Text [Bool])
checkOutputs :: SNL s -> ST s (Map Text [Bool])
simulate_ :: Map Text [Bool] -> SNL s -> ST s ()

simulate m nl = simulate m nl >> checkOutputs nl

modifyArray :: (MArray a e m, Ix i, NFData e) => (e -> e) -> a i e -> i -> m ()
simGate :: (SR -> ST s Bool) -> Gate -> GateEval s
simOut :: STUArray s Int Bool -> Map Text (STU s Array Int Bool) ->
          Bool -> SR -> ST s Bool
computeRefsInputs :: Int -> IntMap Gate -> Map Text Int ->
                     (Array Int (IntSet, IntSet),
                      Map Text (Array Int (IntSet, IntSet)))
makeGateList :: Int -> IntMap Gate -> (Gate -> GateEval s) ->
                Array Int (GateEval s)
computeInputs :: IntMap Gate -> Map Text Int ->
                 Map Text (Array Int (IntSet, IntSet))
fillInputs :: Map Text Int -> State [Bool] (Map Text (UArray Int Bool))

fillInputs = traverse $ \i -> state $ listArray (0, i - 1) &&& drop i

look :: Map Text (STUArray s Int Bool, a) -> STUArray s Int Bool -> SR ->
        ST s Bool

look m r = uncurry $ readArray . maybe r (m M.!)

simReadyNL rng (NetList i o g _ m) = do
  out <- newListArray (0, m - 1) rBits
  inp <- traverse unsafeThaw cinput
  let outputs = fmap (uncurry $ flip $ simOut out inp) <$> M.fromList o
  let gates = makeGateList m g $ simGate $ look inp out
  return $ SNL gates outputs used (M.intersectionWith (,) inputs inp) out
  where
    iLen = M.fromList i
    (used, inputs) = computeRefsInputs m g iLen
    cinput = evalState (fillInputs iLen) $ drop m rBits
    rBits = randomBits rng

makeGateList m g f = runSTArray $ do
  gs <- newArray_ (0, m - 1)
  flip IM.traverseWithKey g $ \i gg -> writeArray gs i $ f gg
  return gs

modifyArray f a i = writeArray a i =<< force . f <$> readArray a i
  
computeRefsInputs m g l = runST $ do
  gs <- newArray (0, m - 1) (IS.empty, IS.empty)
  ps <- flip newArray (IS.empty, IS.empty) . (0,) . (-1+) `traverse` g
  let insH f i = uncurry $ modifyArray (f $ IS.insert i) . maybe gs (ps M.!)
  flip IM.traverseWithKey g $ \i -> \case
    GDelay x -> insH second i
    x -> forM_ (wires x) $ insH first i
  gs' <- unsafeFreeze gs
  ps' <- traverse unsafeFreeze ps
  return (gs, ps)

simOut a m False = uncurry $ readArray . maybe a (m M.!)
simOut a m True = uncurry $ fmap not . readArray . maybe a (m M.!)

simConst :: Bool -> GateEval
simUnop :: Bool -> SR -> GateEval
simBinop :: Binop -> Bool -> SR -> SR -> GateEval
simMux :: Bool -> Bool -> SR -> SR -> SR -> GateEval
simDff :: Bool -> SR -> SR -> GateEval
simDffZ :: Bool -> Bool -> Bool -> SR -> SR -> SR -> GateEval
simSR :: Bool -> Bool -> Bool -> SR -> SR -> GateEval
simXor :: Bool -> SR -> SR -> GateEval
simOr :: Bool -> Bool -> Bool -> SR -> SR -> GateEval
orSim :: Maybe Bool -> Maybe Bool -> Maybe Bool
muxSim :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool
dffZSim :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool
srSim :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool

simGate (GConst b) = simConst b
simGate (GUnop f x) = simUnop f x
simGate (GBinop f o l r) = simBinop o f l r
simGate (GMux f0 f1 s d0 d1) = simMux f0 f1 s d0 d1
simGate (GDff fw w d) = simDff fw w d
simGate (GDffZ fw fz zs w z d) = simDffZ fw fz zs w z d
simGate (GSR fs fr fq s r) = simSR fs fr fq s r
simGate (GDelay x) = simUnop False x

xorB :: (Functor f) => Bool -> f Bool -> f Bool
xorB True = not
xorB False = id

-- Use ST to keep track of changes.
-- Change one at a time.

simConst x = \_ _ -> return x
simUnop f x = \g _ -> f' <$> g x where f' = xorB f
simBinop BAnd = simOr True True . not
simBinop BOr = simOr False False
simBinop BImpl = simOr True False
simBinop BXor = simXor
simOr fl fr f = \l r -> \g _ ->
  f' $ orSim <$> (fl' $ g l) <*> (fr' $ g r)
  where f' = xorB f
        fl' = xorB fl
        fr' = xorB fr
simXor f = \l r -> \g _ ->
  liftA2 (/=) <$> f' (g l) <*> g r where
  f' = xorB f
simMux f0 f1 = \s d0 d1 -> \g _ ->
  muxSim <$> (g s) <*> (f0' $ g d0) <*> (f1' $ g d1) where
  f0' = xorB f0
  f1' = xorB f1
simDff fw = \w d -> \g p -> muxSim <$> (fw' $ g w) <$$> p <*> g d where
  fw' = xorB fw
simDffZ fw fz zs = \w z d -> \g p ->
  zs' $ dffZSim <$> (fz' $ g z) <*> (fw' $ g w) <$$>
  zs' p <*> (zs' $ g d) where
  fw' = xorB fw
  fz' = xorB fz
  zs' = xorB zs
simSR fs fr fq = \s r -> \g p ->
  fq' $ srSim <$> (fr' $ g r) <*> (fs' $ g s) <$$> p where
  fs' = xorB fs
  fr' = xorB fr
  fq' = xorB fq

which :: a -> a -> Bool -> a
which a _ True = a
which _ a False = a

orSim True = const True
orSim False = id
muxSim True = const id
muxSim False = const
dffZSim True = muxSim
dffZSim False = const $ const $ const False
srSim True = const $ const False
srSim False = orSim

simInt :: SNL s -> (IntSet, IntSet) -> ST s ()

checkOutputs = traverse sequenceA . snlOutputs

mergeInput :: ([Bool], (Array Int (IntSet, IntSet), STUArray s Int Bool)) ->
              ST s Foo

newtype Foo = Foo {getFoo :: (IntSet, IntSet)}
instance Semigroup Foo where
  (<>) = ((Foo . force) .) . (<>) `on` getFoo
instance Monoid Foo where
  mempty = Foo mempty

simulate_ m nl =
  simInt nl . getFoo . fold <=<
  traverse mergeInput $ M.intersectionWith (,) m $ snlInputs nl

simStuff :: Array Int GateEval ->
            Array Int (IntSet, IntSet) ->
            STUArray s Int Bool -> (IntSet, IntSet) -> ST s ()

simInt = simStuff <$> snlGates <*> snlUsed <*> snlOuts

mergeInput' :: Array Int (IntSet, IntSet) -> STUArray s Int Bool ->
               (Int, Bool) -> ST s Foo

mergeInput (n, (f, o)) =
  foldl' (<>) mempty <$> traverse (mergeInput' f o) (zip [0..] $ reverse n)

mergeInput' fs os = \(i, n) -> do
  o <- readArray os i
  writeArray os i n
  return $ if o == n then mempty else Foo $ fs ! i

simStuff g u r = simA where
  simA (si, sl) = if IS.null si
                  then if IS.null sl then return ()
                       else simA $ simB sl
                  else do
    let (i, si') = IS.deleteFindMin si
    cur <- readArray r i
    nex <- (g ! i) cur
    if cur == nex then simA (si', sl) else do
      writeArray r i nex
      let (sie, sle) = u ! i
      sl `seq` simA (si' <> sie, sl <> sle)
  simB = IS.foldr' simC $ return (IS.empty, IS.empty)
--  simC :: Int -> ST s (IntSet, IntSet) -> ST s (IntSet, IntSet)
  simC i ss = do
    cur <- readArray r i
    nex <- (g ! i) cur
    if cur == nex then ss else do
      writeArray r i nex
      let (sie, sle) = u ! i
      force . ((sie <>) *** (sle <>)) <$> ss

-- Algorithm: Mantain lists of current, next.
-- While current nonempty, take an element from it and update that;
-- if changes, add requisite things to current and next.
-- When current nonempty, update everything in next simultaneously,
-- and for those that change, add their effects to current and next.
