{-# LANGUAGE TupleSections, LambdaCase, RankNTypes #-}
module GB.Util.SimState (SNL, simReadyNL, simulate) where

import qualified Data.IntMap.Lazy as IM
import Data.IntMap.Lazy (IntMap)
import qualified Data.Map.Strict as M
import Data.Map (Map)
import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import Data.Array.IArray
import Data.Array.Unboxed
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
import Control.Monad.State
import Data.Array.ST
import Data.Text (Text)
import Data.Array.Unsafe
import System.Random
import Data.Bits

type GateEval s = (SR -> ST s Bool) -> Bool -> ST s Bool
type OutEval s = Map Text (UArray Int Bool) -> ST s Bool

(<$$>) :: (Functor f) => f (a -> b) -> a -> f b
(<$$>) = flip (fmap . flip id)
infixl 4 <$$>
pamf :: (Functor f) => f (a -> b) -> a -> f b
pamf = flip (fmap . flip id)

data SNL s = SNL {
  snlGates :: Array Int (GateEval s),
  snlOutputs :: Map Text [OutEval s],
  snlUsed :: Array Int (IntSet, IntSet),
  snlInputs :: (IntSet, IntSet),
  snlInputLens :: Map Text Int,
  randBits :: STRef s [Bool],
  snlOuts :: STUArray s Int Bool
  }

randomBits :: (RandomGen g) => g -> [Bool]
randomBits = concatMap listIntBits . unfoldr (Just . random)
listIntBits :: Integer -> [Bool]
listIntBits =  (<$> enumFromTo 0 (finiteBitSize (0 :: Int) - 1)) . testBit

simReadyNL :: (RandomGen g) => g -> NetList -> ST s (SNL s)
simulate :: Map Text [Bool] -> SNL s -> ST s (Map Text [Bool])

modifyArray :: (MArray a e m, Ix i, NFData e) => a i e -> i -> (e -> e) -> m ()
simGate :: Gate -> GateEval s
simOut :: STUArray s Int Bool -> Bool -> Maybe Text -> Int -> OutEval s
computeRefs :: Int -> IntMap Gate -> Array Int (IntSet, IntSet)
makeGateList :: Int -> IntMap Gate -> Array Int (GateEval s)
computeInputs :: IntMap Gate -> (IntSet, IntSet)
fixupB :: [Bool] -> UArray Int Bool

fixupB xs = listArray (0, length xs - 1) (reverse xs)

simReadyNL rng (NetList i o g _ m) = do
  out <- newListArray (0, m - 1) rBits
  let outputs = fmap (uncurry $ flip $ uncurry . simOut out) <$> M.fromList o
  SNL gates outputs used inputs inputLens <$> newSTRef rBits' <$$> out
  where
    gates = makeGateList m g
    used = computeRefs m g
    inputs = computeInputs g
    inputLens = M.fromList i
    rBits' = drop m rBits
    rBits = randomBits rng

makeGateList m g = runSTArray $ do
  gs <- newArray_ (0, m - 1)
  flip IM.traverseWithKey g $ \i gg -> writeArray gs i $ simGate gg
  return gs

modifyArray a i f = writeArray a i =<< force . f <$> readArray a i

computeRefs m g = runSTArray $ do
  gs <- newArray (0, m - 1) (IS.empty, IS.empty)
  flip IM.traverseWithKey g $ \i -> \case
    GDelay (Nothing, ix) -> modifyArray gs ix (second $ IS.insert i)
    x -> forM_ (wires x) $ uncurry $ maybe
      (flip (modifyArray gs) $ first $ IS.insert i) (const $ const $ return ())
  return gs
  
computeInputs = IM.foldrWithKey possAdd (IS.empty, IS.empty) where
  possAdd i = \case
    GDelay (Just _, _) -> second $ IS.insert i
    g -> if any (isJust . fst) (wires g)
         then first $ IS.insert i
         else id

simOut a f Nothing i = const $ f' $ readArray a i
simOut _ f (Just n) i = f' . return . (! i) . (M.! n)

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

extractOutputsInt :: Map Text (UArray Int Bool) -> SNL s ->
                     ST s (Map Text [Bool])
simInt :: Map Text (UArray Int Bool) -> SNL s -> ST s ()

extractOutputsInt m nl = traverse ($ m) `traverse` snlOutputs nl

specialMerge :: (Ord k) => Map k [a] -> Map k b -> Map k ([a], b)
specialMerge =
  M.mergeWithKey (const $ (Just .) . (,)) (const M.empty) (([],) <$>)

tack :: ([Bool], Int) -> State [Bool] [Bool]
tack (as, l) = do
  rs <- state $ splitAt $ l - length as
  return (rs ++ as)

simulate m nl = do
  (m', rbs) <- runState (traverse tack $ specialMerge m $ snlInputLens nl) <$>
    readSTRef (randBits nl)
  writeSTRef (randBits nl) rbs
  let m'' = fixupB <$> m'
  simInt m'' nl
  extractOutputsInt m'' nl

look :: Map Text (UArray Int Bool) -> STUArray s Int Bool -> SR -> ST s Bool
look m _ (Just n, i) = return $ (m M.! n) ! i

look _ r (Nothing, i) = fromJust <$> readArray r i

simStuff :: Array Int GateEval -> (SR -> ST s Bool) ->
            Array Int (IntSet, IntSet) ->
            STUArray s Int Bool -> (IntSet, IntSet) -> ST s ()

simInt m (SNL g _ u i _ _ o) = simStuff g (look m o) u o i

forcepair :: (a, b) -> (a, b)
forcepair x@(a, b) = a `seq` b `seq` x
  
simStuff g l u r = simA where
  simA (si, sl) = if IS.null si
                  then if IS.null sl then return ()
                       else simA $ simB sl
                  else do
    let (i, si') = IS.deleteFindMin si
    cur <- l (Nothing, i)
    nex <- (g ! i) l cur
    if cur == nex then simA (si', sl) else do
      writeArray r i nex
      let (sie, sle) = u ! i
      sl `seq` simA (si' <> sie, sl <> sle)
  simB = IS.foldr' simC $ return (IS.empty, IS.empty)
--  simC :: Int -> ST s (IntSet, IntSet) -> ST s (IntSet, IntSet)
  simC i ss = do
    cur <- l (Nothing, i)
    nex <- (g ! i) l cur
    if cur == nex then ss else do
      writeArray r i nex
      let (sie, sle) = u ! i
      forcepair . ((sie <>) *** (sle <>)) <$> ss

-- Algorithm: Mantain lists of current, next.
-- While current nonempty, take an element from it and update that;
-- if changes, add requisite things to current and next.
-- When current nonempty, update everything in next simultaneously,
-- and for those that change, add their effects to current and next.
