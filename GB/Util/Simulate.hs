{-# LANGUAGE TupleSections, LambdaCase, ExplicitForAll, RankNTypes #-}
module GB.Util.Simulate (SNL, simReadyNL, simulate) where

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
import Data.List (foldl')
import Control.DeepSeq
import Control.Monad
import Data.Monoid
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Data.Tuple
import Data.Function
import Control.Monad.ST
import Data.Array.ST
import Data.Text (Text)
import Data.Array.Unsafe

newtype GateEval = GateEval {getGE :: forall s.
  (SR -> ST s (Maybe Bool)) -> Maybe Bool -> ST s (Maybe Bool)}

(<$$>) :: (Functor f) => f (a -> b) -> a -> f b
(<$$>) = flip (fmap . flip id)
infixl 4 <$$>
  

data SNL = SNL {
  snlGates :: Array Int (Either SR GateEval),
  snlOuts :: UArray Int Bool,
  snlKnown :: UArray Int Bool,
  snlOutputs :: Map Text [(SR, Bool)],
  snlUsed :: Array Int (IntSet, IntSet),
  snlInput :: (IntSet, IntSet)
  }

simReadyNL :: NetList -> SNL
simulate :: Map Text [Bool] -> SNL -> (SNL, Map Text [Maybe Bool])

modifyArray :: (MArray a e m, Ix i, NFData e) => a i e -> i -> (e -> e) -> m ()
simGate :: Gate -> Either SR GateEval
computeRefs :: Int -> IntMap Gate -> Array Int (IntSet, IntSet)
makeGateList :: Int -> IntMap Gate -> Array Int (Either SR GateEval)
computeInputs :: IntMap Gate -> (IntSet, IntSet)
fixupB :: [Bool] -> UArray Int Bool

fixupB xs = listArray (0, length xs - 1) (reverse xs)

simReadyNL (NetList _ o g _ m) = SNL (makeGateList m g)
                                 (listArray (0, m-1) $ repeat False)
                                 (listArray (0, m-1) $ repeat False)
                                 (M.fromList o)
                                 (computeRefs m g)
                                 (computeInputs g)

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

simGate (GConst b) = Right $ simConst b
simGate (GUnop f x) = Right $ simUnop f x
simGate (GBinop f o l r) = Right $ simBinop o f l r
simGate (GMux f0 f1 s d0 d1) = Right $ simMux f0 f1 s d0 d1
simGate (GDff fw w d) = Right $ simDff fw w d
simGate (GDffZ fw fz zs w z d) = Right $ simDffZ fw fz zs w z d
simGate (GSR fs fr fq s r) = Right $ simSR fs fr fq s r
simGate (GDelay x) = Left x

xorB :: (Functor f) => Bool -> f Bool -> f Bool
xorB True = fmap not
xorB False = id

-- Use ST to keep track of changes.
-- Change one at a time.

simConst x = GateEval $ \_ _ -> return (Just x)
simUnop f x = GateEval $ \g _ -> f' <$> g x where f' = xorB f
simBinop BAnd = simOr True True . not
simBinop BOr = simOr False False
simBinop BImpl = simOr True False
simBinop BXor = simXor
simOr fl fr f = \l r -> GateEval $ \g _ ->
  fmap f' $ orSim <$> (fl' <$> g l) <*> (fr' <$> g r)
  where f' = xorB f
        fl' = xorB fl
        fr' = xorB fr
simXor f = \l r -> GateEval $ \g _ ->
  liftA2 (/=) <$> fmap f' (g l) <*> g r where
  f' = xorB f
simMux f0 f1 = \s d0 d1 -> GateEval $ \g _ ->
  muxSim <$> (g s) <*> (f0' <$> g d0) <*> (f1' <$> g d1) where
  f0' = xorB f0
  f1' = xorB f1
simDff fw = \w d ->
  GateEval $ \g p -> muxSim <$> (fw' <$> g w) <$$> p <*> (g d) where
  fw' = xorB fw
simDffZ fw fz zs = \w z d -> GateEval $ \g p ->
  fmap zs' $ dffZSim <$> (fz' <$> g z) <*> (fw' <$> g w) <$$>
  (zs' p) <*> (zs' <$> g d) where
  fw' = xorB fw
  fz' = xorB fz
  zs' = xorB zs
simSR fs fr fq = \s r -> GateEval $ \g p ->
  fmap fq' $ srSim <$> (fr' <$> g r) <*> (fs' <$> g s) <$$> p where
  fs' = xorB fs
  fr' = xorB fr
  fq' = xorB fq

which :: a -> a -> Bool -> a
which a _ True = a
which _ a False = a

orSim x@(Just True) = const x
orSim (Just False) = id
orSim Nothing = maybe Nothing (which (Just True) Nothing)
muxSim (Just True) = const id
muxSim (Just False) = const
muxSim Nothing = \a b -> if a == b then a else Nothing
dffZSim (Just True) = muxSim
dffZSim (Just False) = const $ const $ const $ Just False
dffZSim Nothing = ((maybe Nothing (which Nothing (Just False)) .) .) . muxSim
srSim (Just True) = const $ const $ Just False
srSim (Just False) = orSim
srSim Nothing = (maybe Nothing (which Nothing (Just False)) .) . orSim

within :: (Ord i) => i -> (i, i) -> Bool
within i (a, b) = a <= i && i <= b

extractOutputsInt :: Map Text (UArray Int Bool) -> SNL -> Map Text [Maybe Bool]
simInt :: Map Text (UArray Int Bool) -> SNL -> SNL

extractOutputsInt m (SNL _ r k o _ _) =
  fmap (uncurry $ uncurry $ maybe looka lookb) <$> o where
  looka :: Int -> Bool -> Maybe Bool
  lookb :: Text -> Int -> Bool -> Maybe Bool
  looka i f = if i `within` bounds k then
                if k ! i then Just $ f /= (r ! i) else Nothing
              else Nothing
  lookb t i f = (\l -> if i `within` bounds l then
                  Just $ f /= (l ! i) else Nothing) =<< M.lookup t m

simulate m nl = (,) <*> extractOutputsInt m' $ simInt m' nl where
  m' = fixupB <$> m

look :: Map Text (UArray Int Bool) -> STUArray s Int Bool ->
        STUArray s Int Bool -> SR -> ST s (Maybe Bool)
look m _ _ (Just n, i) = return $
  (\l -> if i `within` bounds l then Just $ l ! i else Nothing) =<< (m M.!? n)

look _ r k (Nothing, i) = do
  x <- readArray k i
  if x then Just <$> readArray r i else return Nothing

simStuff :: Array Int (Either SR GateEval) -> (SR -> ST s (Maybe Bool)) ->
            Array Int (IntSet, IntSet) -> STUArray s Int Bool ->
            STUArray s Int Bool -> (IntSet, IntSet) -> ST s ()

simStep :: Either SR GateEval -> (SR -> ST s (Maybe Bool)) ->
           Maybe Bool -> ST s (Maybe Bool)
simStep = (const .) . flip id ||| getGE

simInt m (SNL g r k o u i) = SNL g r' k' o u i where
  (r', k') = runST $ do
    resul <- thaw r
    known <- thaw k
    simStuff g (look m resul known) u resul known i
    res <- unsafeFreeze resul
    knn <- unsafeFreeze known
    return (res, knn)

simStuff g l u r k = simA where
  simA (si, sl) = if IS.null si
                  then if IS.null sl then return ()
                       else simB sl (IS.empty, IS.empty)
                  else do
    let (i, si') = IS.deleteFindMin si
    cur <- l (Nothing, i)
    nex <- simStep (g ! i) l cur
    if cur == nex then simA (si', sl) else do
      when (isJust nex) $ writeArray r i $ fromJust nex
      writeArray k i $ isJust nex
      let (sie, sle) = u ! i
      sl `seq` simA (si' <> sie, sl <> sle)
  simB s (si, sl) = if IS.null s then simA (si, sl) else do
    let (i, s') = IS.deleteFindMin s
    cur <- l (Nothing, i)
    nex <- simStep (g ! i) l cur
    if cur == nex then simB s' (si, sl) else do
      when (isJust nex) $ writeArray r i $ fromJust nex
      writeArray k i $ isJust nex
      let (sie, sle) = u ! i
      si `seq` sl `seq` simB s' (si <> sie, sl <> sle)
