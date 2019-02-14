{-# LANGUAGE TupleSections #-}
module GB.Util.Simulate (SNL, simReadyNL, extractOutputs,
                 simulate, simRep, simulateOutputs, simRepOutputs) where

import qualified Data.IntMap.Lazy as IM
import Data.IntMap.Lazy (IntMap)
import qualified Data.Map.Strict as M
import Data.Map (Map)
import Data.Array.IArray
import Data.Array.Unboxed
import GB.Util.NetList
import Data.Maybe
import Control.Arrow
import Control.DeepSeq
import Data.List (foldl')
import Control.Monad
import Data.Monoid
import Data.Traversable
import Control.Applicative
import Data.Tuple
import Data.Function

type GateEval =
  (SR -> Maybe Bool) -> (SR -> Maybe Bool) -> Maybe Bool -> Maybe Bool

newtype SNL = SNL { getSNL :: (IntMap (GateEval, Maybe Bool),
                               Map String [(Maybe Bool -> Maybe Bool, SR)]) }

instance NFData SNL where
  rnf (SNL (x, y)) = rnf (snd <$> x) `seq` rnf (fmap snd <$> y)

simReadyNL :: NetList -> SNL
extractOutputs :: Map String [Bool] -> SNL -> Map String [Maybe Bool]
extractOutputsInt :: Map String (UArray Int Bool) -> SNL ->
                     Map String [Maybe Bool]
simulate :: SNL -> Map String [Bool] -> [SNL]
simRep :: SNL -> [Map String [Bool]] -> [SNL]
-- gives just the settled versions
simulateOutputs :: SNL -> Map String [Bool] -> [Map String [Maybe Bool]]
simRepOutputs :: SNL -> [Map String [Bool]] -> [Map String [Maybe Bool]]

extractOutputs = extractOutputsInt . calc

calc :: Map String [Bool] -> Map String (UArray Int Bool)
simOne :: Map String (UArray Int Bool) -> SNL -> Maybe SNL
simOneInt :: Map String (UArray Int Bool) -> IntMap (GateEval, Maybe Bool) ->
             Maybe (IntMap (GateEval, Maybe Bool))

simReadyNL (NetList _ o g _ _) = SNL ((, Nothing) . simGate <$> g,
                                      M.fromList $
                                      second (first xorB . swap <$>) <$> o)

sim :: (a -> Map String (UArray Int Bool)) -> SNL -> a -> [SNL]

sim f = flip $ ((fmap fromJust . takeWhile isJust . tail) .) . (. Just) .
        iterate . (=<<) . simOne . f
simulate = sim calc
simInt = sim id

pamf :: (Functor f) => f (a -> b) -> a -> f b
pamf = flip (fmap . flip id)

simOne m = fmap SNL . uncurry (pamf . fmap (,) . simOneInt m) .
           getSNL

simulateOutputs nl m = extractOutputsInt m' <$> simInt nl m' where
  m' = calc m
  
simRepX :: SNL -> [Map String (UArray Int Bool)] -> [SNL]

simRep = (. fmap calc) . simRepX

simRepOutputs nl m = zipWith extractOutputsInt m' $ simRepX nl m' where
  m' = calc <$> m

simRepX _ [] = []
simRepX nl (m:ms) = nl' : simRepX nl' ms where
  nl' = SNL $ first (simAll m) $ getSNL nl

simAll :: Map String (UArray Int Bool) -> IntMap (GateEval, Maybe Bool) ->
          IntMap (GateEval, Maybe Bool)

simAll m nl = maybe nl (simAll m) $ simOneInt m nl

calc = fmap $ \a -> listArray (0, length a - 1) $ reverse a

simGate :: Gate -> GateEval

lookAt :: IntMap (a, Maybe Bool) -> Map String (UArray Int Bool) -> SR ->
          Maybe Bool
indexSafe :: (IArray a e, Ix i) => a i e -> i -> Maybe e
within :: (Ord a) => a -> (a, a) -> Bool

within b (a, c) = a <= b && b <= c

lookAt gs is = uncurry $ maybe (join . fmap snd . (gs IM.!?))
               ((join .) . pamf . fmap indexSafe . (is M.!?))

indexSafe a i = if i `within` bounds a
                then Just $ a ! i
                else Nothing

iterateOnceUntilDone :: (a -> Maybe a) -> (a -> Maybe a)
iterateOnceUntilDone = (.) =<< fmap . fix . flip (ap . flip maybe)
b2m = uncurry $ which Just (const Nothing) . getAny

simOneInt m g = flip iterateOnceUntilDone g $
  b2m . \g' -> traverse (simG (lookAt g m) (lookAt g' m)) g where
  simG f f' (gg, s) = let s' = gg f f' s in (Any $ s /= s', (gg, s'))

simConst :: Bool -> GateEval
simUnop :: Bool -> SR -> GateEval
simBinop :: Binop -> Bool -> SR -> SR -> GateEval
simMux :: Bool -> Bool -> SR -> SR -> SR -> GateEval
simDff :: Bool -> SR -> SR -> GateEval
simDffZ :: Bool -> Bool -> Bool -> SR -> SR -> SR -> GateEval
simSR :: Bool -> Bool -> Bool -> SR -> SR -> GateEval
simDelay :: SR -> GateEval
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
simGate (GDelay x) = simDelay x

xorB :: (Functor f) => Bool -> f Bool -> f Bool
xorB True = fmap not
xorB False = id

simConst = const . const . const . Just
simUnop f x _ g _ = (f /=) <$> g x
simBinop BAnd = simOr True True . not
simBinop BOr = simOr False False
simBinop BImpl = simOr True False
simBinop BXor = simXor
simOr fl fr f = \l r _ g _ -> f' $ orSim (fl' $ g l) (fr' $ g r) where
  f' = xorB f
  fl' = xorB fl
  fr' = xorB fr
simXor f = \l r _ g _ -> (/=) <$> f' (g l) <*> g r where
  f' = xorB f
simMux f0 f1 = \s d0 d1 _ g _ -> muxSim (g s) (f0' $ g d0) (f1' $ g d1) where
  f0' = xorB f0
  f1' = xorB f1
simDff fw = \w d _ g p -> muxSim (fw' $ g w) p (g d) where
  fw' = xorB fw
simDffZ fw fz zs = \w z d _ g p ->
  zs' $ dffZSim (fz' $ g z) (fw' $ g w) (zs' p) (zs' $ g d) where
  fw' = xorB fw
  fz' = xorB fz
  zs' = xorB zs
simSR fs fr fq = \s r _ g p -> fq' $ srSim (fr' $ g r) (fs' $ g s) p where
  fs' = xorB fs
  fr' = xorB fr
  fq' = xorB fq
simDelay x g _ _ = g x

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

extractOutputsInt = (. getSNL) . uncurry . ((fmap . fmap) .) . look where
  look = ((uncurry . flip (.)) .) . flip lookAt
