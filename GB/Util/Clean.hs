{-# LANGUAGE DeriveGeneric, DeriveAnyClass, TypeFamilies, TupleSections #-}
{-# LANGUAGE PatternSynonyms, FlexibleContexts #-}


-- TODO: Rework to just be cleaning.
module GB.Util.Clean (cleanNL, cleanNLWith, NL(..)) where

import GB.Util.NetList

import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import Data.Functor
import Control.Applicative
import Data.Monoid hiding (Product, Sum)
import Data.Traversable
import Data.Foldable
import Control.Arrow
import Control.Monad
import Data.Function
import Data.Array
import Data.List
import Data.Functor.Compose
import Data.Functor.Product
import Data.Maybe hiding (mapMaybe)
import qualified Data.Set as S
import qualified Data.IntSet as IS
import GHC.Generics
import Data.Tuple

newtype NL g = NL { getNL :: ([(String, [(SR, Bool)])], IntMap g) }
  deriving (Generic, NFData, Show)

cleanNLWith :: [NL Gate -> Maybe (NL Gate)] -> NetList -> NetList
cleanNL :: NetList -> NetList

cleanNL = cleanNLWith []

iterateUntilDone :: (a -> Maybe a) -> a -> a
iterateUntilDone = fix . (. flip maybe) . flip ap

iterateNUntilDone :: Int -> (a -> Maybe a) -> a -> a
iterList :: (a -> Maybe a) -> a -> [a]
iterList = (catMaybes . takeWhile isJust . iterate) . (=<<)

iterateNUntilDone i f = last . take i . iterList f

iterateOnceTillDone :: (a -> Maybe a) -> a -> Maybe a
iterateOnceTillDone = (.) <*> iterateUntilDone

which :: a -> a -> Bool -> a
which = flip . flip if'

strategies :: [NL Gate -> Maybe (NL Gate)]

secondaryStrategies :: [NL Gate -> Maybe (NL Gate)]

handleOutputs :: NL Gate -> NL Gate

cleanNLWith x (NetList i o g s n) = compress $ NetList i o' g' s n where
  (o', g') = getNL $ cleanUp x $ NL (o, g)

cleanUp x =
  iterateUntilDone (force . msum . flip (map . (&))
                    (secondaryStrategies ++ x)) .
  iterateNUntilDone 2000 (force . msum . flip (map . (&)) (strategies ++ x))

strategies = [simplifyGates, removeUnused, mergeCommonNots, mergeConsts,
              mergeNotsPre, removeNotsAndWires]

secondaryStrategies = 

parallelSimp :: (Traversable t) => (a -> Maybe a) -> t a -> Maybe (t a)
maybeToBool :: b -> Maybe b -> (Any, b)
maybeToBool = flip maybe (Any True,) . pure
boolToMaybe :: (Any, b) -> Maybe b
boolToMaybe = uncurry $ which Just (const Nothing) . getAny

parallelSimp = (boolToMaybe .) . traverse . ap maybeToBool

pSimp :: (SR -> Maybe SR) -> NL Gate -> Maybe (NL Gate)

pSimp f = fmap NL . boolToMaybe . uncurry (liftA2 (,)) .
  (foo *** traverse (maybeToBool <*> f)) . getNL where
  foo = fmap (getCompose . getCompose . getCompose) .
        traverse (g . ap maybeToBool f) . Compose . Compose . Compose
  g = uncurry $ uncurry $ (. (,)) . (.) . (,)
  
whatsitToNL = NL . getCompose
nlToWhatsit = Compose . getNL

instance Functor NL where
  fmap f = whatsitToNL . fmap f . nlToWhatsit
instance Foldable NL where
  foldMap f = foldMap f . nlToWhatsit
  foldr f a = foldr f a . nlToWhatsit
instance Traversable NL where
  traverse f = fmap whatsitToNL . traverse f . nlToWhatsit

simplifyGates = parallelSimp gateSimp where
  gateSimp (GBinop f op x y) | x == y = Just $ case op of
                                 BAnd -> GUnop f x
                                 BOr -> GUnop f x
                                 BXor -> GConst f
                                 BImpl -> GConst (not f)
  gateSimp (GMux f0 f1 s d0 d1)
    | d0 == d1 = Just $ if f0 == f1
                        then GUnop f0 d0
                        else if s == d0
                             then GConst f0
                             else GBinop f0 BXor s d0
    | s == d0 = Just $ GBinop f1 (if f0 == f1 then BAnd else BImpl) d0 d1
    | s == d1 = Just $ GBinop f1 (if f0 == f1 then BOr else BImpl) d0 d1
  gateSimp _ = Nothing

removeUnused (NL (out, int)) =
  if IM.null removed then Nothing else Just $ NL (out, retained) where
    retained = IM.restrictKeys int used
    removed = IM.withoutKeys int used
    used = repeatUntilNoChange
      (ap (<>) foldMap (foldMap justNumbers . wires) . IM.restrictKeys int) $
      foldMap (foldMap (justNumbers . fst) . snd) out


repeatUntilNoChange :: Eq a => (a -> a) -> a -> a
repeatUntilNoChange f x = if x == x' then x else repeatUntilNoChange f x' where
  x' = f x
  
replaceWires :: (SR -> Maybe SR) -> Gate -> Maybe Gate
replaceWires = (boolToMaybe .) . replWires . ap maybeToBool where
  replWires f (GUnop t x) = GUnop t <$> f x
  replWires f (GBinop a b x y) = GBinop a b <$> f x <*> f y
  replWires f (GMux a b s x y) = GMux a b <$> f s <*> f x <*> f y
  replWires f (GDff fw w d) = GDff fw <$> f w <*> f d
  replWires f (GDffZ fw fz fd w z d) =
    GDffZ fw fz fd <$> f w <*> f z <*> f d
  replWires _ x = pure x

justNumbers :: SR -> IS.IntSet
justNumbers = uncurry $ maybe IS.singleton (const $ const IS.empty)

justUnops :: Gate -> Maybe (SR, Bool)
justUnops (GUnop f x) = Just (x, f)
justUnops _ = Nothing

gatherUnopsInt :: IntMap Gate -> IntMap (SR, Bool)
gatherUnopsInt = IM.mapMaybe justUnops

keepOnlyNots :: IntMap (SR, Bool) -> IntMap SR
keepOnlyNots = IM.mapMaybe (uncurry $ flip $ which Just (const Nothing))

flipNots :: IntMap SR -> M.Map SR Int
flipNots = IM.foldMapWithKey $ flip M.singleton

canonMap :: (Ord b) => IntMap a -> M.Map b Int -> b -> Maybe a
canonMap o i = (o IM.!?) <=< (i M.!?)

canonicalizeNot :: IntMap SR -> M.Map SR Int -> SR -> Maybe SR
canonicalizeNot c f a = do ac <- canonMap c f a
                           when (a == ac) Nothing
                           Just ac

mergeCommonNots x@(NL (_, int)) =
  pSimp (replaceWires $ canonicalizeNot c f) x where
  c = keepOnlyNots $ gatherUnopsInt int
  f = flipNots c

counts :: Foldable t => t Gate -> IntMap Int
counts = flip appEndo IM.empty . foldMap (foldMap (Endo . cnt) . wires) where
  cnt = uncurry $ maybe (flip (IM.insertWith (+)) 1) (const $ const id)

countsH :: [(String, [(SR, Bool)])] -> IntMap Int
countsH = flip appEndo IM.empty

inOnce = ((== 1) .) . flip (IM.findWithDefault 0)
isInt = uncurry $ maybe Just (const $ constNothing)
isntDelay m i = case m IM.! i of
  GDff _ _ _ -> False
  GDffZ _ _ _ _ _ _ -> False
  _ -> True

findMergeableNots :: NL Gate -> IntMap Int
findMergeableNots x@(NL (_, int)) =
  IM.filter (liftA2 (&&) (inOnce $ counts x) $ isntDelay int) $ 
  IM.mapMaybe isInt $ keepOnlyNots $ gatherUnopsInt int
  
notOutsAndIns :: IntMap Int -> IS.IntSet
notOutsAndIns m = (k IS.\\ v) `IS.union` (v IS.\\ k) where
  k = IM.keysSet m
  v = IS.fromList $ IM.elems m
  
mergeNotAfterInto :: Gate -> Gate
mergeNotAfterInto g = case g of
  GConst b -> GConst (not b)
  GUnop f x -> GUnop (not f) x
  GBinop f o x y -> GBinop (not f) o x y
  GMux f0 f1 s d0 d1 -> on GMux not f0 f1 s d0 d1
  _ -> error "shouldn't happen"

mergeNotsPre x =
  if IS.null mns then Nothing
  else Just $ NL $ second (flip (IS.foldr $ IM.adjust mergeNotAfterInto) mns) $
       getNL x where
    mns = notOutsAndIns $ findMergeableNots x

checkRes :: IntMap (SR, Bool) -> SR -> Maybe (SR, Bool)
checkRes m x = do when (isJust $ fst x) Nothing
                  v <- m IM.!? (snd x)
                  when (fst v == x) $
                    error "Setting x to either x or !x always for some x."
                  return v

-- TODO: Fix so as to avoid issues with loops.

handleNotsAndWires :: (SR -> Maybe (SR, Bool)) -> Gate -> Maybe (Gate, Bool)
handleNotsAndWires = (boolToMaybe .) . hnw . ap (maybeToBool . (, False)) where
  hnw f (GUnop b x) = (, False) <$> hmUnop b <$> f x
  hnw f (GBinop b o x y) = fmap (, False) $ hmBinop b o <$> f x <*> f y
  hnw f (GMux f0 f1 s d0 d1) = fmap (, False) $
                               hmMux f0 f1 <$> f s <*> f d0 <*> f d1
  hnw f (GDff fw fd w d) = hmDff fw fd <$> f w <*> f d
  hnw f (GDffZ fq fw fz fd w z d) =
    hmDffZ fq fw fz fd <$> f w <*> f z <*> f d
  hnw _ x = pure x

removeNotsAndWires x@(NL (_, int)) =
  parallelSimp (handleNotsAndWires $ checkRes $ gatherUnopsInt int) x

gatherConsts :: IntMap Gate -> IntMap Bool
gatherConsts = IM.mapMaybe justConsts where
  justConsts (GConst b) = Just b
  justConsts _ = Nothing

correctConsts :: (Int -> Maybe Bool) -> Gate -> Maybe Gate
correctConsts = (boolToMaybe .) . crcns . foo where
  foo = liftA2 maybeToBool Right . (fmap Left .) . uncurry .
    flip maybe (const $ const Nothing)
  crcns :: (Bool -> SR) -> (SR -> (Any, Either Bool SR)) -> Gate -> (Any, Gate)
  crcns f (GUnop b x) = huo b <$> f x
  crcns f (GBinop b o x y) = hbo b o <$> f x <*> f y
  crcns f (GMux f0 f1 s d0 d1) = hmx f0 f1 <$> f s <*> f d0 <*> f d1
  crcns f (GDff fw w d) = hdff fw <$> f w <*> f d
  crcns f x@(GDffZ fw fz zs w z d) =
    ((hdfz x fw fz zs =<< f w) =<< f z) =<< f d    
  crcns _ x = pure x
  huo b = either (GConst . (b /=)) (GUnop b)
  hbo b o = either (either <$> (GConst .) . evbo b o <*> lbo b o)
                   (either <$> rbo b o <*> GBinop b o)
  evbo b BAnd = ((b /=) .) . (&&)
  evbo b BOr = ((b /=) .) . (||)
  evbo b BImpl = ((b /=) .) . (<=)
  evbo b BXor = (/=) . (b /=)
  lbo b BAnd = which (GUnop b) (const $ GConst b)
  lbo b BOr = which (const $ GConst $ not b) (GUnop b)
  lbo b BImpl = which (GUnop b) (const $ GConst $ not b)
  lbo b BXor = GUnop . (b /=)
  rbo b BImpl = flip $ which (const $ GConst $ not b) (GUnop $ not b)
  rbo b o = flip $ lbo b o
  hmx f _ (Left False) = const . huo f 
  hmx _ f (Left True) = const $ huo f
  hmx f0 f1 q@(Right s) = either (sth q f1 . (f0 /=))
    (either <$> flip (sthe q f0 . (f1 /=)) <*> GMux f0 f1 s)
  sth q b c = hbo b (if b == c then BAnd else BImpl) q
  sthe q b c = flip (hbo c $ if b /= c then BOr else BImpl) q . Right
  hdff fw = either (which (const $ GConst True) (huo False) . (fw ==))
               (either GConst . GDff fw)
  hdfz x _ _ zs (Right _) (Right _) (Left dv) =
    if dv == zs
    then (Any True, GConst dv)
    else return x
  hdfz _ fw fz zs w z d = hdfz' fw fz zs w z d
  hdfz' fw fz zs =
    either (which ((if' fz id flip) (hbo (not (fz || zs))
                                     (if' (fz /= zs) BImpl $ if' fz BOr BAnd)))
             (const $ const $ GConst zs) . (/= fw)) $ hdfz'' fw fz zs
  hdfz'' fw fz zs w =
    either (which (hdff fw (Right w))
             (const $ GConst zs) . (/= fz)) $ error "Can't happen"
                     
    
mergeConsts x@(NL (_, int)) =
  parallelSimp (correctConsts $ (IM.!?) $ gatherConsts int) x

handleOutputs x@(NL (out, int)) = NL (oFix, iFix) where
  cnts = counts x
  (toRem, Compose (Compose oFix)) = traverse fixGate $
    Compose $ Compose out
  fixGate (GUnop b (Nothing, i))
    | cnts IM.! i == 1 = (IS.singleton i,
                           (if' b mergeNotAfterInto id) $ int IM.! i)
  fixGate x = (IS.empty, x)
  iFix = int `IM.withoutKeys` toRem
