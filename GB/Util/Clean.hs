{-# LANGUAGE TupleSections, PatternSynonyms, FlexibleContexts, LambdaCase #-}


-- TODO: Rework to just be cleaning.
module GB.Util.Clean (cleanNL, cleanNLWith, NL(..)) where

import GB.Util.NetList

import Control.Category hiding (id, (.))
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
import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import Data.Tuple
import Control.Monad.Writer.Strict
import Control.Monad.Trans.Maybe
import Control.Monad.Identity

type PGate = Either SR Gate

-- Idea! Change PGate to Gate whenever possible, so only handleNots need know.

newtype DiffSet = DiffSet { getDiffSet :: IntSet }
instance Semigroup DiffSet where
  (<>) = (DiffSet .) . on ($+$) getDiffSet

instance Monoid DiffSet where
  mempty = DiffSet IS.empty

newtype NL g = NL { getNL :: ([(String, [(SR, Bool)])], IntMap g) }
  deriving (Show)

cleanNLWith :: [NL Gate -> Maybe (NL Gate)] -> NetList -> NetList
cleanNL :: NetList -> NetList

onlyChanges  :: IntMap PGate -> IntMap Gate -> IntMap PGate

cleanNL = cleanNLWith []

iterateUntilDoneMonad :: (Monad m) => (a -> MaybeT m a) -> a -> m a
iterateUntilDoneMonad f = fix $ join . ((<=< runMaybeT . f) .) .
                          (. return) . flip maybe

iterateUntilDone :: (a -> Maybe a) -> a -> a
iterateUntilDone =
  (runIdentity .) . iterateUntilDoneMonad . ((MaybeT . Identity) .)
                       
iterateOnceTillDone :: (a -> Maybe a) -> a -> Maybe a
iterateOnceTillDone = (.) <*> iterateUntilDone

if' :: Bool -> a -> a -> a
if' True = const
if' False = const id

which :: a -> a -> Bool -> a
which = flip . flip if'

strategies :: [NL PGate -> Maybe (NL PGate)]

dePGate = Not ||| id

handleThingy :: (NL Gate -> Maybe (NL Gate)) -> NL PGate -> Maybe (NL PGate)

handleThingy f (NL (o, i)) = do
  NL (o', i') <- f $ NL (o, dePGate <$> i)
  return $ NL (o', onlyChanges i i')

cleanNLWith x (NetList i o g s n) = compress $ NetList i o' g' s n where
  (o', g') = second (fmap dePGate) $ getNL $ cleanUp x $ NL (o, Right <$> g)

cleanUp x =
  iterateUntilDone (msum . flip (map . (&))
                    (strategies ++ fmap handleThingy x))

strategies =
  fmap handleThingy [simplifyGates, removeUnused,
                     mergeCommonNots, mergeConsts,
                     mergeNotsPre] ++
  [removeNotsAndWires, handleThingy canonConsts]

onlyChanges = IM.mergeWithKey (const hChanges) (const IM.empty) (fmap Right)
  where hChanges (Left _) (Not x) = Just $ Left x
        hChanges _ g = Just $ Right g

parallelSimp :: (Traversable t) => (a -> Maybe a) -> t a -> Maybe (t a)
maybeToBool :: b -> Maybe b -> (Any, b)
maybeToBool = flip maybe (Any True,) . pure
boolToMaybe :: (Any, b) -> Maybe b
boolToMaybe = uncurry $ which Just (const Nothing) . getAny

parallelSimp = (boolToMaybe .) . traverse . ap maybeToBool

pSimp :: (SR -> Maybe SR) -> NL Gate -> Maybe (NL Gate)

pSimp = \f -> fmap NL . boolToMaybe . uncurry (liftA2 (,)) .
  (foo f *** traverse (maybeToBool <*> replaceWires f)) . getNL where
  foo f = fmap (getCompose . getCompose) .
          traverse (g . first (maybeToBool <*> f)) .
          Compose . Compose
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
  gateSimp (GDff fw w d) | w == d = Just $ GConst $ not fw
  gateSimp (GDffZ fw fz zs w z d)
    | w == z && fw == fz =
      Just $ (if zs then if' fz GOr (flip GImpl) else if' fz GNimpl GAnd) z d
    | z == d && fz /= zs = Just $ GConst zs
  gateSimp (GSR fs fr fq s r)
    | s == r = Just $ if fs == fr then GConst fq else GUnop (fs /= fq) s
  gateSimp _ = Nothing

pWires :: PGate -> [SR]
pWires = either (:[]) wires

removeUnused (NL (out, int)) =
  if IM.null removed then Nothing else Just $ NL (out, retained) where
    retained = IM.restrictKeys int used
    removed = IM.withoutKeys int used
    used = closure (foldMap justNumbers . wires) int $
           foldMap (foldMap (justNumbers . fst) . snd) out
           
closure :: (a -> IntSet) -> IntMap a -> IntSet -> IntSet
closure f g = closure' . (, IS.empty) where
  closure' (gray, black) = let gray' = gray IS.\\ black in
    if IS.null gray'
    then black
    else closure' (foldMap f (IM.restrictKeys g gray'), gray <> black)
  
replaceWires :: (SR -> Maybe SR) -> Gate -> Maybe Gate
replaceWires = (boolToMaybe .) . replWires . ap maybeToBool where
  replWires f (GUnop t x) = GUnop t <$> f x
  replWires f (GBinop a b x y) = GBinop a b <$> f x <*> f y
  replWires f (GMux a b s x y) = GMux a b <$> f s <*> f x <*> f y
  replWires f (GDff fw w d) = GDff fw <$> f w <*> f d
  replWires f (GDffZ fw fz zs w z d) =
    GDffZ fw fz zs <$> f w <*> f z <*> f d
  replWires f (GSR fs fr fq s r) = GSR fs fr fq <$> f s <*> f r
  replWires _ x = pure x

justNumbers :: SR -> IS.IntSet
justNumbers = uncurry $ maybe IS.singleton (const $ const IS.empty)

justUnops :: Gate -> Maybe (SR, Bool)
justUnops (GUnop f x) = Just (x, f)
justUnops _ = Nothing

gatherUnopsInt :: IntMap Gate -> IntMap (SR, Bool)
gatherUnopsInt = IM.mapMaybe justUnops

gatherNotsInt :: IntMap Gate -> IntMap SR
gatherNotsInt = IM.mapMaybe $ \case
  Not x -> Just x
  _ -> Nothing

keepOnlyNots :: IntMap (SR, Bool) -> IntMap SR
keepOnlyNots = IM.mapMaybe (uncurry $ flip $ which Just (const Nothing))

flipNots :: IntMap SR -> M.Map SR Int
flipNots = IM.foldMapWithKey (flip M.singleton)

canonMap :: (Ord b) => IntMap a -> M.Map b Int -> b -> Maybe a
canonMap o i = (o IM.!?) <=< (i M.!?)

canonicalizeNot :: IntMap SR -> M.Map SR Int -> SR -> Maybe SR
canonicalizeNot c f a = do ac <- canonMap c f a
                           when (a == ac) Nothing
                           Just ac

mergeCommonNots x@(NL (_, int)) =
  pSimp (canonicalizeNot c f) x where
  c = gatherNotsInt int
  f = flipNots c

newtype IntMSet = IntMSet { getIntMSet :: IntMap Int }
instance Semigroup IntMSet where
  (<>) = (IntMSet .) . on (IM.unionWith (+)) getIntMSet
instance Monoid IntMSet where
  mempty = IntMSet IM.empty

countsG :: (Foldable f, Foldable g) => (a -> g b) -> (b -> SR) -> f a ->
          IntMap Int
countsG f g = getIntMSet . foldMap (foldMap (IntMSet . cnt . g) . f) where
  cnt = uncurry $ maybe (flip IM.singleton 1) (const $ const IM.empty)

counts :: NL Gate -> IntMap Int
counts = uncurry (IM.unionWith (+)) . (countsG snd fst *** countsG wires id) .
         getNL

inOnce = ((== 1) .) . flip (IM.findWithDefault 0)
isInt = uncurry $ maybe Just (const $ const Nothing)
isntDelay m i = case m IM.! i of
  GDff _ _ _ -> False
  GDffZ _ _ _ _ _ _ -> False
  _ -> True

findMergeableNots :: NL Gate -> IntMap Int
findMergeableNots x@(NL (_, int)) =
  IM.filter (liftA2 (&&) (inOnce $ counts x) $ isntDelay int) $ 
  IM.mapMaybe isInt $ keepOnlyNots $ gatherUnopsInt int

($+$) :: IntSet -> IntSet -> IntSet
infixl 9 $+$
k $+$ v = (k IS.\\ v) `IS.union` (v IS.\\ k)
  
notOutsAndIns :: IntMap Int -> IS.IntSet
notOutsAndIns m = k $+$ v where
  k = IM.keysSet m
  v = IS.fromList $ IM.elems m
  
mergeNotAfterInto :: Gate -> Gate
mergeNotAfterInto = \case 
  GConst b -> GConst (not b)
  GUnop f x -> GUnop (not f) x
  GBinop f o x y -> GBinop (not f) o x y
  GMux f0 f1 s d0 d1 -> on GMux not f0 f1 s d0 d1
  GSR fs fr fq s r -> GSR fs fr (not fq) s r
  _ -> error "shouldn't happen"

mergeNotsPre x =
  if IS.null mns then Nothing
  else Just $ NL $
  second (flip (IS.foldr' $ IM.adjust mergeNotAfterInto) mns) $ getNL x where
    mns = notOutsAndIns $ findMergeableNots x

checkRes :: IntMap (SR, Bool) -> SR -> Maybe (SR, Bool)
checkRes m = uncurry $ maybe (m IM.!?) (const $ const Nothing)

on3 :: (b -> b -> b -> c) -> (a -> b) -> (a -> a -> a -> c)
on3 = join . ((flip . (on .)) .) . (.)

handleNotsAndWires :: (SR -> Maybe (SR, Bool)) -> Gate -> Maybe (Gate, Bool)
handleNotsAndWires = (boolToMaybe .) . hnw . ap (maybeToBool . (, False)) where
  hnw f (GUnop b x) = (, False) . hmUnop b <$> f x
  hnw f (GBinop b o x y) = fmap (, False) $ hmBinop b o <$> f x <*> f y
  hnw f (GMux f0 f1 s d0 d1) = fmap (, False) $
                                hmMux f0 f1 <$> f s <*> f d0 <*> f d1
  hnw f (GDff fw w d) = hmDff fw <$> f w <*> f d
  hnw f (GDffZ fw fz zs w z d) =
    hmDffZ fw fz zs <$> f w <*> f z <*> f d
  hnw f (GSR fs fr fq s r) = fmap (, False) $ hmSR fs fr fq <$> f s <*> f r
  hnw _ x = pure (x, False)
  hmDff fw (w, fw') (d, fd') = (GDff (fw /= fw') w d, fd')
  hmDffZ fw fz zs (w, fw') (z, fz') (d, fd') =
    (GDffZ (fw /= fw') (fz /= fz') (zs /= fd') w z d, fd')
  hmUnop = uncurry . flip . (GUnop .) . (/=)
  hmBinop fo BAnd (l, fl) (r, fr) = hmAnd fl fr fo l r
  hmBinop fo BOr (l, fl) (r, fr) = (hmAnd `on3` not) fl fr fo l r
  hmBinop fo BImpl (l, fl) (r, fr) = (hmAnd fl `on` not) fr fo l r
  hmBinop fo BXor (l, fl) (r, fr) = GBinop (fo /= (fl /= fr)) BXor l r
  hmAnd False False = flip GBinop BAnd
  hmAnd True True = flip GBinop BOr . not
  hmAnd False True = flip GBinop BImpl . not
  hmAnd True False = flip . flip GBinop BImpl . not
  hmMux f0 f1 (s, True) = flip $ hmMux1 f1 f0 s
  hmMux f0 f1 (s, False) = hmMux1 f0 f1 s
  hmMux1 f0 f1 s (d0, f0') (d1, f1') = GMux (f0 /= f0') (f1 /= f1') s d0 d1
  hmSR fs fr fq (s, fs') (r, fr') = GSR (fs /= fs') (fr /= fr') fq s r

handleNotOrWire :: (SR -> Maybe (SR, Bool)) -> PGate -> Maybe (PGate, Bool)
handleNotOrWire = either <$> hnw <*>
  (fmap (first Right) .) . handleNotsAndWires where
  hnw = (.) ((, False) . uncurry (flip $ which (Right . Id) Left) <$>)

fixNotsWires :: IntSet -> IntMap PGate -> Writer DiffSet (IntMap PGate)

removeNotsAndWires = \x@(NL (_, int)) -> do
  let unops = IM.mapMaybe (const Nothing ||| justUnops) int
  unless (runout $ IM.mapMaybe sources unops) Nothing
  (errs, NL (out, int')) <- rnw (checkRes unops) x
  let (int'', DiffSet errs') = runWriter $ do
        tell $ DiffSet errs
        fixNotsWires errs int'
  return $ NL (flipOuts errs' out, int'')
  where
    firstPart :: (SR -> Maybe (SR, Bool)) -> Int -> PGate ->
                 (Any, (IntSet, PGate))
    firstPart f i g = maybe (pure $ pure g)
                      ((Any True,) . swap .
                       second (which (IS.singleton i) IS.empty)) $
                      handleNotOrWire f g
    rnw :: (SR -> Maybe (SR, Bool)) -> NL PGate -> Maybe (IntSet, NL PGate)
    rnw f = boolToMaybe . getCompose . fmap NL . uncurry (liftA2 (,)) .
            (rnw' f *** IM.traverseWithKey ((Compose .) . firstPart f)) .
            getNL
    rnw' :: (SR -> Maybe (SR, Bool)) -> [(String, [(SR, Bool)])] ->
            Compose ((,)Any) ((,)IntSet) [(String, [(SR, Bool)])]
    rnw' f = fmap ddComp .
      traverse (Compose . fmap (IS.empty,) . ap maybeToBool (hnw f)) . dComp
    dComp = Compose . Compose
    ddComp = getCompose . getCompose
    hnw :: (SR -> Maybe (SR, Bool)) -> (SR, Bool) -> Maybe (SR, Bool)
    hnw = uncurry . flip . (. (fmap . second . (/=))) . (>>>)
    sources = uncurry (maybe Just (const $ const Nothing)) . fst
    runout s
      | IM.null s = True
      | IM.size s /= IM.size s' = runout s'
      | otherwise = False
      where s' = squareMap s
    squareMap = (IM.!?) >>= IM.mapMaybe
    flipOuts :: IntSet -> [(String, [(SR, Bool)])] ->
                [(String, [(SR, Bool)])]
    flipOuts = (ddComp .) . (. dComp) . fmap . flopOut
    flopOut :: IntSet -> (SR, Bool) -> (SR, Bool)
    flopOut = uncurry . liftA2 (.) (,) . uncurry .
      flip maybe (const $ const id) .
      ((/=) .) . flip IS.member
               
fixNotsWires = curry $ fmap snd . iterateUntilDoneMonad fixFirstNot

hitsSelf :: Int -> IntSet -> IntMap PGate -> Bool
deNot :: Int -> IntMap PGate -> IntMap PGate
pushNot :: Int -> IntMap PGate -> MaybeT (Writer DiffSet) (IntMap PGate)

pullIsh :: IntMap PGate -> Int -> Maybe Int

hitsSelf x s m = hitsSelf' x where
  hitsSelf' = maybe False hitsSelf'' . pullIsh m
  hitsSelf'' y
    | x == y = True
    | y `IS.member` s = False
    | otherwise = hitsSelf' y
pullIsh = (pullIsh' <=<) . (IM.!?) where
  pullIsh' = const Nothing ||| pullIsh''
  pullIsh'' (GDff _ _ x) = pullIsh''' x
  pullIsh'' (GDffZ _ _ _ _ _ x) = pullIsh''' x
  pullIsh'' _ = Nothing
  pullIsh''' = uncurry $ maybe Just (const $ const Nothing)

adjFF :: Int -> Maybe PGate -> (SR, Maybe PGate)
adjFF = (second Just .) . (. fromJust) . adjFF' where
  adjFF' i (Right x) = Right <$> adjFF'' i x
  adjFF'' i (GDff fw w d) = (d, GDff fw w (Nothing, i))
  adjFF'' i (GDffZ fw fz zs w z d) = (d, GDffZ fw fz (not zs) w z (Nothing, i))

deNot i m = IM.insert pos (Left target) m' where
  pos = fst (IM.findMin m) - 1
  (target, m') = IM.alterF (adjFF pos) i m

match :: Int -> SR -> Bool
match a (Nothing, b) = a == b
match _ _ = False
  
pushNot' :: Int -> Int -> PGate -> (IntSet, PGate)
pushNot' = \i j -> pushNotL i j &&& pushNotR (match i) where
  pushNotL i j (Right (GDff _ _ (Nothing, x))) | x == i = IS.singleton j
  pushNotL i j (Right (GDffZ _ _ _ _ _ (Nothing, x))) | x == i = IS.singleton j
  pushNotL _ _ _ = IS.empty
  pushNotR f x@(Left y) = if f y then Right (Id y) else x
  pushNotR f (Right x) = Right $ pushNot'' f x

pushNot'' :: (SR -> Bool) -> Gate -> Gate
pushNot'' g (GUnop f x) | g x = GUnop (not f) x
pushNot'' g (GBinop f o l r)
  | g l || g r = pushNotBO o f (g l) (g r) l r
pushNot'' g (GMux f0 f1 s d0 d1)
  | g s || g d0 || g d1 = GMux (f0' /= g d0') (f1' /= g d1') s d0' d1'
  where ((f0', d0'), (f1', d1')) = if g s
                                   then ((f1, d1), (f0, d0))
                                   else ((f0, d0), (f1, d1))
pushNot'' g (GDff fw w d) | g w = GDff (not fw) w d
pushNot'' g (GDffZ fw fz zs w z d)
  | g w || g z || g d = GDffZ (fw /= g w) (fz /= g z) (zs /= g d) w z d
pushNot'' _ x = x

pushNotBO :: Binop -> Bool -> Bool -> Bool -> SR -> SR -> Gate
pushNotBO BXor = (((flip GBinop BXor .) . (/=)) .) . (/=)
pushNotBO BAnd = pushNotOr `on3` not
pushNotBO BOr = pushNotOr
pushNotBO BImpl = pushNotOr . not

pushNotOr :: Bool -> Bool -> Bool -> SR -> SR -> Gate
pushNotOr = which (which (which GAnd  GNimpl) (which (flip GNimpl) GNor))
                  (which (which GNand GImpl)  (which (flip GImpl)  GOr))
pushNot = ((MaybeT . writer . (Just *** DiffSet) . swap) .) .
          IM.traverseWithKey . pushNot'

fixFirstNot :: (IntSet, IntMap PGate) ->
               MaybeT (Writer DiffSet) (IntSet, IntMap PGate)
fixFirstNot (errs, int) = do
  (lowNot, errs') <- MaybeT $ return $ IS.minView errs
  if hitsSelf lowNot errs' int then
    return (errs', deNot lowNot int)
    else fmap swap $ listens ((errs' $+$) . getDiffSet) $ pushNot lowNot int
    
gatherConsts :: IntMap Gate -> IntMap Bool
gatherConsts = IM.mapMaybe justConsts where
  justConsts (GConst b) = Just b
  justConsts _ = Nothing

mapBoth = join (***)

flipConsts :: IntMap Bool -> (Int, Int)
flipConsts = mapBoth (maybe 0 fst . IM.lookupMin) .
  (IM.filter id &&& IM.filter not)

canonicalizeConst :: IntMap Bool -> (Int, Int) -> SR -> Maybe SR
canonicalizeConst = ((uncurry . flip maybe (const $ const Nothing)) .) .
                    (. (fmap . uncurry (on which (Nothing,)))) .
                    (>>>) . (IM.!?)

canonConsts x@(NL (_, int)) =
  pSimp (canonicalizeConst c f) x where
  c = gatherConsts int
  f = flipConsts c

flipEither :: Either a b -> Either b a
flipEither = Right ||| Left

correctConsts :: (Int -> Maybe Bool) -> Gate -> Maybe Gate
correctConsts = (boolToMaybe .) . crcns . foo where
  foo = liftA2 maybeToBool Right . (fmap Left .) . uncurry .
    flip maybe (const $ const Nothing)
  crcns :: (SR -> (Any, Either Bool SR)) -> Gate -> (Any, Gate)
  crcns f (GUnop b x) = huo b <$> f x
  crcns f (GBinop b o x y) = hbo o b <$> f x <*> f y
  crcns f (GMux f0 f1 s d0 d1) = hmx f0 f1 <$> f s <*> f d0 <*> f d1
  crcns f (GDff fw w d) = hdff fw <$> f w <*> f d
  crcns f (GDffZ fw fz zs w z d) =
    hdfz fw fz zs <$> f w <*> f z <*> f d
  crcns f (GSR fs fr fq s r) = hsr fs fr fq <$> f s <*> f r
  crcns _ x = pure x
  huo b = either (GConst . (b /=)) (GUnop b)
  hbo BAnd = hor True True . not
  hbo BOr = hor False False
  hbo BXor = hxor
  hbo BImpl = hor True False
  hxor f = huo . (f /=) ||| either <$> flip (GUnop . (f /=)) <*> GBinop f BXor
  hor fl fr f = which (const $ GConst $ not f) (huo $ fr /= f) . (fl /=) |||
                hora fl fr f
  hora fl fr f l = which (GConst $ not f) (GUnop (fl /= f) l) . (fr /=) |||
                   horb fl fr f l
  horb False False = flip GBinop BOr
  horb True False = flip GBinop BImpl
  horb False True = flip . flip GBinop BImpl
  horb True True = flip GBinop BAnd . not
  hmx f0 f1 = which (const $ huo f1) (const . huo f0) ||| hmx' f0 f1
  hmx' f0 f1 s = hmx'' f1 True s . (f0 ==) |||
                 either <$> flip (hmx'' f0 False s . (f1 ==)) . Right <*>
                 GMux f0 f1 s
  hmx'' = (flip .) . hmxa
  hmxa fi wh a = flip (hor wh a (fi /= a)) . Right
  hdff fw = either (which (const $ GConst True) (huo False) . (fw ==))
               (either GConst . GDff fw)
  -- If never set, just call it true.
  hdfz fw fz zs w (Left z) d = if z == fz then GConst zs else hdff fw w d
  hdfz fw fz zs (Left w) z d = if w /= fw
                               then join (hor $ not fz) (not zs) z d
                               else GConst zs
  hdfz fw fz zs w z (Left d) = if d == zs
                               then GConst zs
                               else hsr fw fz zs w z
  hdfz fw fz zs (Right w) (Right z) (Right d) = GDffZ fw fz zs w z d
  hsr fs fr fq s (Left r) = GConst $ (r /= fr) == fq
  hsr fs fr fq (Left s) r = if s /= fs
                            then huo (fr == fq) r
                            else GConst fq
  hsr fs fr fq (Right s) (Right r) = GSR fs fr fq s r
                     
mergeConsts x@(NL (_, int)) =
  parallelSimp (correctConsts $ (IM.!?) $ gatherConsts int) x
