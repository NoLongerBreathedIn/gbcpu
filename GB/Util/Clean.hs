{-# LANGUAGE DeriveGeneric, DeriveAnyClass, TypeFamilies, TupleSections #-}
{-# LANGUAGE PatternSynonyms, FlexibleContexts #-}
module GB.Util.Clean (NetList, listify, listifyWith, showNL,
                      Gate(..), Binop(..), SR,
                      pattern GAnd, pattern GNand,
                      pattern GOr, pattern GNor,
                      pattern GXor, pattern GIff,
                      pattern GImpl, pattern GNimpl,
                      pattern Mux, pattern MuxNS,
                      pattern MuxSN, pattern MuxN,
                      pattern High, pattern Low,
                      pattern Id, pattern Not,
                      Fixable, SingleVar, ListVar,
                      singleVar, listVar) where
import GB.Util.Base
import GB.Lava.VhdlNew as GBL
import Lava hiding (Generic)
import qualified Lava.Generic as LG
import Control.DeepSeq
import Control.Monad.State
import System.IO.Temp
import System.IO.Unsafe
import System.IO
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
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

data NetList = NetList { name :: String,
                         inputs :: [(String, Int)],
                         outputs :: [(String, [Gate])],
                         gates :: IM.IntMap Gate }
             deriving (Generic, NFData, Show)

newtype SingleVar = Var {unVar :: String}
newtype ListVar = VarList {unVarList :: (String, Int)}

singleVar :: String -> SingleVar
singleVar = Var
listVar :: Int -> String -> ListVar
listVar = flip (curry VarList)

type SR = (Maybe String, Int)

data Binop = BAnd | BOr | BXor | BImpl
           deriving (Eq, Generic, NFData, Show)

data Gate = GConst Bool
          | GUnop Bool SR
          | GBinop Bool Binop SR SR -- negafter
          | GMux Bool Bool SR SR SR -- neg d0, neg d1, s, d0, d1,
          deriving (Eq, Generic, NFData, Show)

pattern GAnd   x y = GBinop False BAnd  x y
pattern GOr    x y = GBinop False BOr   x y
pattern GNand  x y = GBinop True  BAnd  x y
pattern GNor   x y = GBinop True  BOr   x y
pattern GXor   x y = GBinop False BXor  x y
pattern GIff   x y = GBinop True  BXor  x y
pattern GImpl  x y = GBinop False BImpl x y
pattern GNimpl x y = GBinop True  BImpl x y
pattern Mux   s d0 d1 = GMux False False s d0 d1
pattern MuxNS s d0 d1 = GMux True  False s d0 d1
pattern MuxSN s d0 d1 = GMux False True  s d0 d1
pattern MuxN  s d0 d1 = GMux True  True  s d0 d1
pattern High = GConst True
pattern Low  = GConst False
pattern Id  x = GUnop False x
pattern Not x = GUnop True  x

{-# COMPLETE High, Low, GUnop, GMux, GBinop #-}
{-# COMPLETE GConst, Id, Not, GMux, GBinop #-}
{-# COMPLETE High, Low, Id, Not, GMux, GBinop #-}
{-# COMPLETE GConst, GUnop, Mux, MuxNS, MuxSN, MuxN, GBinop #-}
{-# COMPLETE High, Low, GUnop, Mux, MuxNS, MuxSN, MuxN, GBinop #-}
{-# COMPLETE GConst, Id, Not, Mux, MuxNS, MuxSN, MuxN, GBinop #-}
{-# COMPLETE High, Low, Id, Not, Mux, MuxNS, MuxSN, MuxN, GBinop #-}
{-# COMPLETE GConst, GUnop, GMux, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE High, Low, GUnop, GMux, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE GConst, Id, Not, GMux, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE High, Low, Id, Not, GMux, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE GConst, GUnop, Mux, MuxNS, MuxSN, MuxN, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE High, Low, GUnop, Mux, MuxNS, MuxSN, MuxN, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE GConst, Id, Not, Mux, MuxNS, MuxSN, MuxN, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE High, Low, Id, Not, Mux, MuxNS, MuxSN, MuxN, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}

class Fixable o where
  type Fixup o
  fixup :: o -> Fixup o
  sigs :: o -> [(String, Int)]

instance Fixable SingleVar where
  type Fixup SingleVar = Signal Bool
  fixup = var . unVar
  sigs = (:[]) . (,1) . unVar

instance Fixable ListVar where
  type Fixup ListVar = [Signal Bool]
  fixup = uncurry (flip varList) . unVarList
  sigs = (:[]) . unVarList

instance Fixable a => Fixable [a] where
  type Fixup [a] = [Fixup a]
  fixup = map fixup
  sigs = concatMap sigs

instance Fixable () where
  type Fixup () = ()
  fixup = const ()
  sigs = const []

instance (Fixable a, Fixable b) => Fixable (a, b) where
  type Fixup (a, b) = (Fixup a, Fixup b)
  fixup = fixup *** fixup
  sigs = uncurry (++) . (sigs *** sigs)

instance (Fixable a, Fixable b, Fixable c) => Fixable (a, b, c) where
  type Fixup (a, b, c) = (Fixup a, Fixup b, Fixup c)
  fixup (a, b, c) = (fixup a, fixup b, fixup c)
  sigs (a, b, c) = sigs a ++ sigs b ++ sigs c

instance (Fixable a, Fixable b, Fixable c, Fixable d) =>
         Fixable (a, b, c, d) where
  type Fixup (a, b, c, d) = (Fixup a, Fixup b, Fixup c, Fixup d)
  fixup (a, b, c, d) = (fixup a, fixup b, fixup c, fixup d)
  sigs (a, b, c, d) = sigs a ++ sigs b ++ sigs c ++ sigs d

instance (Fixable a, Fixable b, Fixable c, Fixable d, Fixable e) =>
         Fixable (a, b, c, d, e) where
  type Fixup (a, b, c, d, e) = (Fixup a, Fixup b, Fixup c, Fixup d, Fixup e)
  fixup (a, b, c, d, e) = (fixup a, fixup b, fixup c, fixup d, fixup e)
  sigs (a, b, c, d, e) = sigs a ++ sigs b ++ sigs c ++ sigs d ++ sigs e

instance (Fixable a, Fixable b, Fixable c, Fixable d, Fixable e, Fixable f) =>
         Fixable (a, b, c, d, e, f) where
  type Fixup (a, b, c, d, e, f) =
    (Fixup a, Fixup b, Fixup c, Fixup d, Fixup e, Fixup f)
  fixup (a, b, c, d, e, f) =
    (fixup a, fixup b, fixup c, fixup d, fixup e, fixup f)
  sigs (a, b, c, d, e, f) =
    sigs a ++ sigs b ++ sigs c ++ sigs d ++ sigs e ++ sigs f

instance (Fixable a, Fixable b, Fixable c,
          Fixable d, Fixable e, Fixable f, Fixable g) =>
         Fixable (a, b, c, d, e, f, g) where
  type Fixup (a, b, c, d, e, f, g) =
    (Fixup a, Fixup b, Fixup c, Fixup d, Fixup e, Fixup f, Fixup g)
  fixup (a, b, c, d, e, f, g) =
    (fixup a, fixup b, fixup c, fixup d, fixup e, fixup f, fixup g)
  sigs (a, b, c, d, e, f, g) =
    sigs a ++ sigs b ++ sigs c ++ sigs d ++ sigs e ++ sigs f ++ sigs g

showNL :: NetList -> String
listify :: (Fixable a, Fixable b,
            LG.Generic (Fixup a), LG.Generic (Fixup b)) =>
           String -> (Fixup a -> Fixup b) -> a -> b -> NetList

listifyWith :: (Fixable a, Fixable b,
                LG.Generic (Fixup a), LG.Generic (Fixup b)) =>
            [IM.IntMap Gate -> Maybe (IM.IntMap Gate)] -> String ->
            (Fixup a -> Fixup b) -> a -> b -> NetList

newtype NL g = NL { getNL :: ([(String, [g])], IM.IntMap g) }
  deriving (Generic, NFData, Show)

cleanUp :: [NL Gate -> Maybe (NL Gate)] -> NL Gate -> NL Gate
readNetList :: [(String, Int)] -> String -> NL Gate

listifyWith x name f a b =
  uncurry (NetList name (sigs a)) $ getNL $ force $
  cleanUp (map (((fmap NL . sequenceA) .) . (. getNL) . second) x) $
  readNetList (sigs b) $ unsafePerformIO $
  withSystemTempFile (name ++ ".vhd") $
  \fn h -> do
    hClose h
    GBL.writeVhdlInputOutputNoClk (reverse $ drop 4 $ reverse fn) f
      (fixup a) (fixup b)
    force <$> readFile fn

listify = listifyWith []

iterateUntilDone :: (a -> Maybe a) -> a -> a
iterateUntilDone = fix . (. flip maybe) . flip ap

iterateOnceTillDone :: (a -> Maybe a) -> a -> Maybe a
iterateOnceTillDone = ap (.) iterateUntilDone

which :: a -> a -> Bool -> a
which = flip . flip if'

strategies :: [NL Gate -> Maybe (NL Gate)]

handleOutputs :: NL Gate -> NL Gate

cleanUp x = handleOutputs .
  iterateUntilDone (force . msum . flip (map . (&)) (strategies ++ x))

handleLines :: [(String, Int)] -> [String] -> NL Gate
type HLState = (M.Map String [(Int, Gate)], IM.IntMap Gate)
handleLine :: [String] -> State HLState ()

handleLines b = hlsToNetList b .
                flip execState (initHLS b) .
                sequence_ . map (handleLine . words) . init

initHLS :: [(String, Int)] -> HLState
hlsToNetList :: [(String, Int)] -> HLState -> NL Gate

decodePort :: String -> SR
decodePort ('w':s) = (Nothing, read s)
decodePort ss = (Just *** read . tail) $ splitAt brk ss where
  brk = last $ elemIndices '_' ss

initHLS = (, IM.empty) . M.fromList . map ((, []) . fst)
hlsToNetList = (NL .) .
  first . (. (uncurry . liftM2 (.) (,) .
              (((elems .) . flip (array . (0,) . (-1 +))) .) . (M.!))) .
  flip map

handleLine (_:_:etype:_:_:pts) =
  modify' $ uncurry (maybe (second . flip IM.insert gate) $
                     (first .) . flip (M.adjust . (:) . (, gate))) $
  last ports
  where ports = map decodePort pts
        gate = case drop 5 etype of
          "vdd" -> High
          "gnd" -> Low
          "wire" -> Id $ ports !! 0
          "invG" -> Not $ ports !! 0
          "andG" -> GAnd (ports !! 0) (ports !! 1)
          "orG" -> GOr (ports !! 0) (ports !! 1)
          "xorG" -> GXor (ports !! 0) (ports !! 1)
          "mux2" -> Mux (ports !! 0) (ports !! 1) (ports !! 2)

handleLine _ = return ()

readNetList b =
  handleLines b . map (filter (`notElem` ", ();:")) . tail .
  dropWhile (not . isPrefixOf "begin") . lines

strategies = [simplifyGates, removeUnused, mergeCommonNots, mergeConsts,
              mergeNotsPre, removeNotsAndWires]

parallelSimp :: (Traversable t) => (a -> Maybe a) -> t a -> Maybe (t a)

parallelSimp = (uncurry (which Just (const Nothing) . getAny) .) .
  traverse . ap (flip maybe (Any True,) . (mempty,))

nlToWhatsit = pairUp . first (Compose . Compose) . getNL

whatsitToNL = NL . first (getCompose . getCompose) . unpair

instance Functor NL where
  fmap f = whatsitToNL . fmap f . nlToWhatsit
instance Foldable NL where
  foldMap f = foldMap f . nlToWhatsit
  foldr f a = foldr f a . nlToWhatsit
instance Traversable NL where
  traverse f = fmap whatsitToNL . traverse f . nlToWhatsit

pairUp :: (f a, g a) -> Product f g a
unpair :: Product f g a -> (f a, g a)

pairUp = uncurry Pair
unpair (Pair a b) = (a, b)

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
      (foldMap (foldMap justNumbers . wires) . IM.restrictKeys int) $
      foldMap (foldMap (foldMap justNumbers . wires) . snd) out


repeatUntilNoChange :: Eq a => (a -> a) -> a -> a
repeatUntilNoChange f x = if x == x' then x else repeatUntilNoChange f x' where
  x' = f x

wires :: Gate -> [SR]
wires g = case g of
  GConst _ -> []
  GUnop _ x -> [x]
  GBinop _ _ x y -> [x, y]
  GMux _ _ s d0 d1 -> [s, d0, d1]

maybeToBool :: b -> Maybe b -> (Any, b)
maybeToBool = flip maybe (Any True,) . pure
boolToMaybe :: (Any, b) -> Maybe b
boolToMaybe = uncurry $ which Just (const Nothing) . getAny
  
replaceWires :: (SR -> Maybe SR) -> Gate -> Maybe Gate
replaceWires = (boolToMaybe .) . replWires . ap maybeToBool where
  replWires f (GUnop t x) = GUnop t <$> f x
  replWires f (GBinop a b x y) = GBinop a b <$> f x <*> f y
  replWires f (GMux a b s x y) = GMux a b <$> f s <*> f x <*> f y
  replWires _ x = pure x

justNumbers :: SR -> IS.IntSet
justNumbers = uncurry $ maybe IS.singleton (const $ const IS.empty)

justUnops :: Gate -> Maybe (SR, Bool)
justUnops (GUnop f x) = Just (x, f)
justUnops _ = Nothing

gatherUnopsInt :: IM.IntMap Gate -> IM.IntMap (SR, Bool)
gatherUnopsInt = IM.mapMaybe justUnops

keepOnlyNots :: IM.IntMap (SR, Bool) -> IM.IntMap SR
keepOnlyNots = IM.mapMaybe (uncurry $ flip $ which Just (const Nothing))

flipNots :: IM.IntMap SR -> M.Map SR Int
flipNots = IM.foldMapWithKey $ flip M.singleton

canonMap :: (Ord b) => IM.IntMap a -> M.Map b Int -> b -> Maybe a
canonMap o i = (o IM.!?) <=< (i M.!?)

canonicalizeNot :: IM.IntMap SR -> M.Map SR Int -> SR -> Maybe SR
canonicalizeNot c f a = do ac <- canonMap c f a
                           when (a == ac) Nothing
                           Just ac

mergeCommonNots x@(NL (_, int)) =
  parallelSimp (replaceWires $ canonicalizeNot c f) x where
  c = keepOnlyNots $ gatherUnopsInt int
  f = flipNots c

counts :: Foldable t => t Gate -> IM.IntMap Int
counts = flip appEndo IM.empty . foldMap (foldMap (Endo . cnt) . wires) where
  cnt = uncurry $ maybe (flip (IM.insertWith (+)) 1) (const $ const id)

findMergeableNots :: NL Gate -> IM.IntMap Int
findMergeableNots x@(NL (_, int)) = IM.filter (inOnce $ counts x) $ 
  IM.mapMaybe isInt $ keepOnlyNots $ gatherUnopsInt int where
  inOnce = ((== 1) .) . flip (IM.findWithDefault 0)
  isInt = uncurry $ maybe Just (const $ const Nothing)

notOutsAndIns :: IM.IntMap Int -> IS.IntSet
notOutsAndIns m = (k IS.\\ v) `IS.union` (v IS.\\ k) where
  k = IM.keysSet m
  v = IS.fromList $ IM.elems m
  
mergeNotAfterInto :: Gate -> Gate
mergeNotAfterInto g = case g of
  GConst b -> GConst (not b)
  GUnop f x -> GUnop (not f) x
  GBinop f o x y -> GBinop (not f) o x y
  GMux f0 f1 s d0 d1 -> on GMux not f0 f1 s d0 d1

mergeNotsPre x =
  if IS.null mns then Nothing
  else Just $ NL $ second (flip (IS.foldr $ IM.adjust mergeNotAfterInto) mns) $
       getNL x where
    mns = notOutsAndIns $ findMergeableNots x

checkRes :: IM.IntMap (SR, Bool) -> SR -> Maybe (SR, Bool)
checkRes m x = do when (isJust $ fst x) Nothing
                  v <- m IM.!? (snd x)
                  when (fst v == x) $
                    error "Setting x to either x or !x always for some x."
                  return v

handleNotsAndWires :: (SR -> Maybe (SR, Bool)) -> Gate -> Maybe Gate
handleNotsAndWires = (boolToMaybe .) . hnw . ap (maybeToBool . (, False)) where
  hnw f (GUnop b x) = huo b <$> f x
  hnw f (GBinop b o x y) = hbo b o <$> f x <*> f y
  hnw f (GMux f0 f1 s d0 d1) = hmx f0 f1 <$> f s <*> f d0 <*> f d1
  hnw _ x = pure x
  huo b0 (s, b0') = GUnop (b0 /= b0') s
  hbo br BAnd (x, bx) (y, by) = han br bx by x y
  hbo br BOr (x, bx) (y, by) = han (not br) (not bx) (not by) x y
  hbo br BImpl (x, bx) (y, by) = han (not br) bx (not by) x y
  hbo br BXor (x, bx) (y, by) = GBinop ((br /= bx) /= by) BXor x y
  han = which (which (which GOr  $ flip GImpl)  (which GImpl  GNand))
              (which (which GNor $ flip GNimpl) (which GNimpl GAnd))
  hmx f0 f1 (s, bs) db0 db1 =
    let ((d0, b0), (d1, b1)) = if' bs swap id (second (/= f0) db0,
                                               second (/= f1) db1) in
      GMux b0 b1 s d0 d1

removeNotsAndWires x@(NL (_, int)) =
  parallelSimp (handleNotsAndWires $ checkRes $ gatherUnopsInt int) x

gatherConsts :: IM.IntMap Gate -> IM.IntMap Bool
gatherConsts = IM.mapMaybe justConsts where
  justConsts (GConst b) = Just b
  justConsts _ = Nothing

correctConsts :: (Int -> Maybe Bool) -> Gate -> Maybe Gate
correctConsts = (boolToMaybe .) . crcns . foo where
  foo = liftA2 maybeToBool Right . (fmap Left .) . uncurry .
    flip maybe (const $ const Nothing)
  crcns :: (SR -> (Any, Either Bool SR)) -> Gate -> (Any, Gate)
  crcns f (GUnop b x) = huo b <$> f x
  crcns f (GBinop b o x y) = hbo b o <$> f x <*> f y
  crcns f (GMux f0 f1 s d0 d1) = hmx f0 f1 <$> f s <*> f d0 <*> f d1
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

showPort :: String -> (String, Int) -> String
showSignal :: Int -> String
dumpComponent :: (SR, Gate) -> String
dumpFancy :: (String, [Gate]) -> [String]

showNL (NetList n i o g) = unlines $
  unwords ["entity", n, "is"]:thePorts ++
  unwords ["component", n]:thePorts ++
  unwords ["architecture structural of", n, "is"]:
  map showSignal (IM.keys g) ++
  "begin":
  map (dumpComponent . first (Nothing,)) (IM.assocs g) ++
  concatMap dumpFancy o
  where
    thePorts = "  port (": map (showPort "in") i ++
      map (showPort "out" . second length) (init o) ++
      [(++");") $ init $ showPort "out" $ second length $ last o,
       "end;",
       ""]

showPort ty (n, l) = "    " ++ unwords [n, ":", ty, "bit"] ++
                     if l /= 1
                     then unwords ["_vector", '(':show (l - 1), "downto 0);"]
                     else ";"
showSignal = ('v':) . show

dumpFancy (n, gs) = zipWith (curry dumpComponent . (Just n,))
                    (iterate (-1+) $ length gs - 1) gs

showSR :: SR -> String
showSR = uncurry $ maybe showSignal $ (. (('(':) . (++ ")") . show)) . (++)
showSRBlob :: SR -> String
showSRBlob = uncurry $ maybe showSignal $ (. (('_':) . show)) . (++)
showGateType :: Gate -> String

dumpComponent (n, g) = concat $
                       ["  c_", showSRBlob n,
                        " entity work.",
                        showGateType g,
                        " port map ("] ++
                        intersperse ", " (map showSR $ wires g ++ [n]) ++
                        [");"]
                        
showGateType (GAnd _ _) = "andG"
showGateType (GOr _ _) = "orG"
showGateType (GNand _ _) = "nandG"
showGateType (GNor _ _) = "norG"
showGateType (GXor _ _) = "xorG"
showGateType (GIff _ _) = "xnorG"
showGateType (GImpl _ _) = "implG"
showGateType (GNimpl _ _) = "nimplG"
showGateType (Mux _ _ _) = "mux2"
showGateType (MuxNS _ _ _) = "muxnk"
showGateType (MuxSN _ _ _) = "muxkn"
showGateType (MuxN _ _ _) = "muxn"
showGateType High = "vdd"
showGateType Low = "gnd"
showGateType (Id _) = "wire"
showGateType (Not _) = "invG"
