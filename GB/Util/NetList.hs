{-# LANGUAGE DeriveGeneric, DeriveAnyClass, TupleSections #-}
{-# LANGUAGE PatternSynonyms, LambdaCase #-}

-- TODO: Implement composition.
module GB.Util.NetList (NetList(..), listify, showNL,
                        Gate(..), Binop(..), SR, NLP,
                        pattern GAnd, pattern GNand,
                        pattern GOr, pattern GNor,
                        pattern GXor, pattern GIff,
                        pattern GImpl, pattern GNimpl,
                        pattern MuxS, pattern MuxNS,
                        pattern MuxSN, pattern MuxN,
                        pattern High, pattern Low,
                        pattern Id, pattern Not,
                        addSignal, addInput, addOutput, setOutput,
                        setOutputs, removeSignals,
                        incorporate, compress, verify, nlEmpty, nlDelay,
                        nlUnop, nlBinop, nlConst, nlMux, nlDff, nlDffZ, nlSR,
                        countTransistors, wires,
                        LavaGen, SingVar, ListVar, Fixup,
                        varSing, varList) where
import GB.Util.Base
import GB.Lava.Signal
import qualified GB.Lava.Netlist as LNL
import GB.Lava.Netlist hiding (sigs)
import Control.Monad.State
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Functor
import Data.Monoid
import Control.Applicative
import Data.Foldable
import Control.Arrow
import Control.Monad
import Data.Function
import Data.Maybe
import GHC.Generics
import Data.List
import Control.DeepSeq

data NetList = NetList { inputs :: [(String, Int)],
                         outputs :: [(String, [(SR, Bool)])],
                         gates :: IntMap Gate,
                         sigs :: IntMap [Int],
                         nGate :: Int}
             deriving (Generic, NFData, Show)

type SR = (Maybe String, Int)
type NLP = (Either String Int, Int)

data Binop = BAnd | BOr | BXor | BImpl
           deriving (Eq, Generic, NFData, Show)

data Gate = GConst Bool -- f
          -- o = f
          | GUnop Bool SR -- f x
          -- o = f ^ x
          | GBinop Bool Binop SR SR -- f op l r
          -- o = f ^ (l `op` r)
          | GMux Bool Bool SR SR SR -- f0 f1 s d0 d1
          -- o = s? f1 ^ d1 : f0 ^ d0
          | GDff Bool SR SR -- fw w d
          -- Set q to d if w /= fw.
          -- Else no change.
          | GDffZ Bool Bool Bool SR SR SR -- fw fz zs w z d
          -- Set q to zs if z == fz.
          -- Else behaves as GDff.
          | GSR Bool Bool Bool SR SR -- fs fr fq s r
          -- Set q to fq if r /= fr.
          -- Else set q to !fq if s /= fs.
          -- Else no change.
          | GDelay SR
          deriving (Eq, Generic, NFData, Show)

pattern GAnd   x y = GBinop False BAnd  x y
pattern GOr    x y = GBinop False BOr   x y
pattern GNand  x y = GBinop True  BAnd  x y
pattern GNor   x y = GBinop True  BOr   x y
pattern GXor   x y = GBinop False BXor  x y
pattern GIff   x y = GBinop True  BXor  x y
pattern GImpl  x y = GBinop False BImpl x y
pattern GNimpl x y = GBinop True  BImpl x y
pattern MuxS  s d0 d1 = GMux False False s d0 d1
pattern MuxNS s d0 d1 = GMux True  False s d0 d1
pattern MuxSN s d0 d1 = GMux False True  s d0 d1
pattern MuxN  s d0 d1 = GMux True  True  s d0 d1
pattern High = GConst True
pattern Low  = GConst False
pattern Id  x = GUnop False x
pattern Not x = GUnop True  x

{-# COMPLETE GDelay, GSR, GDff, GDffZ, High, Low, GUnop, GMux, GBinop #-}
{-# COMPLETE GDelay, GSR, GDff, GDffZ, GConst, Id, Not, GMux, GBinop #-}
{-# COMPLETE GDelay, GSR, GDff, GDffZ, High, Low, Id, Not, GMux, GBinop #-}
{-# COMPLETE GDelay, GSR, GDff, GDffZ, GConst, GUnop, MuxS, MuxNS, MuxSN, MuxN, GBinop #-}
{-# COMPLETE GDelay, GSR, GDff, GDffZ, High, Low, GUnop, MuxS, MuxNS, MuxSN, MuxN, GBinop #-}
{-# COMPLETE GDelay, GSR, GDff, GDffZ, GConst, Id, Not, MuxS, MuxNS, MuxSN, MuxN, GBinop #-}
{-# COMPLETE GDelay, GSR, GDff, GDffZ, High, Low, Id, Not, MuxS, MuxNS, MuxSN, MuxN, GBinop #-}
{-# COMPLETE GDelay, GSR, GDff, GDffZ, GConst, GUnop, GMux, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE GDelay, GSR, GDff, GDffZ, High, Low, GUnop, GMux, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE GDelay, GSR, GDff, GDffZ, GConst, Id, Not, GMux, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE GDelay, GSR, GDff, GDffZ, High, Low, Id, Not, GMux, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE GDelay, GSR, GDff, GDffZ, GConst, GUnop, MuxS, MuxNS, MuxSN, MuxN, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE GDelay, GSR, GDff, GDffZ, High, Low, GUnop, MuxS, MuxNS, MuxSN, MuxN, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE GDelay, GSR, GDff, GDffZ, GConst, Id, Not, MuxS, MuxNS, MuxSN, MuxN, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE GDelay, GSR, GDff, GDffZ, High, Low, Id, Not, MuxS, MuxNS, MuxSN, MuxN, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}

showNL :: NetList -> String -> String
listify :: (LavaGen a, LavaGen b) =>
           (Fixup a -> Fixup b) -> a -> b -> NetList
addSignal :: Int -> NetList -> (NetList, Int)
-- pass in length
addInput :: String -> Int -> NetList -> NetList
-- name, length. Added at start.
addOutput :: String -> Int -> NetList -> NetList
-- name, length. Added at start.
setOutput :: String -> Int -> NLP -> Bool -> NetList -> NetList
setOutputs :: Map String (IntMap (NLP, Bool)) -> NetList -> NetList
incorporate :: (String -> [Maybe NLP]) -> NetList -> NetList -> NetList
-- Port map, part, whole.
-- Part should have no missing pieces.
compress :: NetList -> NetList
-- Changes internals around.
verify :: NetList -> Bool
-- Checks if no dangling inputs.
nlEmpty :: NetList

-- Following netlists have one output named o, of given length.
-- All inputs are of same length.
-- Exceptions: Mux selector and Dff non-data inputs are length 1,
-- as is Const output. Dff/ output is named q.
nlUnop :: Int -> Bool -> NetList -- input: x
nlBinop :: Int -> Bool -> Binop -> NetList -- inputs: l, r
nlConst :: Bool -> NetList -- inputs: none
nlMux :: Int -> Bool -> Bool -> NetList -- inputs: s, d0, d1
nlDff :: Int -> Bool -> NetList -- inputs: c, w, d
nlDffZ :: Int -> Bool -> Bool -> Bool -> NetList
-- inputs: fw, fz, zs
nlSR :: Int -> Bool -> Bool -> Bool -> NetList
-- inputs: fs, fr, fq
nlDelay :: Int -> NetList -- input: i

removeSignals :: NetList -> NetList

countTransistors :: NetList -> Int

wires :: Gate -> [SR]
wires = \case
  GConst _ -> []
  GUnop _ x -> [x]
  GBinop _ _ x y -> [x, y]
  GMux _ _ s d0 d1 -> [s, d0, d1]
  GDff _ w d -> [w, d]
  GDffZ _ _ _ w z d -> [w, z, d]
  GSR _ _ _ s r -> [s, r]
  GDelay s -> [s]

detOuts :: [((String, Int), Int)] -> [(String, Int)] ->
           [(String, [(SR, Bool)])]
detGates :: [(Int, Sig Int)] -> IntMap Gate

listify f a b = compress $ NetList {
  inputs = LNL.sigs a,
  outputs = detOuts outs $ LNL.sigs b,
  gates = detGates gates,
  sigs = IM.empty,
  nGate = 0 }
  where (gates, outs) = netlist f a b

droppingList :: Int -> [Int]
droppingList i = [i-1, i-2 .. 0]

detOuts = fmap . uncurry . liftA2 (.) (,) .
          (((. droppingList) . fmap) .) .
          (. (,)) . (.) . (((, False) . (Nothing,)) .) . (M.!) . M.fromList

detGates = fmap detGate . IM.fromList where
  foo = (Nothing,)
  detGate (Bool b) = GConst b
  detGate (Inv b) = Not $ foo b
  detGate (And a b) = on GAnd foo a b
  detGate (Or a b) = on GOr foo a b
  detGate (Xor a b) = on GXor foo a b
  detGate (Var n i) = Id (Just n, i)
  detGate (Dff w d) = (GDff False `on` foo) w d
  detGate (DffZ w z d) = (GDffZ False False False `on3` foo) w z d
  detGate (Mux s d0 d1) = on3 MuxS foo s d0 d1
  detGate (Delay x) = GDelay $ foo x
  
showPort :: String -> (String, Int) -> String
showSignal :: Int -> String
dumpComponent :: (SR, Gate) -> String
dumpFancy :: (String, [(SR, Bool)]) -> [String]

showNL (NetList i o g _ _) n = unlines $
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

showSR :: SR -> String
showSR = uncurry $ maybe showSignal $ (. (('(':) . (++ ")") . show)) . (++)
showSRBlob :: SR -> String
showSRBlob = uncurry $ maybe showSignal $ (. (('_':) . show)) . (++)
showGateType :: Gate -> String

dumpOneFancy :: String -> Int -> (SR, Bool) -> String
dumpFancy (n, g) = zipWith (dumpOneFancy n) [0..] $
                   reverse g
dumpOneFancy sn si (t, z) =
  concat $ ["  ", showSR (Just sn, si), " <= "] ++
  if' z ("not ":) id [showSR t]

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
showGateType (MuxS _ _ _) = "mux2"
showGateType (MuxNS _ _ _) = "muxnk"
showGateType (MuxSN _ _ _) = "muxkn"
showGateType (MuxN _ _ _) = "muxn"
showGateType (GDelay _) = "delay"
showGateType High = "vdd"
showGateType Low = "gnd"
showGateType (Id _) = "wire"
showGateType (Not _) = "invG"
showGateType (GDff w _ _) = "dff_" ++ if' w "l" "h"
showGateType (GDffZ w z s _ _ _) = "dff_" ++ if' w 'l' 'h' : "_z" ++
                                         if' z 'h' 'l' :
                                         if' s "s" "r"
showGateType (GSR s r q _ _) = "srff_s" ++ if' s 'l' 'h' : "_r" ++
                               if' r 'l' 'h' : if' q "_inv" ""

addSignal n (NetList i o g s m) = (NetList i o g s' (m + n), sn)
  where s' = IM.insert sn [m .. m + n - 1] s
        sn = maybe 0 ((+1) . fst) $ IM.lookupMin s

nlpToSR :: (Int -> [Int]) -> NLP -> SR
nlpToSR = uncurry . either ((,) . Just) . ((((Nothing,) .) . (!!)) .)

addInput n l (NetList i o g s m) = NetList ((n,l):i) o g s m

addOutput n l (NetList i o g s m) =
  NetList i ((n, (, False) . (Nothing,) <$> [m .. m + l - 1]):o) g s (m + l)

setOutput n i s b x = x { outputs = foo (outputs x) } where
  foo (x@(a, d):as) = if a == n
                    then (a, bar (length d - i - 1) d):as
                    else x:foo as
  bar 0 (_:bs) = (nlpToSR (sigs x IM.!) s, b):bs
  bar i (b:bs) = i `seq` b:bar (i - 1) bs

setOutputs m x = x {outputs = foo <$> outputs x } where
  foo x@(a, d) = fromMaybe x $ (a,) . reverse . zipWith bar (reverse d) .
                 (<$> [0..]) . (IM.!?) <$> (m M.!? a)
  bar b g = maybe b (first $ nlpToSR (sigs x IM.!)) g
    
on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 = join . ((flip . (on .)) .) . (.)

rewire :: (SR -> SR) -> Gate -> Gate
rewire _ x@(GConst _) = x
rewire g (GUnop f x) = GUnop f $ g x
rewire g (GBinop f o x y) = (GBinop f o `on` g) x y
rewire g (GMux f0 f1 s d0 d1) = (GMux f0 f1 `on3` g) s d0 d1
rewire g (GDff fw w d) = (GDff fw `on` g) w d
rewire g (GDffZ fw fz zs w z d) = (GDffZ fw fz zs `on3` g) w z d
rewire g (GDelay x) = GDelay $ g x

mapEither :: (a -> Either b c) -> [a] -> ([b], [c])
mapEither = flip foldr ([], []) . (either (first . (:)) (second . (:)) .)

incorporate f (NetList pi po pg _ pm) (NetList wi wo wg ws wm) =
  NetList wi wo' wg' ws wm' where
  pio = fmap fst pi ++ fmap fst po
  smap = (M.!) $ M.fromList $
    (,) <*> fmap (nlpToSR (ws IM.!) <$>) . f <$> pio
  po' = second (fmap $ first $ uncurry $
         maybe ((Nothing,) . (+ wm))
         ((fromJust .) . (!!) . smap)) <$> po
  pg' = IM.mapKeysMonotonic (+ wm) $
        rewire (uncurry $
         maybe ((Nothing,) . (+ wm))
         ((fromJust .) . (!!) . smap)) <$> pg
  (mapI, mapO) = mapEither (uncurry $ uncurry $ maybe ((Left .) . (,))
                             ((((Right .) . (,)) .) . (,))) $
    catMaybes . uncurry (zipWith (flip $ fmap . flip (,)) . smap) =<< po'
  wg' = pg' <> foldl' (flip $ uncurry $ flip $
                       flip IM.insert . uncurry (flip GUnop)) wg mapI
  oMap = flip (flip . flip M.findWithDefault) $ M.fromList mapO
  wo' = uncurry ((.) <$> (,) <*>
                 ((reverse .) . (. reverse) .
                  zipWith oMap . (<$> [0..]) . (,))) <$> wo
  wm' = wm + pm

addMap :: Int -> Endo (Int, IntMap Int)

addMap i = Endo $ \(a, m) -> a `seq`
                             if i `IM.member` m
                             then (a, m)
                             else (a + 1, IM.insert i a m)

removeSignals (NetList i o g _ _) = compress $ NetList i o g IM.empty 0

compress (NetList i o g s _) = NetList i o' g' s' m where
  theMaps = foldMap (foldMap (doSR . fst) . snd) o <>
    IM.foldMapWithKey ((. (foldMap doSR . wires)) . (<>) . addMap) g <>
    foldMap (foldMap addMap) s
  doSR = uncurry $ maybe addMap (const $ const mempty)
  (m, finMap) = appEndo theMaps (0, IM.empty)
  rewireSR (Nothing, i) = (Nothing, finMap IM.! i)
  rewireSR x = x
  o' = second (fmap $ first rewireSR) <$> o
  g' = IM.mapKeys (finMap IM.!) $ rewire rewireSR <$> g
  s' = fmap (finMap IM.!) <$> s

verifyInputs :: Map String Int -> All

verifyInputs = foldMap (All . (>= 0))

verify (NetList i o g s m) = getAll $ verifyInputs i' <>
                             All (IS.findMin g' >= 0) <>
                             All (IS.findMax g' < m) <>
                             checkLegal (checkSR . fst) snd o <>
                             checkLegal checkSR wires g <>
                             checkLegal (`IS.member` g') id s where
  g' = IM.keysSet g
  i' = M.fromList i
  checkLegal :: (Foldable f, Foldable g) => (b -> Bool) -> (a -> g b) ->
                f a -> All
  checkLegal = (foldMap .) . (.) . foldMap . (All .)
  checkSR = uncurry $ maybe (`IS.member` g') $
    liftA2 (&&) (>= 0) . (>) . flip (M.findWithDefault 0) i'


mkSR :: String -> Int -> SR
i2SR :: Int -> SR
mkSR = (,) . Just
i2SR = (Nothing,)

makeGateSet :: (Int -> Gate) -> Int -> IntMap Gate
makeGateSet = (. (IS.fromAscList . enumFromTo 0 . (-1+))) . IM.fromSet
outputsOf :: String -> Int -> [(String, [(SR, Bool)])]
outputsOf s i = [(s, (, False) . i2SR <$> droppingList i)]

standard :: Int -> [(String, Int)] -> String -> (Int -> Gate) -> NetList

standard n i s f = NetList i (outputsOf s n) (makeGateSet f n) IM.empty n

nlEmpty = NetList [] [] IM.empty IM.empty 0

nlUnop n f = NetList {
  inputs = [("x", n)],
  outputs = [("o", (, f) . mkSR "x" <$> [n-1, n-2 .. 0])],
  gates = IM.empty,
  sigs = IM.empty,
  nGate = 0 }
nlBinop n f o = standard n [("l", n), ("r", n)] "o" $
                GBinop f o <$> mkSR "l" <*> mkSR "r"
nlConst = standard 1 [] "o" . const . GConst
nlMux n f0 f1 = standard n [("s", 1), ("d0", n), ("d1", n)] "o" $
                GMux f0 f1 (mkSR "s" 0) <$> mkSR "d0" <*> mkSR "d1"
nlDff n fw =
  standard n [("w", 1), ("d", n)] "q" $ GDff fw (mkSR "w" 0) . mkSR "d"
nlDffZ n fw fz zs =
  standard n [("w", 1), ("z", 1), ("d", n)] "q" $
  GDffZ fw fz zs (mkSR "w" 0) (mkSR "z" 0) . mkSR "d"
nlSR n fs fr fq =
  standard n [("s", n), ("r", n)] "q" $
  GSR fs fr fq <$> mkSR "s" <*> mkSR "r"
nlDelay n = standard n [("i", n)] "o" $ GDelay <$> mkSR "i"

countTransistors = (+) <$> countTransistorsO <*> countTransistorsI

which :: c -> c -> Bool -> c
which a _ True = a
which _ a False = a

countTransistorsO = getSum .
                    foldMap (foldMap (Sum . which 2 0 . snd) . snd) .
                    outputs

countTransistorsI = getSum . foldMap (Sum . countTrans) . gates where
  countTrans (GConst _) = 0
  countTrans (GUnop False _) = 0
  countTrans (GUnop True _) = 2
  countTrans (GBinop _ BXor _ _) = 8
  countTrans (GBinop _ _ _ _) = 4
  countTrans (GMux _ _ _ _ _) = 8
  countTrans (GDff _ _ _) = 6
  countTrans (GDffZ _ _ _ _ _ _) = 8
  countTrans (GSR _ _ _ _ _) = 6
  countTrans (GDelay _) = 10 -- buf^5

-- t(s, q, t, a, b) transmits signals of type q from a to b when t is s.
-- tm(s, t, a, b) is t(s, H, t, a, b) and t(s, L, t, a, b).
-- Dff(fw, w, d) is:
--   tm(fw, w, q, x)
--   tm(!fw, w, d, x)
--   t(H, x, H, q)
--   t(L, x, L, q)
--
-- DffZ(fw, fz, zs, w, z, d) is:
--   tm(fw, w, q, x)
--   tm(!fw, w, d, x)
--   t(zs, x, zs, q)
--   t(!fz, z, zs, q)
--   t(!zs, x, !zs, y)
--   t(fz, z, y, q)

