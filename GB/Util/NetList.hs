{-# LANGUAGE DeriveGeneric, DeriveAnyClass, TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}

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
                        incorporate, compress, verify, nlEmpty,
                        nlUnop, nlBinop, nlConst, nlMux, nlDff, nlDffZ,
                        countTransistors, wires, hmDff,
                        hmDffZ, hmUnop, hmBinop, hmMux) where
import GB.Util.Base
import GB.Lava.Signal
import GB.Lava.Netlist
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
import Control.Monad.State
import Control.DeepSeq

data NetList = NetList { inputs :: [(String, Int)],
                         outputs :: [(String, [(SR, Bool)])],
                         gates :: IntMap Gate,
                         sigs :: IntMap [Int],
                         nGate :: Int}
             deriving (Generic, NFData, Show)

type SR = (Maybe String, Int)
type NLP = (Either String Int, Int)

vdata Binop = BAnd | BOr | BXor | BImpl
           deriving (Eq, Generic, NFData, Show)

data Gate = GConst Bool
          | GUnop Bool SR
          | GBinop Bool Binop SR SR -- negafter
          | GMux Bool Bool SR SR SR -- neg d0, neg d1, s, d0, d1
          | GDff Bool SR SR -- write, data
          | GDffZ Bool Bool Bool SR SR SR
          -- write, async, data
          -- neg write, z al/ah, z rs/set
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

{-# COMPLETE GDff, GDffZ, High, Low, GUnop, GMux, GBinop #-}
{-# COMPLETE GDff, GDffZ, GConst, Id, Not, GMux, GBinop #-}
{-# COMPLETE GDff, GDffZ, High, Low, Id, Not, GMux, GBinop #-}
{-# COMPLETE GDff, GDffZ, GConst, GUnop, MuxS, MuxNS, MuxSN, MuxN, GBinop #-}
{-# COMPLETE GDff, GDffZ, High, Low, GUnop, MuxS, MuxNS, MuxSN, MuxN, GBinop #-}
{-# COMPLETE GDff, GDffZ, GConst, Id, Not, MuxS, MuxNS, MuxSN, MuxN, GBinop #-}
{-# COMPLETE GDff, GDffZ, High, Low, Id, Not, MuxS, MuxNS, MuxSN, MuxN, GBinop #-}
{-# COMPLETE GDff, GDffZ, GConst, GUnop, GMuxS, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE GDff, GDffZ, High, Low, GUnop, GMuxS, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE GDff, GDffZ, GConst, Id, Not, GMuxS, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE GDff, GDffZ, High, Low, Id, Not, GMuxS, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE GDff, GDffZ, GConst, GUnop, MuxS, MuxNS, MuxSN, MuxN, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE GDff, GDffZ, High, Low, GUnop, MuxS, MuxNS, MuxSN, MuxN, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE GDff, GDffZ, GConst, Id, Not, MuxS, MuxNS, MuxSN, MuxN, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE GDff, GDffZ, High, Low, Id, Not, MuxS, MuxNS, MuxSN, MuxN, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}

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
-- Exceptions: Mux selector and delay non-data inputs are length 1,
-- as is Const output. Dff output is named q.
nlUnop :: Int -> Bool -> NetList -- input: x
nlBinop :: Int -> Bool -> Binop -> NetList -- inputs: l, r
nlConst :: Bool -> NetList -- inputs: none
nlMux :: Int -> Bool -> Bool -> NetList -- inputs: s, d0, d1
nlDff :: Int -> Bool -> NetList -- inputs: c, w, d
nlDffZ :: Int -> Bool -> Bool -> Bool -> NetList
-- inputs: fw, fz, zs

countTransistors :: NetList -> Int

type SRB = (SR, Bool)

hmDff ::  Bool -> SRB -> SRB -> (Gate, Bool)
hmDffZ :: Bool -> Bool -> Bool -> SRB -> SRB -> SRB -> (Gate, Bool)
hmUnop :: Bool -> SRB -> Gate
hmBinop :: Bool -> Binop -> SRB -> SRB -> Gate
hmMux :: Bool -> Bool -> SRB -> SRB -> SRB -> Gate

wires :: Gate -> [SR]
wires g = case g of
  GConst _ -> []
  GUnop _ x -> [x]
  GBinop _ _ x y -> [x, y]
  GMux _ _ s d0 d1 -> [s, d0, d1]
  GDff _ w d -> [w, d]
  GDffZ _ _ _ w z d -> [w, z, d]

detOuts :: [((String, Int), Int)] -> [(String, Int)] -> [(String, [Gate])]
detGates :: [(Int, Sig Int)] -> IntMap (Sig Int)

listify f a b =
  NetList (sigs a) (detOuts outs $ sigs b) (detGates gates) IM.empty where
  (gates, outs) = netlist f a b

detOuts = fmap . liftA2 (.) (,) .
          (((. flip (enumFromTo . (-1+)) 0) . fmap) .) . (. (,)) .
          (.) . ((Nothing,) .) . flip lookup

detGates = fmap detGate . IM.fromList where
  foo = (Nothing,)
  detGate (Bool b) = GConst b
  detGate (Inv b) = Not $ foo b
  detGate (And a b) = GAnd `on` foo a b
  detGate (Or a b) = GOr `on` foo a b
  detGate (Xor a b) = GOr `on` foo a b
  detGate (Var n i) = Id (Just n, i)
  detGate (Dff w d) = GDff False (foo w) (foo d)
  detGate (DffZ w z d) = GDffZ False False False (foo w) (foo z) (foo d)

showPort :: String -> (String, Int) -> String
showSignal :: Int -> String
dumpComponent :: (SR, Gate) -> String
dumpFancy :: (String, [SRB]) -> [String]

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

dumpOneFancy :: String -> Int -> SRB -> String
dumpFancy (n, g) = zipWith (dumpOneFancy n) [0..] $
                   reverse g
dumpOneFancy sn si (t, z) =
  concat $ ["  ", showSR n, " <= "] ++ if' z ("not ":) id [showSR t]



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
showGateType High = "vdd"
showGateType Low = "gnd"
showGateType (Id _) = "wire"
showGateType (Not _) = "invG"
showGateType (GDff w _ _) = "dff_" ++ if' w "l" "h"
showGateType (GDffZ w z s _ _ _) = "dff_" ++ if' w 'l' 'h' : "_z" ++
                                         if' z 'h' 'l' :
                                         if' s "s" "r"


addSignal n (NetList i o g s m) = (NetList i o g s' (m + n), sn)
  where s' = IM.insert sn [m .. m + n - 1] s
        sn = maybe 0 ((+1) . fst) $ IM.lookupMin s

nlpToSR :: (Int -> [Int]) -> NLS -> SR
nlpToSR = uncurry . either ((,) . Just) . (((Nothing,) . (!!)) .)

addInput n l (NetList i o g s m) = NetList ((n,l):i) o g s m

addOutput n l (NetList i o g s m) =
  NetList i ((n, (Nothing,) <$> [m .. m + l - 1]):o) g s (m + l)

setOutput n i s b x = x { outputs = foo (outputs x) } where
  foo ((a, d):as) = if a == n
                    then (a, bar (length d - i - 1) d):as
                    else a:foo as
  bar 0 (_:bs) = (nlpToSR (sigs x IM.!) s, b):bs
  bar i (b:bs) = i `seq` b:bar (i - 1) bs

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 = join . ((flip . (on .)) .) . (.)

rewire :: (SR -> (SR, Bool)) -> Gate -> (Gate, Bool)
rewire _ x@(GConst _) = x
rewire g (GUnop f x) = (, False) $ hmUnop f $ g x
rewire g (GBinop f o x y) = (, False) (hmBinop f o `on` g x y)
rewire g (GMux f0 f1 s d0 d1) = (, False) (hmMux f0 f1 `on3` g s d0 d1)
rewire g (GDff fw w d) = hmDff fw `on` g w d
rewire g (GDffZ fw fz zs w z d) = hmDffZ fw fz zs `on3` g w z d

incorporate f (NetList pi po pg _ pm) (NetList wi wo wg ws wm) =
  NetList wi wo' wg' ws wm' where
  mapStr = fmap (nlpToSR (ws IM.!)) . f
  po' = (fmap $ uncurry $
         maybe ((Nothing,) . (+ wm))
         ((fromJust .) . (!!) . mapStr)) <$> po
  pg' = IM.mapKeysMonotonic (+ wm) $
        (rewire $ uncurry $
         maybe ((Nothing,) . (+ wm))
         ((fromJust .) . (!!) . mapStr)) <$> pg
  mapStuff = M.fromList $
    concatMap (catMaybes . uncurry ((fmap sequenceA .) .
                                    flip zip . mapStr)) po'
  oMap = flip ((, False) >>= M.findWithDefault) mapStuff
  doMap = uncurry $ flip (second . (/=)) . oMap
  wo' = second (fmap doMap) <$> wo
  wg' = (rewire oMap <$> wg) <> pg'
  wm' = wm + pm

addMap :: Int -> Endo (Int, IntMap Int)

addMap i = Endo $ \(a, m) -> a `seq`
                             if i `IM.member` m
                             then (a, m)
                             else (a + 1, IM.insert i a m)

compress (NetList i o g s _) = NetList i o' g' s' m where
  theMaps = foldMap (foldMap doSR . snd) o <>
    foldMapWithKey ((. (foldMap doSR . wires)) . (<>) . addMap) g <>
    foldMap (foldMap addMap) s
  doSR = uncurry $ maybe addMap (const $ const mempty)
  (m, finMap) = appEndo theMaps (0, IM.empty)
  rewireSR (Nothing, i) = (Nothing, finMap IM.! i)
  rewireSR x = x
  o' = second (fmap $ first rewireSR) <$> o
  g' = IM.mapKeys (finMap IM.!) $ rewire ((, False) . rewireSR) <$> g
  s' = fmap (finMap IM.!) <$> s

verifyInputs :: Map String Int -> All

verifyInputs = foldMap (All . (>= 0))

verify (NetList i o g s m) = getAll $ verifyInputs i' <>
                             All (findMin g' >= 0) <>
                             All (findMax g' < m) <>
                             checkLegal checkSR snd o <>
                             checkLegal checkSR wires g <>
                             checkLegal (`IS.member` g') id s where
  g' = IM.keysSet g
  i' = M.fromList i
  checkLegal = (foldMap .) . (.) . foldMap
  checkSR = uncurry $ maybe (`IS.member` g') $
    liftA2 (&&) (>= 0) . (>) . flip (IM.findWithDefault 0) i'


mkSR :: String -> Int -> SR
i2SR :: Int -> SR
mkSR = (,) . Just
i2SR = (Nothing,)

makeGateSet :: (Int -> Gate) -> Int -> IntMap Gate
makeGateSet = (. (IS.fromAscList . enumFromTo 0 . (-1+))) . IM.fromSet
outputsOf :: String -> Int -> [(String, [(SR, Bool)])]
outputsOf s i = [(s, (, False) . i2SR <$> [n-1, n-2 .. 0])]

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
                GBinop f o <$> mkSR "l" <*> mkSR "r",
nlConst = standard 1 [] "o" . const . GConst
nlMux n f0 f1 = standard n [("s", 1), ("d0", n), ("d1", n)] "o" $
                GMux f0 f1 (mkSR "s" 0) <$> mkSR "d0" <*> mkSR "d1"
nlDff n fw =
  standard n [("w", 1), ("d", n)] "q" $ GDff fw (mkSR "w" 0) . mkSR "d"
nlDffZ n fw fz zs =
  standard n [("w", 1), ("z", 1), ("d", n)] "q" $
  GDffZ fw fz zs (mkSR "w" 0) (mkSR "z" 0) . mkSR "d"

countTransistors = (+) <$> countTransistorsO <*> countTransistorsI

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

which :: a -> a -> Bool -> a
which _ a False = a
which a _ True = a

hmDff fw (w, fw') (d, fd') = (GDff (fw /= fw') d, fd')
hmDffZ fw fz zs (w, fw') (z, fz'), (d, fd') =
  (GDffZ (fw /= fw') (fz /= fz') (zs /= fd') w z d, fd')
hmUnop = uncurry . flip . (GUnop .) . (/=)
hmBinop fo BAnd (l, fl) (r, fr) = hmAnd fl fr fo l r
hmBinop fo BOr (l, fl) (r, fr) = hmAnd `on3` not fl fr fo l r
hmBinop fo BImpl (l, fl) (r, fr) = hmAnd fl `on` not fr fo l r
hmBinop fo BXor (l, fl) (r, fr) = GBinop (fo /= (fl /= fr)) BXor l r
hmAnd False False = flip GBinop BAnd
hmAnd True True = flip GBinop BOr . not
hmAnd False True = flip GBinop BImpl . not
hmAnd True False = flip . flip GBinop BImpl . not
hmMux f0 f1 (s, True) = flip $ hmMux1 f1 f0 s
hmMux f0 f1 (s, False) = hmMux1 f0 f1 s
hmMux1 f0 f1 s (d0, f0') (d1, f1') = GMux (f0 /= f0') (f1 /= f1') s d0 d1
