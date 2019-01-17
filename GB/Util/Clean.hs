{-# LANGUAGE DeriveGeneric, DeriveAnyClass, TypeFamilies, TupleSections #-}
module GB.Util.Clean (NetList, listify, showNL,
                      Fixable, SingleVar, ListVar,
                      singleVar, listVar) where
import GB.Lava.VhdlNew (writeVhdlInputOutputNoClk)
import Lava
import Control.DeepSeq
import Control.Monad.State
import System.IO.Temp
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Data.Functor
import Control.Applicative
import Data.Monoid
import Data.Traversable
import Data.Foldable
import Control.Arrow ((***))
import Control.Monad
import Data.Function
import Data.Array
import Data.Functor.Compose
import Data.Functor.Product
import qualified Data.Set as S

data NetList = NetList { name :: String,
                         inputs :: [(String, Int)],
                         outputs :: [(String, [Gate])],
                         gates :: IM.IntMap Gate } deriving (NFData, Show)

newtype SingleVar = Var {unVar :: String}
newtype ListVar = VarList {unVarList :: (String, Int)}

singleVar :: String -> SingleVar
singleVar = Var
listVar :: Int -> String -> VarList
listVar = flip (curry VarList)

type SR = (Maybe String, Int)

data Gate = High
          | Low
          | Id SR
          | Not SR
          | GAnd SR SR
          | GNand SR SR
          | GOr SR SR
          | GNor SR SR
          | GXor SR SR
          | GIff SR SR
          | GImpl SR SR
          | GNimpl SR SR
          | Mux SR SR SR -- s d0 d1
          | MuxNS SR SR SR -- s d0b d1
          | MuxSN SR SR SR -- s d0 d1b
          | MuxN SR SR SR -- s d0b d1b
          deriving (Eq, NFData)

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
listify :: (Fixable a, Fixable b, Generic (Fixup a), Generic (Fixup b)) =>
           String -> (Fixup a -> Fixup b) -> a -> b -> NetList

newtype NL g = NL { getNL :: ([(String, [g])], IM.IntMap Int Gate) }
  deriving (NFData, Show)

cleanUp :: NL Gate -> NL Gate
readNetList :: [(String, Int)] -> [String] -> NL Gate

listify name f a b = uncurry (NetList name (sigs a)) $ getNL $ force $
                     cleanUp $ readNetList (sigs b) $
                     unsafePerformIO $
                     withSystemTempFile (name ++ ".vhd") $ \fn h -> do
  hClose h
  (writeVhdlInputOutputNoClk (reverse $ drop 4 $ reverse fn) f `on` fixup) a b
  force <$> readFile fn

iterateUntilDone :: (a -> Maybe a) -> a -> a
iterateUntilDone = fix . (. flip maybe) . flip ap

iterateOnceTillDone :: (a -> Maybe a) -> a -> Maybe a
iterateOnceTillDone = ap (.) iterateUntilDone

which :: a -> a -> Bool -> a
which = flip . flip if'

strategies :: [NL Gate -> Maybe (NL Gate)]

cleanUp = iterateUntilDone (force . msum . flip (map . (&)) strategies)

handleLines :: [(String, Int)] -> [String] -> NL Gate
type HLState = (M.Map String [(Int, Gate)], IM.IntMap Gate)
handleLine :: [String] -> State HLState ()

handleLines b = hlsToNetList b .
                       flip execState (initHLS b) .
                       sequence_ . map (handleLine . words) . init

initHLS :: [(String, Int)] -> HLState
hlsToNetList :: [(String, Int)] -> HLState -> NL Gate

decodePort :: String -> SR
decodePort 'w':s = (Nothing, read s)
decodePort ss = (Just *** read . tail) splitAt brk ss where
  brk = last $ elemIndices '_' ss

initHLS = (, IM.empty) . M.fromList . map ((, []) . fst)
hlsToNetList = (NL .) .
  flip (first . (. (uncurry . (((elems .) . flip (array . (0,) . (-1 +))) .) .
                    (M.!))) . flip map)

handleLine (_:_:etype:_:_:pts) = modify' $ uncurry .
                                 maybe (second . flip IM.insert gate)
                                 ((first .) . flip (M.adjust . ((, gate):))) $
                                 last ports where
  ports = map decodePort pts
  gate = case drop 5 etype of
    "vdd" -> High
    "gnd" -> Low
    "wire" -> Id $ ports !! 0
    "invG" -> GNot $ ports !! 0
    "andG" -> GAnd (ports !! 0) (ports !! 1)
    "orG" -> GOr (ports !! 0) (ports !! 1)
    "xorG" -> GXor (ports !! 0) (ports !! 1)
    "mux2" -> Mux (ports !! 0) (ports !! 1) (ports !! 2)

handleLine _ = return ()

readNetList b =
  handleLines b . map (filter (`notElem` ", ();:")) . tail .
  dropWhile (not . isPrefixOf "begin") . lines

strategies = [simplifyGates, removeUnused, mergeCommonNots,
              iterateOnceTillDone mergeNotPre,
              removeNots . removeWires, mergeConsts, handleOutputs]

parallelSimp :: (Traversable t) => (a -> Maybe a) -> t a -> Maybe (t a)

parallelSimp = (uncurry (which Just (const Nothing) . getAny) .) .
  traverse . ap (flip maybe (Any True,) . (mempty,))

nlToWhatsit = pairUp . first (Compose . Compose . Compose)

whatsitToNL = first (getCompose . getCompose . getCompose) . unpair

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
  gateSimp (GAnd x y) | x == y = Just $ Id x
  gateSimp (GOr x y) | x == y = Just $ Id x
  gateSimp (GNand x y) | x == y = Just $ Not x
  gateSimp (GNor x y) | x == y = Just $ Not x
  gateSimp (GXor x y) | x == y = Just $ Low
  gateSimp (GIff x y) | x == y = Just $ High
  gateSimp (GImpl x y) | x == y = Just $ High
  gateSimp (GNimpl x y) | x == y = Just $ Low
  gateSimp (Mux x y z)
    | y == z = Just $ Id y
    | x == y = Just $ GAnd x z
    | x == z = Just $ GOr x y
  gateSimp (MuxNS x y z)
    | y == z = Just $ if x == y then High else Iff x y
    | x == y = Just $ GImpl x z
    | x == z = Just $ GImpl y x
  gateSimp (MuxSN x y z)
    | y == z = Just $ if x == y then Low else Xor x y
    | x == y = Just $ GNimpl x z
    | x == z = Just $ GNimpl y x
  gateSimp (MuxN x y z)
    | y == z = Just $ Not y
    | x == y = Just $ GNand x z
    | x == z = Just $ GNor x y
  gateSimp _ = Nothing

removeUnused x@(NL (out, int)) =
  if IM.null removed then Nothing else Just $ NL (out, retained) where
    retained = IM.intersection int used
    removed = int IM.\\ used
    used = foldMap (foldMap justNumbers . wires) x

wires :: Gate -> [SR]
wires g = case g of
  Id x -> [x]
  Not x -> [x]
  GAnd x y -> [x, y]
  GNand x y -> [x, y]
  GOr x y -> [x, y]
  GNor x y -> [x, y]
  GIff x y -> [x, y]
  GXor x y -> [x, y]
  GImpl x y -> [x, y]
  GNimpl x y -> [x, y]
  Mux x y z -> [x, y, z]
  MuxNS x y z -> [x, y, z]
  MuxSN x y z -> [x, y, z]
  MuxN x y z -> [x, y, z]
  _ -> []

justNumbers :: SR -> IM.IntMap ()
justNumbers = uncurry $ maybe (const IM.singleton) (const IM.empty)

mergeCommonNots = undefined

mergeNotPre = undefined
removeNots = undefined
removeWires = undefined

mergeConsts = undefined
handleOutputs = undefined

-- TODO: simplification algorithm, writing.
