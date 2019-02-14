{-# LANGUAGE TupleSections #-}
module GB.Util.CombSimp (simplifyComb, simplifyCombNL) where
import qualified Data.IntMap.Lazy as IM
import Data.IntMap.Lazy (IntMap)
import GB.Util.Base
import GB.Util.NetList
import GB.Util.Clean 
import GB.Util.Symbolic
import Data.Function
import Data.Traversable
import Data.Monoid
import Control.Monad
import Control.Arrow (second, Kleisli(..))
import Data.Maybe

simplifyComb :: IntMap Gate -> Maybe (IntMap Gate)
simplifyCombNL :: NL Gate -> Maybe (NL Gate)

simplifyCombNL = fmap NL . runKleisli (second $ Kleisli simplifyComb) . getNL

type DffP = (Maybe (Maybe (Bool, Bool, Maybe SR), Bool, SR), SR)

computeReps :: IntMap Gate -> IntMap (Symb (Either DffP SR))

look :: IntMap (Symb (Either a SR)) -> SR -> Symb (Either a SR)

computeReps = fix . flip (IM.map . compGate) where
  hBin BAnd = (&-&)
  hBin BOr = (|-|)
  hBin BXor = (^-^)
  hBin BImpl = (!||)
  compGate _ (GConst x) = Inj x
  compGate m (GUnop f x) = Inj f ^-^ look m x
  compGate m (GBinop f o x y) = Inj f ^-^ on (hBin o) (look m) x y
  compGate m (GMux f0 f1 s d0 d1) =
    mux2 (look m s) (Inj f0 ^-^ look m d0) (Inj f1 ^-^ look m d1)
  compGate _ (GDff fw w d) = Symb $ Left (Just (Nothing, fw, w), d)
  compGate _ (GDffZ fw fz zs w z d) =
    Symb $ Left (Just (Just (fz, zs, Just z), fw, w), d)
  compGate _ (GSR fs fr fq s r) =
    Symb $ Left (Just (Just (fr, fq, Nothing), fs, s), r)
  compGate _ (GDelay x) = Symb $ Left (Nothing, x)

simpGate :: (Eq a) => IntMap (Symb (Either a SR)) -> Gate -> Maybe Gate

simpWith :: (a -> Maybe a) -> IntMap a -> Maybe (IntMap a)
maybeToBool :: a -> Maybe a -> (Any, a)
boolToMaybe :: (Any, a) -> Maybe a

which :: a -> a -> Bool -> a
which a _ True = a
which _ a False = a

maybeToBool = flip maybe (Any True,) . pure
boolToMaybe = uncurry $ which Just (const Nothing) . getAny

simpWith = (boolToMaybe .) . traverse . ap maybeToBool

simplifyComb = simpWith =<< simpGate . computeReps

look m (Nothing, x) = m IM.! x
look _ x = Symb $ Right x

simpBOp :: (Eq a) => Binop -> Bool -> Symb a -> Symb a ->
           SR -> SR -> Maybe Gate
simpMux :: (Eq a) => Bool -> Bool -> Symb a -> Symb a -> Symb a ->
           SR -> SR -> SR -> Maybe Gate

simpDff :: (Eq a) => Bool -> Symb a -> Symb a -> SR -> SR -> Maybe Gate
simpDffZ :: (Eq a) => Bool -> Bool -> Bool ->
            Symb a -> Symb a -> Symb a -> SR -> SR -> SR -> Maybe Gate
simpSR :: (Eq a) => Bool -> Bool -> Bool -> Symb a -> Symb a -> SR -> SR ->
          Maybe Gate

simpGate m (GBinop f o l r) = simpBOp o f (look m l) (look m r) l r
simpGate m (GMux f0 f1 s d0 d1) =
  simpMux f0 f1 (look m s) (look m d0) (look m d1) s d0 d1
simpGate m (GDff fw w d) = simpDff fw (look m w) (look m d) w d
simpGate m (GDffZ fw fz zs w z d) =
  simpDffZ fw fz zs (look m w) (look m z) (look m d) w z d
simpGate m (GSR fs fr fq s r) =
  simpSR fs fr fq (look m s) (look m r) s r
simpGate _ _ = Nothing

simpBAnd :: (Eq a) => Bool -> Bool -> Bool -> Symb a -> Symb a ->
            SR -> SR -> Maybe Gate
-- fl, fr, fres
simpBXor :: (Eq a) => Bool -> Symb a -> Symb a -> SR -> SR -> Maybe Gate
simpBOp BAnd = simpBAnd False False
simpBOp BOr = simpBAnd True True . not
simpBOp BImpl = simpBAnd False True . not
simpBOp BXor = simpBXor

simpBAnd fl fr f ll rr l r
  | equivalent (ll' &-& rr') (Inj True) = Just $ GConst $ not f
  | equivalent (ll' &-& rr') (Inj False) = Just $ GConst f
  | equivalent (ll' !|| rr') (Inj True) = Just $ GUnop (f /= fl) l
  | equivalent (rr' !|| ll') (Inj True) = Just $ GUnop (f /= fr) r
  | otherwise = Nothing
  where ll' = if fl then neg ll else ll
        rr' = if fr then neg rr else rr

decAnd :: Bool -> Bool -> Bool -> SR -> SR -> Gate
decAnd False False = flip GBinop BAnd
decAnd True True = flip GBinop BOr . not
decAnd False True = flip GBinop BImpl . not
decAnd True False = flip . flip GBinop BImpl . not

psimpBAnd fl fr f ll rr l r = fromMaybe (decAnd fl fr f l r) $
  simpBAnd fl fr f ll rr l r

simpBXor f ll rr l r
  | equivalent (ll &-& rr) (Inj False) =
      Just $ psimpBAnd True True (not f) ll rr l r
  | equivalent (ll !|| rr) (Inj True) =
      Just $ psimpBAnd True False f ll rr l r
  | equivalent (rr !|| ll) (Inj True) =
      Just $ psimpBAnd False True f ll rr l r
  | equivalent (ll |-| rr) (Inj True) =
      Just $ psimpBAnd False False (not f) ll rr l r
  | otherwise = Nothing

psimpBXor f ll rr l r = fromMaybe (GBinop f BXor l r) $ simpBXor f ll rr l r

simpMux f0 f1 ss dd0 dd1 s d0 d1
  | equivalent (ddf !|| ss) (Inj True) =
      Just $ GUnop f1 d1
  | equivalent (ddf &-& ss) (Inj False) =
      Just $ GUnop f0 d0
  | equivalent (ss |-| dd0') (Inj True) =
      Just $ psimpBAnd False (not f1) True ss dd1 s d1
  | equivalent (dd0' !|| ss) (Inj True) =
      Just $ psimpBAnd False f1 False ss dd1 s d1
  | equivalent (ss !|| dd1') (Inj True) =
      Just $ psimpBAnd True (not f0) True ss dd0 s d0
  | equivalent (ss &-& dd1') (Inj False) =
      Just $ psimpBAnd True f0 False ss dd0 s d0
  | equivalent (ddf |-| ss) (Inj True) =
      Just $ psimpBXor True ss dd1 s d1
  | equivalent (ss !|| ddf) (Inj True) =
      Just $ psimpBXor False ss dd0 s d0
  | equivalent (ss ^-^ dd1' |-| dd0' &-& dd1') (Inj True) =
      Just $ psimpBAnd f0 f1 False dd0 dd1 d0 d1
  | equivalent ((ss ^-^ dd1') &-& (dd0' |-| dd1')) (Inj False) =
      Just $ psimpBAnd (not f0) (not f1) True dd0 dd1 d0 d1
  | equivalent (ss ^-^ dd0' !|| dd0' &-& dd1') (Inj True) =
      Just $ psimpBAnd f0 f1 False dd0 dd1 d0 d1
  | equivalent (dd0' |-| dd1' !|| ss ^-^ dd0') (Inj True) =
      Just $ psimpBAnd (not f0) (not f1) True dd0 dd1 d0 d1
  | otherwise = Nothing
  where
    ddf = dd0' ^-^ dd1'
    dd0' = if f0 then neg dd0 else dd0
    dd1' = if f1 then neg dd1 else dd1

simpDff fw ww dd w d
  | equivalent (ww' !|| dd) (Inj True) =
      Just $ GConst True
  | equivalent (ww' &-& dd) (Inj False) =
      Just $ GConst False
  | otherwise = Nothing
  where
    ww' = if fw then neg ww else ww

psimpDff fw ww dd w d = fromMaybe (GDff fw w d) $ simpDff fw ww dd w d

psimpSR fs fr fq ss rr s r = fromMaybe (GSR fs fr fq s r) $
                             simpSR fs fr fq ss rr s r

simpDffZ fw fz zs ww zz dd w z d
  | equivalent (zz' &-& dd' &-& ww') (Inj False) =
      Just $ GConst zs
  | equivalent (zz' !|| ww') (Inj True) =
      Just $ psimpBAnd fz zs zs zz dd z d
  | equivalent (zz' &-& ww' !|| dd') (Inj True) =
      Just $ psimpSR fw fz zs ww zz w z
  | otherwise = Nothing
  where
    zz' = if fz then neg zz else zz
    ww' = if fw then neg ww else ww
    dd' = if zs then neg dd else dd

simpSR fs fr fq ss rr s r
  | equivalent (ss' |-| rr') (Inj True) =
      Just $ GUnop (fq == fr) r
  | equivalent (ss' !|| rr') (Inj True) =
      Just $ GConst fq
  | otherwise = Nothing
  where
    ss' = if fs then neg ss else ss
    rr' = if fr then neg rr else rr
