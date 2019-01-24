{-# LANGUAGE TupleSections #-}
module GB.Util.CombSimp (simplifyComb) where
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import GB.Util.Base
import GB.Util.Clean
import GB.Util.Symbolic
import Data.Function
import Data.Traversable
import Data.Monoid
import Control.Monad

simplifyComb :: IntMap Gate -> Maybe (IntMap Gate)

computeReps :: IntMap Gate -> IntMap (Symb SR)

look :: IntMap (Symb SR) -> SR -> Symb SR

computeReps = fix . flip (IM.map . compGate) where
  hBin BAnd = (&&&)
  hBin BOr = (|||)
  hBin BXor = (^^^)
  hBin BImpl = (!||)
  compGate _ (GConst x) = Inj x
  compGate m (GUnop f x) = Inj f ^^^ look m x
  compGate m (GBinop f o x y) = Inj f ^^^ hBin o (look m x) (look m y)
  compGate m (GMux f0 f1 s d0 d1) = mux2 s (Inj f0 ^^^ d0) (Inj f1 ^^^ d1)

simpGate :: IntMap (Symb SR) -> Gate -> Maybe Gate

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

look m = uncurry $ maybe (m IM.!) (curry Symb . Just)

simpBOp :: (Eq a) => Binop -> Bool -> Symb a -> Symb a ->
           SR -> SR -> Maybe Gate
simpMux :: (Eq a) => Bool -> Bool -> Symb a -> Symb a -> Symb a ->
           SR -> SR -> SR -> Maybe Gate

simpGate m (GBinop f o l r) = simpBOp o f (look m l) (look m r) l r
simpGate m (GMux f0 f1 s d0 d1) =
  simpMux f0 f1 (look m s) (look m d0) (look m d1)
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
  | equivalent (ll' &&& rr') (Inj True) = Just $ GConst $ not f
  | equivalent (ll' &&& rr') (Inj False) = Just $ GConst f
  | equivalent (ll' !|| rr') (Inj True) = Just $ GUnop (f /= fl) l
  | equivalent (rr' !|| ll') (Inj True) = Just $ GUnop (f /= fr) r
  | otherwise = Nothing
  where ll' = Inj fl ^^^ ll
        rr' = Inj fr ^^^ rr

decAnd :: Bool -> Bool -> Bool -> SR -> SR -> Gate
decAnd False False = flip GBinop BAnd
decAnd True True = flip GBinop BOr . not
decAnd False True = flip GBinop BImpl . not
decAnd True False = flip . flip GBinop BImpl . not

psimpBAnd fl fr f ll rr l r = maybe (decAnd fl fr f l r) id $
  simpBAnd fl fr f ll rr l r

simpBXor f ll rr l r
  | equivalent (ll &&& rr) (Inj False) =
      Just $ psimpBAnd True True (not f) ll rr l r
  | equivalent (ll !|| rr) (Inj True) =
      Just $ psimpBAnd True False f ll rr l r
  | equivalent (rr !|| ll) (Inj True) =
      Just $ psimpBAnd False True f ll rr l r
  | equivalent (ll ||| rr) (Inj True) =
      Just $ psimpBAnd False False (not f) ll rr l r
  | otherwise = Nothing

psimpBXor f ll rr l r = maybe (GBinop f BXor l r) id $ simpBXor f ll rr l r

simpMux f0 f1 ss dd0 dd1 s d0 d1 =
  | equivalent (ddf !|| ss) (Inj True) =
      Just $ GUnop f1 d1
  | equivalent (ddf &&& ss) (Inj False) =
      Just $ GUnop f0 d0
  | equivalent (ss ||| dd0') (Inj True) =
      Just $ psimpBAnd False (not f1) True ss dd1 s d1
  | equivalent (dd0' !|| ss) (Inj True) =
      Just $ psimpBAnd False f1 False ss dd1 s d1
  | equivalent (ss !|| dd1) (Inj True) =
      Just $ psimpBAnd True (not f0) True ss dd0 s d0
  | equivalent (ss &&& d1) (Inj False) =
      Just $ psimpBAnd True f0 False ss dd0 s d0
  | equivalent (ddf ||| ss) (Inj True) =
      Just $ psimpBXor True ss dd1 s d1
  | equivalent (ss !|| ddf) (Inj True) =
      Just $ psimpBXor False ss dd0 s d0
  | equivalent (ss ^^^ dd1' ||| dd0' &&& dd1') (Inj True) =
      Just $ psimpBAnd f0 f1 False dd0 dd1 d0 d1
  | equivalent ((ss ^^^ dd1') &&& (dd0' ||| dd1')) (Inj False) =
      Just $ psimpBAnd (not f0) (not f1) True dd0 dd1 d0 d1
  | equivalent (ss ^^^ dd0' !|| dd0' &&& dd1') (Inj True) =
      Just $ psimpBAnd f0 f1 False dd0 dd1 d0 d1
  | equivalent (dd0' ||| dd1' !|| ss ^^^ dd0') (Inj True) =
      Just $ psimpBAnd (not f0) (not f1) True dd0 dd1 d0 d1
  | otherwise = Nothing
  where
    ddf = dd0' ^^^ dd1'
    dd0' = if f0 then neg dd0 else dd0
    dd1' = if f1 then neg dd1 else dd1
