{-# LANGUAGE DeriveGeneric, DeriveAnyClass, TupleSections #-}
{-# LANGUAGE PatternSynonyms, FlexibleContexts #-}

-- TODO: Implement composition.
module GB.Util.NetList (NetList(..), listify, showNL,
                        Gate(..), Binop(..), SR,
                        pattern GAnd, pattern GNand,
                        pattern GOr, pattern GNor,
                        pattern GXor, pattern GIff,
                        pattern GImpl, pattern GNimpl,
                        pattern MuxS, pattern MuxNS,
                        pattern MuxSN, pattern MuxN,
                        pattern High, pattern Low,
                        pattern Id, pattern Not) where
import GB.Util.Base
import GB.Lava.Signal
import GB.Lava.Netlist
import Control.DeepSeq
import Control.Monad.State
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import Data.Functor
import Control.Applicative
import Data.Foldable
import Control.Arrow
import Control.Monad
import Data.Function
import Data.Maybe hiding (mapMaybe)
import GHC.Generics

data NetList = NetList { inputs :: [(String, Int)],
                         outputs :: [(String, [Gate])],
                         gates :: IntMap Gate }
             deriving (Generic, NFData, Show)


type SR = (Maybe String, Int)

data Binop = BAnd | BOr | BXor | BImpl
           deriving (Eq, Generic, NFData, Show)

data Gate = GConst Bool
          | GUnop Bool SR
          | GBinop Bool Binop SR SR -- negafter
          | GMux Bool Bool SR SR SR -- neg d0, neg d1, s, d0, d1
          | GDelay Bool Bool SR SR -- neg clock, neg data, clock, data
          | GDelayZ Bool Bool Bool Bool SR SR SR -- clock, zero, data
          -- on rise or fall, q or qbar, z ah/al, neg data
          | GDff Bool Bool Bool SR SR SR -- clock, write, data
          | GDffZ Bool Bool Bool Bool Bool SR SR SR SR
          -- clock, write, zero, data
          -- r/f, q/qbar, neg write, z ah/al, neg data
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

{-# COMPLETE GDelay, GDelayZ, GDff, GDffZ, High, Low, GUnop, GMux, GBinop #-}
{-# COMPLETE GDelay, GDelayZ, GDff, GDffZ, GConst, Id, Not, GMux, GBinop #-}
{-# COMPLETE GDelay, GDelayZ, GDff, GDffZ, High, Low, Id, Not, GMux, GBinop #-}
{-# COMPLETE GDelay, GDelayZ, GDff, GDffZ, GConst, GUnop, MuxS, MuxNS, MuxSN, MuxN, GBinop #-}
{-# COMPLETE GDelay, GDelayZ, GDff, GDffZ, High, Low, GUnop, MuxS, MuxNS, MuxSN, MuxN, GBinop #-}
{-# COMPLETE GDelay, GDelayZ, GDff, GDffZ, GConst, Id, Not, MuxS, MuxNS, MuxSN, MuxN, GBinop #-}
{-# COMPLETE GDelay, GDelayZ, GDff, GDffZ, High, Low, Id, Not, MuxS, MuxNS, MuxSN, MuxN, GBinop #-}
{-# COMPLETE GDelay, GDelayZ, GDff, GDffZ, GConst, GUnop, GMuxS, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE GDelay, GDelayZ, GDff, GDffZ, High, Low, GUnop, GMuxS, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE GDelay, GDelayZ, GDff, GDffZ, GConst, Id, Not, GMuxS, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE GDelay, GDelayZ, GDff, GDffZ, High, Low, Id, Not, GMuxS, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE GDelay, GDelayZ, GDff, GDffZ, GConst, GUnop, MuxS, MuxNS, MuxSN, MuxN, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE GDelay, GDelayZ, GDff, GDffZ, High, Low, GUnop, MuxS, MuxNS, MuxSN, MuxN, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE GDelay, GDelayZ, GDff, GDffZ, GConst, Id, Not, MuxS, MuxNS, MuxSN, MuxN, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}
{-# COMPLETE GDelay, GDelayZ, GDff, GDffZ, High, Low, Id, Not, MuxS, MuxNS, MuxSN, MuxN, GAnd, GOr, GNand, GNor, GXor, GIff, GImpl, GNimpl #-}

showNL :: NetList -> String -> String
listify :: (LavaGen a, LavaGen b) =>
           (Fixup a -> Fixup b) -> a -> b -> NetList

wires :: Gate -> [SR]
wires g = case g of
  GConst _ -> []
  GUnop _ x -> [x]
  GBinop _ _ x y -> [x, y]
  GMux _ _ s d0 d1 -> [s, d0, d1]
  GDelay _ _ c d -> [c, d]
  GDelayZ _ _ _ _ c z d -> [c, z, d]
  GDff _ _ _ c w d -> [c, w, d]
  GDffZ _ _ _ _ _ c w z d -> [c, w, z, d]

detOuts :: [(String, Int)] -> [((String, Int), Int)] -> [(String, [Gate])]
detGates :: [(Int, Sig Int)] -> IntMap

listify f a b = NetList (sigs a) (detOuts (sigs b) outs) (detGates gates) where
  (gates, outs) = netlist f a b

showPort :: String -> (String, Int) -> String
showSignal :: Int -> String
dumpComponent :: (SR, Gate) -> String
dumpFancy :: (String, [Gate]) -> [String]

showNL (NetList i o g) n = unlines $
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
showGateType (MuxS _ _ _) = "mux2"
showGateType (MuxNS _ _ _) = "muxnk"
showGateType (MuxSN _ _ _) = "muxkn"
showGateType (MuxN _ _ _) = "muxn"
showGateType High = "vdd"
showGateType Low = "gnd"
showGateType (Id _) = "wire"
showGateType (Not _) = "invG"
showGateType (GDelay c d _ _) = "del_" ++ if' c "fall" "rise" ++
                                if' d "_inv" ""
showGateType (GDff c w d _ _ _) = "dff_" ++ if' w 'l' 'h' :
                                  if' c "_fall" "_rise" ++
                                  if' d "_inv" ""
showGateType (GDelayZ c q z d _ _ _) = "delZ_" ++ if' c "fall" "rise" ++
                                       "_z" ++ if' z 'h' 'l' :
                                       if' d "_neg" "" ++
                                       if' q "_inv" ""
showGateType (GDffZ c q w z d _ _ _ _) = "delZ_" ++ if' w 'l' 'h' : '_' :
                                         if' c "fall" "rise" ++ "_z" ++
                                         if' z 'h' 'l' :
                                         if' d "_neg" "" ++
                                         if' q "_inv" ""
                                       
                                
