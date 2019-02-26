{-# LANGUAGE OverloadedStrings, TupleSections #-}
module GB.CPU.CoreShim (cpuShim, shimmedCPU,
                        cpuShimChunk, shimmedCPUChunk) where

import GB.Lava.Signal
import GB.Util.Base
import GB.CPU.Core
import GB.CPU.Core.Decoder
import GB.Util.NetList
import GB.Util.Clean
import GB.Util.CombSimp
import Control.Arrow
import Data.Text
import Control.Monad.State
import Data.Tuple
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)

cpuShim :: [Signal] -> Signal -> [Signal] -> Signal -> Signal -> Signal ->
           CPUOutputs -> ((CPUInputs Signal, (Signal, Signal, Signal)),
                          ([Signal], [Signal], Signal, Signal,
                           Signal, Signal, Signal))
-- data ext, irq, iadd, co, ci, rs, cpu core out
-- output is: ((CPU core in, CPU core co, CPU core ci, CPU core reset),
--             (address bus, data bus, read, write,
--              in halt, in stop, interrupt service))

cpuShim edata irq iadd co ci rs (CPUOutputs pc ma mw wt ih is iserv) =
  ((CPUInputs ibuf dbuf irq iadd, (c20, c32, rs)),
    (abus, dbus, neg s0, st2 &-& wt, ih, is, iserv)) where
  ibuf = dff st0 <$> edata
  dbuf = dff st1 <$> edata
  dbus = mw
  abus = zipWith (mux2 $ delay st0) ma pc
  [s1, s0] = registerAWz co ci rs [s0, neg s1]
  c20 = fallingEdge s1
  c32 = fallingEdge s0
  st0 = s1 |!| s0
  st1 = neg s1 &-& s0
  st2 = s1 &-& neg s0

cpuShimParens ::
  ([Signal], Signal, [Signal], Signal, Signal, Signal,
    ([Signal], [Signal], [Signal], Signal, Signal, Signal, Signal)) ->
  ((([Signal], [Signal], Signal, [Signal]), (Signal, Signal, Signal)),
   ([Signal], [Signal], Signal, Signal, Signal, Signal, Signal))

cpuShimInput (edata, irq, iadd, co, ci, rs, (pc, ma, mw, wt, ih, is, iserv)) =
  cpuShim edata irq iadd co ci rs $ CPUOutputs pc ma mw wt ih is iserv

cpuShimParens = first (first futzInp) . cpuShimInput where
  futzInp (CPUInputs i d irq iadd) = (i, d, irq, iadd)

shimmedCPU :: [Signal] -> Signal -> [Signal] -> Signal -> Signal -> Signal ->
              ([Signal], [Signal], Signal, Signal, Signal, Signal, Signal)
shimmedCPU edata irq iadd co ci rs = snd out where
  out = cpuShim edata irq iadd co ci rs $ fullCPUCore cpui cpuco cpuci cpurs
  (cpui, cpuco, cpuci, cpurs) = fst out

cpuShimChunk :: NetList
cpuShimChunk = cleanNLWith [simplifyCombNL] $ listify cpuShimParens
               (varList "dbus_i" 8, varSing "irq", varList "iadd" 3,
                varSing "co", varSing "ci", varSing "rs",
                (varList "cpu_pc" 16, varList "cpu_ma" 16, varList "cpu_mw" 8,
                 varSing "cpu_wt", varSing "cpu_ih", varSing "cpu_is",
                 varSing "cpu_intsrv"))
               (((varList "cpu_instr" 8, varList "cpu_memR" 8,
                  varSing "cpu_irq", varList "cpu_ivec" 3),
                  (varSing "cpu_co", varSing "cpu_ci", varSing "cpu_rs")),
                (varList "abus" 16, varList "dbus_o" 8, varSing "rd",
                 varSing "wt", varSing "halted", varSing "stopped",
                 varSing "interrupted"))

addSigS :: Int -> State NetList Int
addSigS = state . (swap .) . addSignal
addInS :: Text -> Int -> State NetList ()
addInS = (modify' .) . addInput
addOutS :: Text -> Int -> State NetList ()
addOutS = (modify' .) . addOutput
setOutS :: Text -> Int -> NLP -> Bool -> State NetList ()
setOutS = (((modify' .) .) .) . setOutput
setOutsS :: Map Text (IntMap (NLP, Bool)) -> State NetList ()
setOutsS = modify' . setOutputs
incS :: (Text -> [NLP]) -> NetList -> State NetList ()
incS = (modify' .) . incorporate . (fmap Just .)

dropList :: Int -> [Int]
dropList n = [n - 1, n - 2 .. 0]

prep :: State NetList (Map Text [Maybe NLP])

incDffZPE :: Int -> Bool -> Bool -> Bool ->
                   Text -> Text -> [NLP] -> [NLP] -> State NetList ()
incDffZPE n fw fz zs w z d q =
  incS (fromJust . flip lookup [("w", [(Right w, 0)]), ("z", [(Right z, 0)]),
                                ("d", d), ("q", q)]) $ nlDffZ n fw fz zs
incDffZ :: Bool -> Bool -> Bool -> Text -> Text -> [NLP] -> State NetList Int
incDffZ n fw fz zs w z d = do
  let n = length d
  q <- addSigS n
  incDffZPE n fw fz zs w z d $ (Left q,) <$> dropList n
  return q
  
incUnopPE :: Int -> Bool -> [NLP] -> [NLP] -> State NetList ()
incUnopPE n f x o =
  incS (fromJust . flip lookup [("x", x), ("o", o)]) $ nlUnop n f
incUnop :: Bool -> [NLP] -> State NetList Int
incUnop f x = do
  let n = length x
  o <- addSigS n
  incUnopPE n f x $ (Left o,) <$> dropList n
  return o

incBinopPE :: Int -> Bool -> Binop -> [NLP] -> [NLP] -> [NLP] ->
              State NetList ()
incBinopPE n f op l r o =
  incS (fromJust . flip lookup [("l", l), ("r", r), ("o", o)]) $
  nlBinop n f op
incBinop :: Int -> Bool -> Binop -> [NLP] -> [NLP] -> State NetList Int
incBinop n f op l r = do
  o <- addSigS n
  incBinopPE n f op l r $ (Left o,) <$> dropList n
  return o

incDff :: Int -> Bool -> NLP -> Either Int Text ->
          State NetList Int
incDff n fw w d = do
  q <- addSigS n
  incS (fromJust . flip lookup [("w", [w]), ("d", f d),
                                ("q", f $ Left q)]) $ nlDff n fw
  return q
  where
  f x = (x,) <$> dropList n

incMuxPE :: Int -> Bool -> Bool -> NLP -> [NLP] -> [NLP] -> [NLP] ->
            State NetList ()
incMuxPE n f0 f1 s d0 d1 o =
  incS (fromJust . flip lookup [("s", [s]), ("d0", d0), ("d1", d1),
                                ("o", o)]) $
  nlMux n f0 f1
incDelay :: [NLP] -> State NetList Int
incDelay i = do
  let n = length i
  o <- addSigS n
  incS (fromJust . flip lookup [("i", i), ("o", (Left o,) <$> dropList n)]) $
    nlDelay n

{-
cpuShim edata irq iadd co ci rs (CPUOutputs pc ma mw wt ih is iserv) =
  ((CPUInputs ibuf dbuf irq iadd, (c20, c32, rs)),
    (abus, dbus, neg s0, st2 &-& wt, ih, is, iserv)) where
  ibuf = dff st0 <$> edata
  dbuf = dff st1 <$> edata
  dbus = mw
  abus = zipWith (mux2 $ delay st0) ma pc
  [s1, s0] = registerAWz co ci rs [s0, neg s1]
  c20 = fallingEdge s1
  c32 = fallingEdge s0
  st0 = s1 |!| s0
  st1 = neg s1 &-& s0
  st2 = s1 &-& neg s0
-}

prep = do
  addInS "dbus_i" 8
  addInS "irq" 1
  addInS "ivec" 3
  addInS "co" 1
  addInS "ci" 1
  addInS "rs" 1
  addOutS "abus" 16
  addOutS "dbus_o" 8
  addOutS "rd" 1
  addOutS "wt" 1
  addOutS "halted" 1
  addOutS "stopped" 1
  addOutS "interrupted" 1
  svec' <- addSigS 2
  let f n x = (x,) <$> dropList n
  let g n x = Just . (x,) <$> dropList n
  svec <- incDffZ False False False "co" "rs" $ f 2 $ Left svec'
  svec'' <- incUnop True [(Left svec, 1)]
  incDffZPE 2 False False False "ci" "rs" ((,0) . Left <$> [svec, svec'']) $
    f 2 $ Left svec'
  st <- addSigS 3
  incBinopPE 1 True BOr [(Left svec, 1)] [(Left svec, 0)] [(Left st, 0)]
  incBinopPE 1 True BImpl [(Left svec, 0)] [(Left svec, 1)] [(Left st, 1)]
  incBinopPE 1 True BImpl [(Left svec', 1)] [(Left svec', 0)] [(Left st, 2)]
  ibuf <- incDff 8 False (Left st, 0) (Right "dbus_i")
  dbuf <- incDff 8 False (Left st, 1) (Right "dbus_i")
  dst0 <- incDelay [(Left st, 0)]
  cpuMA <- addSigS 16
  cpuPC <- addSigS 16
  incMuxPE 16 False False (Left dst0, 0) (f 16 $ Left cpuMA)
    (f 16 $ Left cpuPC) (f 16 $ Right "abus")
  ck <- incBinop 2 True BImpl (f 2 $ Left svec) (f 2 $ Left svec')
  cpuWT <- addSigS 1
  incBinopPE 1 False BAnd [(Left st, 2)] [(Left cpuWT, 0)] [(Right "wt", 0)]
  setOutS "rd" 0 (Left svec, 0) True
  return $ M.fromDistinctAscList
    [("clocki", [Just (Left ck, 1)]),
     ("clocko", [Just (Left ck, 0)]),
     ("halted", g 1 $ Right "halted"),
     ("instr", g 8 $ Left ibuf),
     ("interruptServiced", g 8 $ Right "interrupted")
     ("irq", g 1 $ Right "irq"),
     ("ivec", g 3 $ Right "ivec"),
     ("memA", g 16 $ Left cpuMA),
     ("memR", g 8 $ Left dbuf),
     ("memW", g 8 $ Right "dbus_o"),
     ("pc", g 16 $ Left cpuPC),
     ("reset", g 1 $ Right "rs"),
     ("stopped", g 1 $ Right "stopped"),
     ("wt", g 1 $ Left cpuWT)]

shimmedCPUChunk :: NetList
shimmedCPUChunk = cleanNLWith [simplifyCombNL] $ removeSignals $
                  incorporate (coreMap M.!) cpuCoreChunk $
                  shimReady where
  (coreMap, shimReady) = runState prep nlEmpty
