module GB.CPU.Memory.HMem (hMem, HMemSpec, memMultiplexer, memReq) where

import GB.Util.Base
import Data.Array
import Data.Word
import Control.Monad
import Control.Arrow (second)
import GB.Lava.Signal

type HMemSpec = (Word8, Array Word8 [Signal] -> Signal ->
                 Signal -> Signal -> Signal -> Signal -> [Signal] ->
                 ([Signal], [Signal]))

type MemReq = ([Signal], [Signal], Signal, Signal)

hMem :: [HMemSpec] -> Signal -> Signal -> Signal -> [Signal] -> Signal ->
        Signal -> [Signal] -> ([[Signal]], Array Word8 [Signal])
-- replacements, CGB mode, co, ci, waddr[8], w, zero, wdata[8], results

-- replacements take extra data, CGB, co, ci, w, zero, wdata[8]

hMem repl cgb co ci waddr w z wdata = outs where
  outs = second (listArray (0,255)) $ unzip $
         zipWith passin (demux w waddr) $ elems $
         accum (const id) (listArray (0,255) $ replicate 256 reg) repl
  passin w' f = f (snd outs) cgb co ci w' z wdata
  reg _ _ co ci w _ wd = (register co ci w wd, [])

demux :: Signal -> [Signal] -> [Signal]
demux = foldl demux' . (:[])
demux' :: [Signal] -> Signal -> [Signal]
demux' ws a = ws >>= flip (map . flip id) [(neg a &-&), (a &-&)]

memMultiplexer :: MemReq -> MemReq -> MemReq
-- priority comes second

memMultiplexer (a0, d0, r0, w0) (a1, d1, r1, w1) = (a, d, r, w) where
  a = zipWith (mux2 s) a0 a1
  d = zipWith (mux2 s) d0 d1
  r = r0 &-& neg w1 |-| r1
  w = w0 &-& neg r1 |-| w1
  s = w1 |-| r1
