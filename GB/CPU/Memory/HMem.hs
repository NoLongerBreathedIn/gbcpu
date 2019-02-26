module GB.Memory.HMem (hMem, HMemSpec) where

import GB.Util.Base
import Data.Array
import Data.Word
import Control.Monad
import Control.Arrow (second)
import GB.Lava.Signal

type HMemSpec = (Word8, Array Word8 [Signal] -> Signal ->
                 Signal -> Signal -> Signal -> Signal -> [Signal] ->
                 ([Signal], [Signal]))


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
