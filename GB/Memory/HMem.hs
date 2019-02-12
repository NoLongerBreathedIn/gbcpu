module GB.Memory.HMem (hMem) where

import GB.Util.Base
import Data.Array
import Data.Word
import Control.Monad

hMem :: [(Word8, Signal -> Signal -> Signal -> [Signal] -> [Signal])] ->
        Signal -> Signal -> [Signal] -> Signal -> [Signal] -> [[Signal]]
-- replacements, co, ci, waddr[8], w, wdata[8], results

-- replacements take co, ci, w, wdata[8], results

hMem repl co ci waddr w wdata = zipWith passin (demux w waddr) $ elems $
  accum (const id) (listArray (0,255) $ replicate 256 register) repl where
  passin w' f = f co ci w' wdata

demux :: Signal -> [Signal] -> [Signal]
demux w = foldl demux' . (:[])
demux' :: [Signal] -> Signal -> [Signal]
demux' ws a = ws >>= flip (map . flip id) [(not a &&&), (a &&&)]
