module GB.CPU.CoreShim (cpuShim, shimmedCPU) where

import GB.Lava.Signal
import GB.Util.Base
import GB.CPU.Core
import GB.CPU.Core.Decoder

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
    (abus, dbus, neg s0, delay st3 &-& wt, ih, is, iserv)) where
  ibuf = dff st0 <$> edata
  dbuf = dff st1 <$> edata
  dbus = mw
  abus = zipWith (mux2 $ delay st0) ma pc
  [s1, s0] = registerAWz co ci rs [s0, neg s1]
  c20 = fallingEdge s1
  c32 = fallingEdge s0
  st0 = s0 |!| s1
  st1 = s0 &&! s1
  st3 = s0 &-& s1

shimmedCPU :: [Signal] -> Signal -> [Signal] -> Signal -> Signal -> Signal ->
              ([Signal], [Signal], Signal, Signal, Signal, Signal, Signal)
shimmedCPU edata irq iadd co ci rs = snd out where
  out = cpuShim edata irq iadd co ci rs $ fullCPUCore cpui cpuco cpuci cpurs
  (cpui, cpuco, cpuci, cpurs) = fst out
