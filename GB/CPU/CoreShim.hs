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
  ((CPUInputs ibuf dbuf irq iadd, (st3 &-& ci, st2 &-& ci, rs)),
    (abus, dbus, neg s0, st3 &-& wt, ih, is, iserv)) where
  ibuf = dff (st0 &-& ci) <$> edata
  dbuf = dff (st1 &-& ci) <$> edata
  dbus = mw
  abus = zipWith (mux2 st0) ma pc
  [s1, s0] = registerAWz co ci rs [s0, neg s1]
  st0 = s0 |!| s1
  st1 = s0 &&! s1
  st2 = s1 &&! s0
  st3 = s0 &-& s1

shimmedCPU :: [Signal] -> Signal -> [Signal] -> Signal -> Signal -> Signal ->
              ([Signal], [Signal], Signal, Signal, Signal, Signal, Signal)
shimmedCPU edata irq iadd co ci rs = snd out where
  out = cpuShim edata irq iadd co ci rs $ fullCPUCore cpui cpuco cpuci cpurs
  (cpui, cpuco, cpuci, cpurs) = fst out
