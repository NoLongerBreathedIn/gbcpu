module GB.CPU.Memory.IOShim (cpuIOShim, MemReq, memMultiplexer,
                             ioShimmedCPU,
                             IOReqs(..), IOResps(..)) where

import GB.Lava.Signal
import GB.Util.Base
import GB.CPU.CoreShim

type MemReq = ([Signal], [Signal], Signal, Signal)

data IOReqs = IOReqs
data IOResps = IOResps

memMultiplexer :: MemReq -> MemReq -> MemReq
-- priority comes second

memMultiplexer (a0, d0, r0, w0) (a1, d1, r1, w1) = (a, d, r, w) where
  a = zipWith (mux2 s) a0 a1
  d = zipWith (mux2 s) d0 d1
  r = r0 &-& neg w1 |-| r1
  w = w0 &-& neg r1 |-| w1
  s = w1 |-| r1

cpuIOShim :: Signal -> Signal -> Signal -> Signal -> Signal -> Signal ->
             MemReq -> [Signal] -> [Signal] -> [Signal] -> IOResps ->
             (IOReqs, MemReq, MemReq, MemReq, [Signal], Signal,
              [Signal], Signal)
-- co ci fco fci rs isvc cpu_mem_request mem_response vram_response
-- himem_response io_responses
-- returns:
-- (io_requests, mem_request, vram_request, himem_request, cpu_mem_resp,
-- halt_cpu, ivec, irq)
-- mem_request has 16-bit address; vram has 13-bit address, himem has 7-bit
-- address

ioShimmedCPU :: Signal -> Signal -> Signal -> Signal -> Signal ->
                [Signal] -> [Signal] -> [Signal] -> IOResps ->
                (IOReqs, MemReq, MemReq, MemReq, Signal)

ioShimmedCPU co ci fco fci rs ds mds hds resps = (reqs, dr, mdr, hdr, ins)
  where
    (reqs, dr, mdr, hdr, cds, hcpu, ivec, irq) =
      cpuIOShim co ci fco fci rs isvc cdr ds mds hds resps
    hcpu' = inh |!| hcpu
    co' = co &-& hcpu'
    ci' = ci &-& hcpu'
    cdr = (abuf, dbuf, rd, wt)
    (abuf, dbuf, rd, wt, inh, ins, isvc) = shimmedCPU cds irq ivec co' ci' rs

cpuIOShim = undefined


{-
joypad buttons_out =
  (0, \_ _ co ci w _ wdata ->
      let es = register co ci w (take 2 $ drop 2 wdata)
      in (replicate 2 high ++ es ++ buttons_out,
          fallingEdge (ands buttons_out) : es))
-}

{-
serialControl cgb sc co ci w z wd =
  (transfer : replicate 5 high ++ [cgb !|| cksp, ckt],
    [transfin, tck, transfer, tcko, tcki]) where
  ciw = ci &-& w
  [transfer] = registerAWz co ciw (neg transfin) [head wd]
  [ckt] = registerAW co ciw $ drop 7 wd
  [cksp] = registerAWz co ciw cgb [wd !! 6]
  tcko = fallingEdge tck
  tcki = risingEdge tck
  acks = ensureLength 9 $ registerAW co ci $ zipWith (^-^) acks $
    drop 1 acks ++ [high]
  tck = transfer !|| mux2 ckt sc (mux2 cksp (head acks) (acks !! 5))
  counter = registerz tcko tcki transfer z $
            ((counter !! 2) ^-^ ((counter !! 1) !|| head counter)) :
            take 2 counter
  transfin = tcki &-& nors counter &-& transfer
-}

{-
serialRegister [_, _, transfer, tcko, tcki] si co ci w wd =
  (regOut, [head regOut]) where
  [cko, cki] = zipWith (mux2 transfer) [co, ci &-& w] [tcko, tcki]
  regOut = ensureLength 8 $ registerAW cko cki $ zipWith (mux2 transfer) wd $
           tail regOut ++ [si]
-}
