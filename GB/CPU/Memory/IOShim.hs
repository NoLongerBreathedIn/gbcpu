{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module GB.CPU.Memory.IOShim (cpuIOShim, MemReq, memMultiplexer,
                             ioShimmedCPU,
                             IOReqs(..), IOResps(..)) where

import GB.Lava.Signal
import GB.Util.Base
import GB.CPU.CoreShim
import Data.Foldable

type MemReq = ([Signal], [Signal], Signal, Signal)

data IOReqs = IOReqs {
  sysco :: Signal,
  sysci :: Signal,
  oamReq :: MemReq, -- 8bit address
  portReq :: MemReq, -- 3bit address
  nrReq :: MemReq, -- 5bit address
  waveReq :: MemReq, -- 4bit address from now on
  lcdReq :: MemReq,
  dmaReq :: MemReq,
  cpalReq :: MemReq
  }
data IOResps = IOResps {
  oamds :: [Signal],
  portResp :: [Signal],
  timerInt :: Signal,
  serialInt :: Signal,
  buttonInt :: Signal,
  nrResp :: [Signal],
  pcm01 :: [Signal],
  pcm23 :: [Signal],
  waveResp :: [Signal],
  lcdResp :: [Signal],
  lycInt :: Signal,
  vblankInt :: Signal,
  dmaResp :: [Signal],
  inDMA :: Signal,
  cpalResp :: [Signal],
  iereg :: [Signal]
  }

memMultiplexer :: MemReq -> MemReq -> MemReq
-- priority comes second

memMultiplexer (a0, d0, r0, w0) (a1, d1, r1, w1) = (a, d, r, w) where
  a = zipWith (mux2 s) a0 a1
  d = zipWith (mux2 s) d0 d1
  r = r0 &-& neg w1 |-| r1
  w = w0 &-& neg r1 |-| w1
  s = w1 |-| r1

cpuIOShim :: Signal -> Signal -> Signal -> Signal -> Signal -> Signal ->
             Signal -> MemReq -> [Signal] -> [Signal] -> IOResps ->
             (IOReqs, MemReq, MemReq, [Signal],  [Signal], Signal)
-- co ci fco fci rs isvc inhalt cpu_mem_request mem_response
-- himem_response io_responses
-- returns:
-- (io_requests, mem_request, vram_request, himem_request, cpu_mem_resp,
-- ivec, irq)
-- mem_request has 16-bit address; himem has 7-bit address

ioShimmedCPU :: Signal -> Signal -> Signal -> Signal -> Signal ->
                [Signal] -> [Signal] -> IOResps ->
                (IOReqs, MemReq, MemReq, Signal)

ioShimmedCPU sco sci fco fci rs ds hds resps = (reqs, dr, hdr, ins)
  where
    (reqs, dr, hdr, cds, ivec, irq) =
      cpuIOShim sco sci fco fci rs isvc cdr ds hds resps
    co' = sysco reqs
    ci' = sysci reqs
    cdr = (abuf, dbuf, rd, wt)
    (abuf, dbuf, rd, wt, inh, ins, isvc) = shimmedCPU cds irq ivec co' ci' rs

cpuIOShim sco sci fco fci rs isvc inh cdr ds hds (IOResps {..}) =
  (reqs, dr, hdr, cds, ivec, irq) where
  reqs = IOReqs {..}
  gbc = undefined -- we'll figure it out later
  (ca, cd, cr, cw) = cdr
  dr = (ca, cd, isNormal &-& cr, isNormal &-& cw)
  hdr = (drop 9 ca, cd, isHRam &-& cr, isHRam &-& cw)
  isQuiteHigh = ands $ take 7 ca
  isNormal = neg isQuiteHigh
  isVeryHigh = (ca !! 7) &-& isQuiteHigh
  isOAM = isQuiteHigh &&! (ca !! 7)
  isIO = isVeryHigh &&! (ca !! 8)
  isHRam = isVeryHigh &-& (ca !! 8)
  vhds = zipWith (mux2 $ ca !! 8) iods hds
  qhds = zipWith (mux2 $ ca !! 7) oamds vhds
  cds = zipWith (mux2 isNormal) ds qhds
  noise = (ca !! 10) ^-^ (ca !! 11)
  isSoundPort = isIO &&! (ca !! 9)
  isWave = isSoundPort &-& (ca !! 10) &-& (ca !! 11)
  isPortIF = isSoundPort &&! (ca !! 10) &&! (ca !! 11)
  isPort = isPortIF &&! (ca !! 12)
  isNR = isSoundPort &-& noise
  oamReq = (drop 8 ca, cd, isOAM &-& cr, isOAM &-& cw)
  nrReq = (drop 11 ca, cd, isNR &-& cr, isNR &-& cw)
  waveReq = (drop 12 ca, cd, isWave &-& cr, isWave &-& cw)
  portReq = (drop 13 ca, cd, isPort &-& cr, isPort &-& cw)
  isHighIO = isIO &-& (ca !! 9)
  isLCDDMA = isHighIO &&! (ca !! 10)
  isPalMisc = isHighIO &-& (ca !! 10)
  isCPal = isPalMisc &&! (ca !! 11)
  isLCD = isLCDDMA &&! (ca !! 11)
  isDMA = isLCDDMA &-& (ca !! 11)
  lcdReq = (drop 12 ca, cd, isLCD &-& cr, isLCD &-& cw)
  dmaReq = (drop 12 ca, cd, isDMA &-& cr, isDMA &-& cw)
  cpalReq = (drop 12 ca, cd, isCPal &-& cr, isCPal &-& cw)
  sysco = hcpu &-& sco
  sysci = hcpu &-& sci
  portd = zipWith (mux2 $ ca !! 12) portResp $
    (ands (drop 13 ca) &-&) <$> replicate 3 low ++ ifreg
  portwave = zipWith (mux2 $ ca !! 11) portd waveResp
  loiod = zipWith (mux2 noise) portwave nrResp
  lcdmad = zipWith (mux2 $ ca !! 11) lcdResp dmaResp
  palmiscd = zipWith (mux2 $ ca !! 11) cpalResp miscOut
  hiiod = zipWith (mux2 $ ca !! 10) lcdmad palmiscd
  iods = zipWith (mux2 $ ca !! 9) loiod hiiod
  ifreg = undefined -- determine later
  miscOut = undefined -- determine later
  (irq, ivec) = determineInterrupts $ zipWith (&-&) ifreg $ drop 3 iereg
  hcpu = inh &&! irq |-| inDMA

determineInterrupts :: [Signal] -> (Signal, [Signal])
determineInterrupts = undefined -- work out later

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
