{-# LANGUAGE NamedFieldPuns, RecordWildCards, ImplicitParams #-}
module GB.CPU.Memory.IOShim (cpuIOShim, MemReq, memMultiplexer,
                             ioShimmedCPU,
                             IOReqs(..), IOResps(..)) where

import GB.Lava.Signal
import GB.Util.Base
import GB.CPU.CoreShim
import Data.Foldable
import GB.GameBoy

type MemReq = ([Signal], [Signal], Signal, Signal)

data IOReqs = IOReqs {
  sysco :: Signal,
  sysci :: Signal,
  dmg :: Signal,
  ninboot :: Signal,
  oamReq :: MemReq, -- 8bit address
  portReq :: MemReq, -- 3bit address
  nrReq :: MemReq, -- 5bit address
  waveReq :: MemReq, -- 4bit address
  lcdReq :: MemReq, -- 4bit address
  dmaReq :: MemReq, -- 3bit address
  cpalReq :: MemReq
  }
data IOResps = IOResps {
  oamds :: [Signal],
  portResp :: [Signal],
  timerInt :: Signal,
  serialInt :: Signal,
  buttonInt :: Signal,
  nrResp :: [Signal],
  pcm12 :: [Signal],
  pcm34 :: [Signal],
  waveResp :: [Signal],
  lcdResp :: [Signal],
  lcdcInt :: Signal,
  vblankInt :: Signal,
  dmaResp :: [Signal],
  inDMA :: Signal,
  cpalResp :: [Signal]
  }

memMultiplexer :: MemReq -> MemReq -> MemReq
-- priority comes second

memMultiplexer (a0, d0, r0, w0) (a1, d1, r1, w1) = (a, d, r, w) where
  a = zipWith (mux2 s) a0 a1
  d = zipWith (mux2 s) d0 d1
  r = r0 &-& neg w1 |-| r1
  w = w0 &-& neg r1 |-| w1
  s = w1 |-| r1

cpuIOShim :: (?gbtype :: GBType) => Signal -> Signal -> Signal -> Signal ->
             Signal -> Signal -> Signal -> MemReq -> [Signal] -> [Signal] ->
             [Signal] -> IOResps ->
             (IOReqs, MemReq, MemReq, [Signal], [Signal], Signal, Signal)
-- co ci fco fci rs isvc inhalt cpu_mem_request mem_response
-- himem_response io_responses
-- returns:
-- (io_requests, mem_request, vram_request, himem_request, cpu_mem_resp,
-- ivec, irq, inboot)
-- mem_request has 16-bit address; himem has 7-bit address

ioShimmedCPU :: (?gbtype :: GBType) => Signal -> Signal -> Signal -> Signal ->
                Signal -> [Signal] -> [Signal] -> [Signal] -> IOResps ->
                (IOReqs, MemReq, MemReq, Signal, Signal)

ioShimmedCPU sco sci fco fci rs ds hds ier resps = (reqs, dr, hdr, ins, inboot)
  where
    (reqs, dr, hdr, cds, ivec, irq, inboot) =
      cpuIOShim sco sci fco fci rs isvc cdr ds hds ier resps
    co' = sysco reqs
    ci' = sysci reqs
    cdr = (abuf, dbuf, rd, wt)
    (abuf, dbuf, rd, wt, inh, ins, isvc) = shimmedCPU cds irq ivec co' ci' rs

cpuIOShim sco sci fco fci rs isvc inh cdr ds hds iereg (IOResps {..}) =
  (reqs, dr, hdr, cds, ivec, irq, inboot) where
  reqs = IOReqs {..}
  writeDMG = fci &-& isDMGEmulator &-& cw
  dmg = if ?gbtype == DMG then high
        else dffO fco rs $ dffO (writeDMG &&! ninboot) rs $ cd !! 7
  fakeDMG = dmg |-| dffO fco rs (dffO writeDMG rs $ cd !! 7)
  ninboot = dffZ fco $ dffZ (isBootM &-& fci) rs $ cd !! 7
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
  isRealCPal = isCPal &-& (ca !! 12)
  isDMGEmulator = isRealCPal &-& dmgEm
  dmgEm = (ca !! 13) &&! (ca !! 14) &&! (ca !! 15)
  isLCD = isLCDDMA &&! (ca !! 11)
  isDMA = isLCDDMA &-& (ca !! 11)
  isRealDMA = isDMA &&! (ca !! 12)
  zEight = ors (drop 13 ca)
  isBootM = cw &-& isRealDMA &&! zEight
  isReallyDMA = isRealDMA &-& zEight
  lcdReq = (drop 12 ca, cd, isLCD &-& cr, isLCD &-& cw)
  dmaReq = (drop 13 ca, cd, isReallyDMA &-& cr, isReallyDMA &-& cw)
  cpalReq = (drop 14 ca, cd, isReallyCPal &-& cr, isReallyCPal &-& cw)
  sysco = hcpu &-& sco
  sysci = hcpu &-& sci
  portd = zipWith (mux2 $ ca !! 12) portResp $
    (zEight !||) <$> replicate 3 high ++ ifreg
  portwave = zipWith (mux2 $ ca !! 11) portd waveResp
  loiod = zipWith (mux2 noise) portwave nrResp
  dmad = (zEight &&! (ca !! 12) |-|) <$> dmaResp
  lcdmad = zipWith (mux2 $ ca !! 11) lcdResp dmad
  dmgResp = replicate 7 high ++ [(ca !! 14) |-| (ca !! 15) |-| fakeDMG]
  cpald = ((ca !! 12) !||) <$> zipWith (mux2 $ ca !! 13) cpalResp dmgResp
  palmiscd = zipWith (mux2 $ ca !! 11) cpald miscOut
  hiiod = zipWith (mux2 $ ca !! 10) lcdmad palmiscd
  iods = zipWith (mux2 $ ca !! 9) loiod hiiod
  isIF = cw &-& isPortIF &-& ands $ drop 12 ca
  ifreg = interruptReg fco fci isIF isvc
    [buttonInt, serialInt, timerInt, lcdcInt, vblankInt] imi $ drop 3 cd
  (miscOut, svbk) =
    handleMiscOut fco (fci &-& cw &-& isPalMisc &-& (ca !! 11) &&! (ca !! 12))
    (drop 13 ca) cd dmg pcm01 pcm23
  (irq, ivec, imi) = determineInterrupts $ ensureLength 5 $
    zipWith (&-&) ifreg $ drop 3 iereg
  hcpu = inh &&! irq |-| inDMA

ensureLength :: Int -> [a] -> [a]
ensureLength 0 = []
ensureLength i = \x -> head x : ensureLength (i - 1) (tail x)

determineInterrupts :: [Signal] -> (Signal, [Signal], [Signal])
-- returns irq, ivec, location of last interrupt

handleMiscOut :: (?gbtype :: GBType) => Signal -> Signal -> [Signal] ->
                 [Signal] -> Signal -> [Signal] -> [Signal] ->
                 ([Signal], [Signal])
-- co ci/w a[3] d[8] dmg pcm12[8] pcm34[8]
-- returns (data[8], svbk[3])

interruptReg :: Signal -> Signal -> Signal -> Signal ->
                [Signal] -> [Signal] -> [Signal] -> [Signal]
-- clock out, clock in, write, activate reset, async set, reset, data
-- reset happens only if activate was off last clock out and is now on.

handleMiscOut co ci a d dmg pcm12 pcm34 = (dat, svbk) where
  dat = mmux a [replicate 5 high ++ svbkf,
                replicate 8 high,
                unk2,
                unk3,
                unk4,
                unk5,
                foo pcm12,
                foo pcm34]
  wts = demux ci a
  regs = (\w -> registerAW co w d) <$> demux ci a
  svbk = (&&! dmg) <$> drop 5 (regs !! 0)
  svbkf = (|-| dmg) <$> drop 5 (regs !! 0)
  foo = if ?gbtype == DMG then const $ replicate 8 high else id
  unk2 = foo $ regs !! 2
  unk3 = foo $ regs !! 3
  unk4 = (|-| dmg) <$> (regs !! 4)
  unk5 = foo $ high : take 3 (tail $ regs !! 5) ++ replicate 5 high

interruptReg co ci w ars = zipWith3 iReg where
  iReg s r d = let r' = r &-& rse in
                 dffO co (neg s) $ dffO (ci &-& (w |-| r')) (neg s) d
  ars' = dff co $ dff ci ars
  rse = ars &&! ars'

determineInterrupts = detI . foldr wrok (low, []) where
  wrok i (pi, is) = (pi |-| i, i &&! pi : is)
  detI (irq, imi) =
    (irq,
     [head imi, (imi !! 1) |-| (imi !! 2), (imi !! 1) |-| (imi !! 3)],
     imi)


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
