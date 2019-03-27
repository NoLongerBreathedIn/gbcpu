{-# LANGUAGE ExplicitForAll, RankNTypes #-}
module GB.ROM.RomMake (GBasm, makeROM, label, move,
                       GB8(..),
                       GB16(..),
                       GBSpec(..),
                       GBHigh(..),
                       GBCond(..),
                       ld, ldh, lda16, sta16, lda, sta,
                       ld8, ld16, stsp, sphl, hlsp,
                       push, pop,
                       add, adc, sub, sbc,
                       ond, ur, xur, cp,
                       addi, adci, subi, sbci,
                       ondi, uri, xuri, cpi,
                       inc, dec, add16, ajsp, inc16, dec16,
                       daa, cpl, ccf, scf, halt, stop, di, ei,
                       rlca, rla, rrca, rra,
                       jr, jrl, jrc, jrlc, jp, jpc, jphl,
                       call, callc, ret, retc, reti,
                       rst, swop, rlc, rl, rrc, rr,
                       sla, sra, srl, bat, stb, rsb, byte, bytes,
                       regP1, regSB, regSC, regDIV, regTIMA, regTMA, regTAC,
                       regIF, regBIOS,
                       regAVOL, regAOUT, regAENA,
                       regA1SWP, regA1ENV, regA1LEN, regA1LOW, regA1HGH,
                       regA2ENV, regA2LEN, regA2LOW, regA2HGH,
                       regA3ENA, regA3LEN, regA3LEV, regA3LOW, regA3HGH,
                       regA4ENV, regA4POL, regA4BEG, waveRAM,
                       regLCDC, regSTAT, regLY, regLYC, regDMA,
                       regBGP, regOBP0, regOBP1, regWY, regWX,
                       regCSRH, regCSRL, regCDSH, regCDSL, regCAMT,
                       regSVBK, regKEY1, regIRP, regVBK,
                       regBGPI, regBGPD, regOBPI, regOBPD,
                       regDMGEmu, regIODMGEmu, regUNK1,
                       regUNK2, regUNK3, regUNK4, regUNK5,
                       regPCM12, regPCM34, lReg, copyRegion) where

import Data.Word
import Data.Bits
import Data.Array.Unboxed
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans.Reader hiding (ask, local, reader, asks)
import Control.Monad.Reader.Class
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Data.Function
import Data.Traversable

lReg :: Word8 -> Word16
lReg = (.|. 0xFF00) . fromIntegral

pamf :: (Functor f) => f (a -> b) -> a -> f b
pamf = flip $ fmap . flip id

newtype GBasm a = GBasm {
  runGBasm :: forall s.
    StateT Word16
    (ReaderT (STRef s [(Word16, Either Word8 Word16)]) (ST s)) a }

makeROM :: Word16 -> Word16 -> GBasm a -> UArray Word16 Word8

insoit :: (Ix i, MArray a e m, MArray b Bool m) =>
          a i e -> b i Bool -> i -> (Either i e) -> m ()

copyRegion :: Word16 -> Word16 -> GBasm ()

insoit a i = either (readArray a >=> writeArray a i) (writeArray a i) 

makeROM low high asm = runSTUArray $ do
  l <- newSTRef []
  runReaderT (evalStateT (runGBasm asm) low) l
  a <- newArray_ (low, high)
  traverse (uncurry $ insoit a) =<< reverse $ readSTRef l
  return a

instance Functor GBasm where
  fmap f x = GBasm $ fmap f $ runGBasm x
instance Applicative GBasm where
  pure x = GBasm $ pure x
  f <*> x = GBasm $ runGBasm f <*> runGBasm x
instance Monad GBasm where
  x >>= f = GBasm $ runGBasm x >>= \y -> runGBasm $ f y
instance MonadFix GBasm where
  fix f = GBasm $ fix $ \x -> runGBasm $ f x

-- Immediates get h.
-- lda and sta are special load and store from A into
-- (BC)/(DE)/(HL+)/(HL-).
-- ldh is for ld a, (c) and ld a, (a8);
-- sth is similar.
-- lda16 and sta16 are for ld a, (a16) and st a, (a16).
-- sphl is ld sp, hl; hlsp is ld hl, sp+r8; ld16 is for immediate 16,
-- ld8 for immediate 8.
-- stsp is ld (a16), sp.
-- ajsp is add sp, r8.
-- add16 means add hl,-
-- dec16 and inc16 are for 16-bit math
-- for imm math use i suffix
-- RegSF is SP in ld16 and AF in push/pop.
-- jrl and jrlc are jump to nearby label (conditional)
-- SWAP is written as swop, AND as ond, OR as ur, XOR as xur, BIT as bat

data GB8 = RegA | RegB | RegC | RegD | RegE | RegH | RegL | LocHL
  deriving (Eq, Show)
data GB16 = RegSF | RegBC | RegDE | RegHL
  deriving (Eq, Show)
data GBSpec = LocBC | LocDE | LocHLI | LocHLD
  deriving (Eq, Show)
data GBHigh = LocC | Loc8 Word8
  deriving (Eq, Show)
data GBCond = CondNZ | CondZ | CondNC | CondC
  deriving (Eq, Show)

label :: GBasm Word16
move :: Word16 -> GBasm ()
nq :: Word16 -> Word8 -> GBasm ()
nq a d = do
  q <- GBasm ask
  GBasm $ lift $ lift $ modifySTRef q ((a, Right d):)

nqc :: Word16 -> Word16 -> GBasm ()
nqc a d = do
  q <- GBasm ask
  GBasm $ lift $ lift $ modifySTRef q ((a, Left d):)

label = GBasm get
move x = x `seq` GBasm $ put x

byte :: Word8 -> GBasm ()
byte b = do
  l <- label
  nq l b
  move $ l + 1

wyde :: Word16 -> GBasm ()
wyde w = do
  l <- label
  nq l $ fromIntegral w
  nq (l + 1) $ fromIntegral $ w `shiftR` 8
  move $ l + 2

cpra :: Word16 -> Word16 -> Word16 -> GBasm ()
cpra 0 _ t = return t
cpra l f t = f `seq` t `seq` nqc t f >> cpra (l - 1) (f + 1) (t + 1)

copyRegion s l = label >>= cpra l s >>= move

bytes :: [Word8] -> GBasm ()
bytes bs = label >>= flip byaux bs >>= move

byaux :: Word16 -> [Word8] -> GBasm Word16
byaux l [] = return l
byaux l (b:bs) = l `seq` nq l b >> byaux (l + 1) bs

argb :: Word8 -> Word8 -> GBasm ()
argb = (. byte) . (>>) . byte

argw :: Word8 -> Word16 -> GBasm ()
argw = (. wyde) . (>>) . byte

pref :: Word8 -> Word8 -> Word8 -> GBasm ()
pref = (((byte 0xCB >>) .) .) . twoargbyte
opref :: Word8 -> Word8 -> GBasm ()
opref = ((byte 0xCB >>) .) . oneargbyte

convLabel :: Word16 -> GBasm Word8
convLabel = pamf (flip (-) . (+2) . fromIntegral <$> label)

n8 :: GB8 -> Word8
nS :: GBSpec -> Word8
n16 :: GB16 -> Word8
nC :: GBCond -> Word8
hH :: GBH -> Maybe Word8
hH LocC = Nothing
hH (Loc8 a) = Just a

n8 RegB = 0
n8 RegC = 1
n8 RegD = 2
n8 RegE = 3
n8 RegH = 4
n8 RegL = 5
n8 LocHL = 6
n8 RegA = 7

nS LocBC = 0
nS LocDE = 16
nS LocHLI = 32
nS LocHLD = 48

n16 RegBC = 0
n16 RegDE = 16
n16 RegHL = 32
n16 RegSF = 48

nC CondNZ = 0
nC CondZ = 8
nC CondNC = 16
nC CondC = 24

twoargbyte :: Word8 -> Word8 -> Word8 -> GBasm ()
twoargbyte = ((byte .) .) . twoarg
oneargbyte :: Word8 -> Word8 -> GBasm ()
oneargbyte = (byte .) . (.|.)
flipargbyte :: Word8 -> Word8 -> Word8 -> GBasm ()
flipargbyte = flip . twoargbyte

twoarg :: Word8 -> Word8 -> Word8 -> Word8
twoarg a x y = a `shiftL` 6 .|. x `shiftL` 3 .|. y
fliparg :: Word8 -> Word8 -> Word8 -> Word8
fliparg = flip . twoarg

nop :: GBasm ()
ld16 :: GB16 -> Word16 -> GBasm ()
sta :: GBSpec -> GBasm ()
inc16 :: GB16 -> GBasm ()
inc :: GB8 -> GBasm ()
dec :: GB8 -> GBasm ()
ld8 :: GB8 -> GBasm ()
rlca :: GBasm ()
stsp :: Word16 -> GBasm ()
add16 :: GB16 -> GBasm ()
lda :: GBSpec -> GBasm ()
dec16 :: GB16 -> GBasm ()
rrca :: GBasm ()
stop :: GBasm () -- does two bytes
rla :: GBasm ()
jr :: Word8 -> GBasm ()
jrl :: Word16 -> GBasm ()
rra :: GBasm ()
jrc :: GBCond -> Word8 -> GBasm ()
jrlc :: GBCond -> Word16 -> GBasm ()
daa :: GBasm ()
cpl :: GBasm ()
scf :: GBasm ()
ccf :: GBasm ()
ld :: GB8 -> GB8 -> GBasm ()
halt :: GBasm ()
add :: GB8 -> GBasm ()
adc :: GB8 -> GBasm ()
sub :: GB8 -> GBasm ()
sbc :: GB8 -> GBasm ()
ond :: GB8 -> GBasm ()
xur :: GB8 -> GBasm ()
ur :: GB8 -> GBasm ()
cp :: GB8 -> GBasm ()
retc :: GBCond -> GBasm ()
pop :: GB16 -> GBasm ()
jpc :: GBCond -> Word16 -> GBasm ()
jp :: Word16 -> GBasm ()
callc :: GBCond -> Word16 -> GBasm ()
push :: GB16 -> GBasm ()
addi :: Word8 -> GBasm ()
rst :: Word8 -> GBasm ()
ret :: GBasm ()
rlc :: GB8 -> GBasm ()
rrc :: GB8 -> GBasm ()
rl :: GB8 -> GBasm ()
rr :: GB8 -> GBasm ()
sl :: GB8 -> GBasm ()
sra :: GB8 -> GBasm ()
swop :: GB8 -> GBasm ()
srl :: GB8 -> GBasm ()
bat :: Word8 -> GB8 -> GBasm ()
rsb :: Word8 -> GB8 -> GBasm ()
stb :: Word8 -> GB8 -> GBasm ()
call :: Word16 -> GBasm ()
adci :: Word8 -> GBasm ()
subi :: Word8 -> GBasm ()
reti :: GBasm ()
sbci :: Word8 -> GBasm ()
sth :: GBH -> GBasm ()
ondi :: Word8 -> GBasm ()
ajsp :: Word8 -> GBasm ()
jphl :: GBasm ()
sta16 :: Word16 -> GBasm ()
xuri :: Word8 -> GBasm ()
ldh :: GBH -> GBasm ()
di :: GBasm ()
uri :: Word8 -> GBasm ()
hlsp :: Word8 -> GBasm ()
sphl :: GBasm ()
lda16 :: Word16 -> GBasm ()
ei :: GBasm ()
cpi :: Word8 -> GBasm ()

nop = byte 0x00
ld16 = argw . (0x01 .|.) . n16
sta = argw . (0x02 .|.) . nS
inc16 = oneargbyte 0x03 . n16
inc = flipargbyte 0 4 . n8
dec = flipargbyte 0 5 . n8
ld8 = argb . flipargbyte 0 6 . n8
rlca = byte 0x07
stsp = argw 0x08
add16 = argw . (0x09 .|.) . n16
lda = oneargbyte 0x0A . nS
dec16 = argw . (0x0B .|.) . n16
rrca = byte 0x0F
stop = wyde 0x10
rla = byte 0x17
jr = argb 0x18
jrl = jr <=< convLabel
rra = byte 0x1F
jrc = argb . (0x20 .|.) . nC
jrlc = (convLabel >=>) . jrc
daa = byte 0x27
cpl = byte 0x2F
scf = byte 0x37
ccf = byte 0x3F
ld = twoargbyte 1 `on` n8
halt = byte 0x76
add = oneargbyte 0x80 . n8
adc = oneargbyte 0x88 . n8
sub = oneargbyte 0x90 . n8
sbc = oneargbyte 0x98 . n8
ond = oneargbyte 0xA0 . n8
xur = oneargbyte 0xA8 . n8
ur = oneargbyte 0xB0 . n8
cp = oneargbyte 0xB8 . n8
retc = oneargbyte 0xC0 . nC
pop = oneargbyte 0xC1 . n16
jpc = argw . (0xC2 .|.) . nC
jp = argw 0xC3
callc = argw . (0xC4 .|.) . nC
push = oneargbyte 0xC5 . n16
addi = argb 0xC6
rst = flipargbyte 3 7
ret = byte 0xC9
rlc = opref 0x00 . n8
rrc = opref 0x08 . n8
rl = opref 0x10 . n8
rr = opref 0x18 . n8
sl = opref 0x20 . n8
sra = opref 0x28 . n8
swop = opref 0x30 . n8
srl = opref 0x38 . n8
bat = (. n8) . pref 1 . (.&. 7)
rsb = (. n8) . pref 2 . (.&. 7)
stb = (. n8) . pref 3 . (.&. 7)
call = argw 0xCD
adci = argb 0xCE
subi = argb 0xD6
reti = byte 0xD9
sbci = argb 0xDE
sth = maybe (byte 0xE2) (argb 0xE0) . hH
ondi = argb 0xE6
ajsp = argb 0xE8
jphl = byte 0xE9
sta16 = argw 0xEA
xuri = argb 0xEE
ldh = maybe (byte 0xF2) (argb 0xF0) . hH
di = byte 0xF3
uri = argb 0xF6
hlsp = argb 0xF8
sphl = byte 0xF9
lda16 = argw 0xFA
ei = byte 0xFB
cpi = argb 0xFE

regP1   :: Word8 -- D-pad + buttons
regSB   :: Word8 -- Serial transfer data
regSC   :: Word8 -- Serial I/O control
regDIV  :: Word8 -- Divider register, incremented at 2^14 HZ. Writing sets to 0
regTIM  :: Word8 -- Timer counter
regTMA  :: Word8 -- Timer modulus (minimum value)
regTAC  :: Word8 -- Timer control
regIF   :: Word8 -- Interrupt flags
regLCDC :: Word8 -- display control
regSTAT :: Word8 -- display status (see below)
regSCY  :: Word8 -- scroll Y
regSCX  :: Word8 -- scroll X
regLY   :: Word8 -- current line we're on; 0-143 are screen, 144-153 is Vblank
regLYC  :: Word8 -- compares with LY
regDMA  :: Word8 -- starts transfer to OAM from input; use only FF00 up in DMA
regBGP  :: Word8 -- background palette (GB mode)
regOBP0 :: Word8 -- object palette 0 (GB mode)
regOBP1 :: Word8 -- object palette 1 (GB mode)
regWY   :: Word8 -- window Y
regWX   :: Word8 -- window X
regIE   :: Word8 -- Interrupt enable
regBIOS :: Word8
-- display status: bit 6 turns on interrupt when LY=LYC
-- bits 5-3 turn on interrupt when bits 10 become 10/01/00
-- bit 2 is LY=LYC.
-- bits 10 indicate mode:
-- 00: Hblank (all accessible)
-- 01: Vblank (all accessible)
-- 10: OAM usage (OAM inaccessible)
-- 11: VRAM usage (OAM, VRAM inaccessible, don't edit palettes)
regP1   = 0x00
regSB   = 0x01
regSC   = 0x02
regDIV  = 0x03
regTIM  = 0x04
regTMA  = 0x05
regTAC  = 0x06
regIF   = 0x07
regLCDC = 0x40
regSTAT = 0x41
regSCY  = 0x42
regSCX  = 0x43
regLY   = 0x44
regLYC  = 0x45
regDMA  = 0x46
regBGP  = 0x47
regOP0  = 0x48
regOP1  = 0x49
regWY   = 0x4A
regWX   = 0x4B
regIE   = 0xFF
regBIOS = 0x50

-- Sound output
regAVOL  :: Word8 -- Audio volume
regAOUT  :: Word8 -- Audio select
regAENA  :: Word8 -- Sound on/off
regA1SWP :: Word8 -- Device 1 freq sweep
regA1LEN :: Word8 -- Device 1 duty cycle (2 bits), sound length (6 bits)
regA1ENV :: Word8 -- Device 1 envelope
regA1LOW :: Word8 -- Device 1 frequency (low bits)
regA1HGH :: Word8 -- Device 1 frequency (high bits) + start device 1
regA2LEN :: Word8 -- similar to A1LEN, but device 2
regA2ENV :: Word8 -- similar to A1ENV
regA2LOW :: Word8 -- similar to A1LOW
regA2HGH :: Word8 -- similar to A1HGH
regA3ENA :: Word8 -- similar to AENA, but only dev 3
regA3LEN :: Word8 -- similar to A1LEN, but device 3
regA3LEV :: Word8 -- Output level for device 3 (0 is mute, 2 is half, 3 is 1/4)
regA3LOW :: Word8 -- similar to A1LOW
regA3HGH :: Word8 -- similar to A1HGH
regA4LEN :: Word8 -- similar to A1LEN, but device 4
regA4ENV :: Word8 -- similar to A1ENV
regA4POL :: Word8 -- Select driving frequency for white noise?
regA4BEG :: Word8 -- Start playing device 4
waveRAM :: Word8 -> Word8 -- wave pattern for device 3
regAVOL  = 0x24
regAOUT  = 0x25
regAENA  = 0x26
regA1SWP = 0x10
regA1LEN = 0x11
regA1ENV = 0x12
regA1LOW = 0x13
regA1HGH = 0x14
regA2LEN = 0x16
regA2ENV = 0x17
regA2LOW = 0x18
regA2HGH = 0x19
regA3ENA = 0x1A
regA3LEN = 0x1B
regA3LEV = 0x1C
regA3LOW = 0x1D
regA3HGH = 0x1E
regA4LEN = 0x20
regA4ENV = 0x21
regA4POL = 0x22
regA4BEG = 0x23
waveRAM = (.|.0x30)

-- GBC enhancements
regCSRH :: Word8 -- GP DMA src, high
regCSRL :: Word8 -- GP DMA src, low
regCDSH :: Word8 -- GP DMA dest, high
regCDSL :: Word8 -- GP DMA dest, low
regCAMT :: Word8 -- GP DMA amt
regSVBK :: Word8 -- select bank for 0xD*** memory (1-7)
regKEY1 :: Word8 -- speed switching
regIRP  :: Word8 -- infrared port
regVBK  :: Word8 -- display RAM bank select (0x8000-0x9FFF)
regBGPI :: Word8 -- background color palette spec
regBGPD :: Word8 -- ditto data
regOBPI :: Word8 -- object ditto
regOBPD :: Word8 -- ditto
regCSRH = 0x51
regCSRL = 0x52
regCDSH = 0x53
regCDSL = 0x54
regCAMT = 0x55
regSVBK = 0x70
regKEY1 = 0x4D
regIRP  = 0x56
regVBK  = 0x4F
regBGPI = 0x68
regBGPD = 0x69
regOBPI = 0x6A
regOBPD = 0x6B

regIODMGEmu :: Word8
regDMGEmu   :: Word8
regUNK2     :: Word8
regUNK3     :: Word8
regUNK4     :: Word8
regUNK5     :: Word8
regPCM12    :: Word8
regPCM34    :: Word8
regIODMGEmu = 0x4C
regDMGEmu   = 0x6C
regUNK2     = 0x72
regUNK3     = 0x73
regUNK4     = 0x74
regUNK5     = 0x75
regPCM12    = 0x76
regPCM34    = 0x77
