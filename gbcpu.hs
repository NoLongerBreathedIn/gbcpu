module mmix where
import Data.Word
import Data.List (foldl')
import Data.Function (on)
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer.Strict
import Data.Bits hiding (xor)

type GBasm a = StateT Word16 (Writer [Word8] a)

asm :: Word16 -> GBasm a -> [Word8]
asm = (execWriter .) . flip evalStateT

byte :: Word8 -> GBasm ()
byte = tell . (:[]) >> modify' (+1)

bytes :: [Word8] -> GBasm ()
bytes a = tell a >> modify' (+ (length a))

wyde :: Word16 -> GBasm ()
wyde a = modify' (+2) >> tell [fromIntegral a, fromIntegral $ shiftR a 8]

label :: GBasm Word16
label = get

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

data GBEight = RegA | RegB | RegC | RegD | RegE | RegH | RegL | LocHL
  deriving (Eq, Show)
data GBSixteen = RegSF | RegBC | RegDE | RegHL
  deriving (Eq, Show)
data GBSpecial = LocBC | LocDE | LocHLI | LocHLD
  deriving (Eq, Show)
data GBHigh = LocC | Loc8 Word8
  deriving (Eq, Show)
data GBCond = CondNZ | CondZ | CondNC | CondC
  deriving (Eq, Show)

eightNum :: GBEight -> Word8
specNum :: GBSpecial -> Word8
sixNum :: GBSixteen -> Word8
condNum :: GBCond -> Word8

ld :: GBEight -> GBEight -> GBasm ()
twoargbyte :: Word8 -> Word8 -> Word8 -> GBasm ()
twoargbyte a x y = byte $ a `shiftL` 6 .|. x `shiftL` 3 .|. y
flipargbyte :: Word8 -> Word8 -> Word8 -> GBasm ()
flipargbyte = flip . twoargbyte
oneargbyte :: Word8 -> Word8 -> GBasm ()
oneargbyte = (byte .) . (.|.)

ld = twoargbyte 1 `on` eightNum

ldh :: GBHigh -> GBasm ()
ldh x = case x of
          LocC -> byte 0xF2
          Loc8 a -> byte 0xF0 >> byte a
sth :: GBHigh -> GBasm ()
sth x = case x of
          LocC -> byte 0xE2
          Loc8 a -> byte 0xE0 >> byte a

lda16 :: Word16 -> GBasm ()
sta16 :: Word16 -> GBasm ()
lda16 = (byte 0xFA >>) . wyde
sta16 = (byte 0xEA >>) . wyde

lda :: GBSpecial -> GBasm ()
sta :: GBSpecial -> GBasm ()
lda = flipargbyte 0 0xA . specNum
sta = flipargbyte 0 0x2 . specNum

ld8 :: GBEight -> Word8 -> GBasm ()
ld8 = (. byte) . (>>) . flipargbyte 0 0x6 . eightNum
ld16 :: GBSixteen -> Word16 -> GBasm ()
ld16 = (. wyde) . (>>) . flipargbyte 0 0x1 . sixNum

stsp :: Word16 -> GBasm ()
sphl :: GBasm ()
hlsp :: Int8 -> GBasm ()
stsp = (byte 0x08 >>) . wyde
sphl = byte 0xF9
hlsp = (byte 0xF8 >>) . byte . fromIntegral

push :: GBSixteen -> GBasm ()
push = flipargbyte 3 0x5
pop :: GBSixteen -> GBasm ()
pop = flipargbyte 3 0x1

add :: GBEight -> GBasm ()
adc :: GBEight -> GBasm ()
sub :: GBEight -> GBasm ()
sbc :: GBEight -> GBasm ()
and :: GBEight -> GBasm ()
xor :: GBEight -> GBasm ()
or  :: GBEight -> GBasm ()
cp  :: GBEight -> GBasm ()
addi :: Word8 -> GBasm ()
adci :: Word8 -> GBasm ()
subi :: Word8 -> GBasm ()
sbci :: Word8 -> GBasm ()
andi :: Word8 -> GBasm ()
xori :: Word8 -> GBasm ()
ori  :: Word8 -> GBasm ()
cpi  :: Word8 -> GBasm ()
inc  :: GBEight -> GBasm ()
dec  :: GBEight -> GBasm ()

add = oneargbyte 0x80 . eightNum
adc = oneargbyte 0x88 . eightNum
sub = oneargbyte 0x90 . eightNum
sbc = oneargbyte 0x98 . eightNum
and = oneargbyte 0xA0 . eightNum
xor = oneargbyte 0xA8 . eightNum
or  = oneargbyte 0xB0 . eightNum
cp  = oneargbyte 0xB8 . eightNum

addi = (byte 0xC6 >>) . byte
adci = (byte 0xCE >>) . byte
subi = (byte 0xD6 >>) . byte
sbci = (byte 0xDE >>) . byte
andi = (byte 0xE6 >>) . byte
xori = (byte 0xEE >>) . byte
ori  = (byte 0xF6 >>) . byte
cpi  = (byte 0xFE >>) . byte

inc = flipargbyte 0 0x4 . eightNum
dec = flipargbyte 0 0x5 . eightNum

add16 :: GBSixteen -> GBasm ()
ajsp :: Int8 -> GBasm ()
inc16 :: GBSixteen -> GBasm ()
dec16 :: GBSixteen -> GBasm ()
add16 = flipargbyte 0 0x9 . sixNum
ajsp = (byte 0xE8 >>) . byte . fromIntegral
inc16 = flipargbyte 0 0x3 . sixNum
dec16 = flipargbyte 0 0xB . sixNum
-- misc
daa = byte 0x27
cpl = byte 0x2F
ccf = byte 0x3F
scf = byte 0x37
nop = byte 0x00
halt = wyde 0x76 -- actually this is halt nop.
stop = wyde 0x10 -- maybe stop nop?
di = byte 0xF3
ei = byte 0xFB
rlca = byte 0x07
rla = byte 0x17
rra = byte 0x0F
rrca = byte 0x1F

-- jumps
jr :: Int8 -> GBasm ()
jrc :: GBCond -> Int8 -> GBasm ()
jp :: Word16 -> GBasm ()
jpc :: GBCond -> Word16 -> GBasm ()
jphl :: GBasm ()
call :: Word16 -> GBasm ()
callc :: GBCond -> Word16 -> GBasm ()
ret :: GBasm ()
retc :: GBCond -> GBasm ()
reti :: GBasm ()
rst :: Word8 -> GBasm ()

jr = (byte 0x18 >>) . byte . fromIntegral
jrc = (. (byte . fromIntegral)) . (>>) . (flipargbyte 0 0x20) . condNum
jp = (byte 0xC3 >>) . wyde
jpc = (. wyde) . (>>) . (flipargbyte 3 2) . condNum
jphl = byte 0xE9
call = (byte 0xCD >>) . wyde
callc = (. wyde) . (>>) . (flipargbyte 3 4) . condNum
ret = byte 0xC9
retc = flipargbyte 3 0 . condNum
reti = byte 0xD9
rst = flipargbyte 3 7 . (`shiftR` 3)

-- CB pref
pref :: Word8 -> Word8 -> Word8 -> GBasm ()
pref a b c = byte 0xCB >> twoargbyte a b c
opref :: Word8 -> Word8 -> GBasm ()
opref a b = byte 0xCB >> oneargbyte a b

swap :: GBEight -> GBasm ()
rlc :: GBEight -> GBasm ()
rl :: GBEight -> GBasm ()
rrc :: GBEight -> GBasm ()
rr :: GBEight -> GBasm ()
sla :: GBEight -> GBasm ()
sra :: GBEight -> GBasm ()
srl :: GBEight -> GBasm ()
swap = opref 0x30 . eightNum
rlc = opref 0x00 . eightNum
rl = opref 0x10 . eightNum
rrc = opref 0x08 . eightNum
rr = opref 0x18 . eightNum
sla = opref 0x20 . eightNum
sra = opref 0x28 . eightNum
srl = opref 0x38 . eightNum

bit :: Word8 -> GBEight -> GBasm ()
set :: Word8 -> GBEight -> GBasm ()
res :: Word8 -> GBEight -> GBasm ()
bit = (. eightNum) . pref 1 . (.&. 7)
set = (. eightNum) . pref 3 . (.&. 7)
res = (. eightNum) . pref 2 . (.&. 7)

eightNum x = case x of
               RegB -> 0
               RegC -> 1
               RegD -> 2
               RegE -> 3
               RegH -> 4
               RegL -> 5
               LocHL -> 6
               RegA -> 7
specNum x = case x of
              LocBC -> 0
              LocDE -> 2
              LocHLI -> 4
              LocHLD -> 6
sixNum x = case x of
             RegBC -> 0
             RegDE -> 2
             RegHL -> 4
             RegSF -> 6

color :: Word8 -> Word8 -> Word8 -> Word16
compress :: Word8 -> Word16
color r g b = let rc = compress r
                  gc = compress g
                  bc = compress b in
                bc `shiftL` 10 .|. gc `shiftL` 5 .|. rc
compress = fromIntegral . (0x1F .&.)

pixdata :: Word16 -> Word16
squeeze :: (Bits a) => a -> a
pixdata x = squeeze (x `shiftR` 1) `shiftL` 8 .|. squeeze x
squeeze = foldl' (.|.) 0 . flip (map . liftM2 (.&.) bit . shiftR) [0..7]

regP1  :: Word8 -- D-pad + buttons
regSB  :: Word8 -- Serial transfer data
regSC  :: Word8 -- Serial I/O control
regDIV :: Word8 -- Divider register, incremented at 2^14 HZ. Writing sets to 0
regTIM :: Word8 -- Timer counter
regTMA :: Word8 -- Timer modulus (minimum value)
regTAC :: Word8 -- Timer control
regIF  :: Word8 -- Interrupt flags
regDSC :: Word8 -- display control
regDST :: Word8 -- display status (see below)
regSCY :: Word8 -- scroll Y
regSCX :: Word8 -- scroll X
regLY  :: Word8 -- current line we're on; 0-143 are screen, 144-153 is Vblank
regLYC :: Word8 -- compares with LY
regDMA :: Word8 -- starts transfer to OAM from input; use only FF00 up in DMA
regBGP :: Word8 -- background palette (GB mode)
regOP0 :: Word8 -- object palette 0 (GB mode)
regOP1 :: Word8 -- object palette 1 (GB mode)
regWY  :: Word8 -- window Y
regWX  :: Word8 -- window X
regIE  :: Word8 -- Interrupt enable
-- display status: bit 6 turns on interrupt when LY=LYC
-- bits 5-3 turn on interrupt when bits 10 become 10/01/00
-- bit 2 is LY=LYC.
-- bits 10 indicate mode:
-- 00: Hblank (all accessible)
-- 01: Vblank (all accessible)
-- 10: OAM usage (OAM inaccessible)
-- 11: VRAM usage (OAM, VRAM inaccessible, don't edit palettes)
regP1  = 0x00
regSB  = 0x01
regSC  = 0x02
regDIV = 0x03
regTIM = 0x04
regTMA = 0x05
regTAC = 0x06
regIF  = 0x07
regDSC = 0x40
regDST = 0x41
regSCY = 0x42
regSCX = 0x43
regLY  = 0x44
regLYC = 0x45
regDMA = 0x46
regBGP = 0x47
regOP0 = 0x48
regOP1 = 0x49
regWY  = 0x4A
regWX  = 0x4B
regIE  = 0xFF

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
regBCPS :: Word8 -- background color palette spec
regBCPD :: Word8 -- ditto data
regOCPS :: Word8 -- object ditto
regOCPD :: Word8 -- ditto
regCSRH = 0x51
regCSRL = 0x52
regCDSH = 0x53
regCDSL = 0x54
regCAMT = 0x55
