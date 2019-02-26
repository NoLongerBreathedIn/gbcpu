module GB.Misc.Cartridge (Cartridge, CartridgeST, readCart, writeCart,
                          thawCart, freezeCart, readData, writeData,
                          isRumbling, tickClock, advanceClock,
                          isClockRunning) where

import Data.Array.IArray
import Data.Array.Unboxed
import Data.Array.ST
import Data.Array.MArray
import Data.STRef
import Data.Word
import Control.Monad.ST
import Data.ByteString (ByteString)
import Data.Traversable
import Control.Applicative
import Data.Maybe
import Data.Bits

data MBCType = MBC1 | MBC2 | MBC3 | MBC5 | MBCR
  deriving (Eq, Ord, Show)

data ClockState = ClockState {
  seconds :: {-# UNPACK #-} !Word8,
  minutes :: {-# UNPACK #-} !Word8,
  hours   :: {-# UNPACK #-} !Word8,
  daysetc :: {-# UNPACK #-} !Word16
  }

data Cartridge = Cartridge {
  mbcType  :: MBCType,
  hasBattery :: {-# UNPACK #-} !Bool,
  romBanks :: Array Word16 (UArray Word16 Word8),
  ramBanks :: Array Word8 (UArray Word16 Word8),
  clockState :: Maybe ClockState
  }

data BankSelInf = BankSelInf {
  romBank :: {-# UNPACK #-} !Word16,
  ramBank :: {-# UNPACK #-} !Word8,
  ramBankMode :: {-# UNPACK #-} !Bool
  }

data CartridgeST s = CartridgeST {
  mbcType' :: MBCType,
  hasBattery' :: {-# UNPACK #-} !Bool,
  romBanks' :: Array Word16 (UArray Word16 Word8),
  ramBanks' :: Array Word8 (STUArray s Word16 Word8),
  clockState' :: Maybe (STRef s ClockState),
  selectInfo :: STRef s BankSelInf
  }

readCart :: ByteString -> ByteString -> Cartridge
writeCart :: Cartridge -> ByteString -- writes only RAM and possibly clock
thawCart :: Cartridge -> ST s (CartridgeST s)
freezeCart :: CartridgeST s -> ST s Cartridge
readData :: CartridgeST s -> Word16 -> ST s Word8
writeData :: CartridgeST s -> Word16 -> Word8 -> ST s ()
isRumbling :: CartridgeST s -> ST s Bool
tickClock :: CartridgeST s -> ST s ()
advanceClock :: Integer -> CartridgeST s -> ST s ()
isClockRunning :: Cartridge -> Bool

type RMBC s = Array Word16 (UArray Word16 Word8) ->
              Array Word8 (STUArray s Word16 Word8) -> STRef s BankSelInf ->
              Word16 -> ST s Word8
type WMBC s = Array Word8 (STUArray s Word16 Word8) -> STRef s BankSelInf ->
              Word16 -> Word8 -> ST s ()

readMBC1 :: RMBC s
readMBC2 :: RMBC s
readMBC3 :: STRef s ClockState -> RMBC s
readMBC5 :: RMBC s
readMBCR :: RMBC s

writeMBC1 :: WMBC s
writeMBC2 :: WMBC s
writeMBC3 :: STRef s ClockState -> WMBC s
writeMBC5 :: WMBC s
writeMBCR :: WMBC s

tickClock = advanceClock 1

readCart = undefined
writeCart = undefined
thawCart cart =
  CartridgeST (mbcType cart) (hasBattery cart) (romBanks cart) <$>
  traverse thaw (ramBanks cart) <*> traverse newSTRef (clockState cart) <*>
  newSTRef (BankSelInf 0 0 False)
freezeCart cart =
  Cartridge (mbcType' cart) (hasBattery' cart) (romBanks' cart) <$>
  traverse freeze (ramBanks' cart) <*> traverse readSTRef (clockState' cart)
readData cart = f (romBanks' cart) (ramBanks' cart) (selInfo cart) where
  f = case mbcType' cart of
        MBC1 -> readMBC1
        MBC2 -> readMBC2
        MBC3 -> readMBC3 (fromJust $ clockState' cart)
        MBC5 -> readMBC5
        MBCR -> readMBCR
writeData cart = f (ramBanks' cart) (selInfo cart) where
    f = case mbcType' cart of
          MBC1 -> writeMBC1
          MBC2 -> writeMBC2
          MBC3 -> writeMBC3 (fromJust $ clockState' cart)
          MBC5 -> writeMBC5
          MBCR -> writeMBCR
isRumbling cart = if mbctype' cart /= MBCR
                  then return False
                  else flip testBit 3 . ramBank <$> readSTRef $ selectInfo cart

isClockRunning = maybe (return False)
                 (fmap (not . flip testBit 14 . daysetc) . readSTRef) .
                 clockState'

advanceClockInt :: Integer -> ClockState -> ClockState

advanceClock a = maybe (return ()) (modifySTRef' $ advanceClockInt a) .
                 clockState'

advanceClockInt as x@(ClockState s m h d) = if testBit d 14 then x
  else ClockState (fromInteger s') (fromInteger m') (fromInteger h') d' where
  (am, s') = (as + toInteger s) `divMod` 60
  (ah, m') = (am + toInteger m) `divMod` 60
  (ad, h') = (ah + toInteger h) `divMod` 24
  dr = ad + toInteger d
  dm = dr .&. 511
  dc = dr > 511
  d' = (if dc then setBit 15 else id) $ fromInteger dm 

readMBC1 = undefined
readMBC2 = undefined
readMBC3 = undefined
readMBC5 = undefined
readMBCR = undefined

writeMBC1 = undefined
writeMBC2 = undefined
writeMBC3 = undefined
writeMBC5 = undefined
writeMBCR = undefined
