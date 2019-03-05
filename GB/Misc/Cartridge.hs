module GB.Misc.Cartridge (Cartridge, CartridgeST, readCart, writeCart,
                          thawCart, freezeCart, readData, writeData,
                          isRumbling, tickClock, advanceClock,
                          isClockRunning) where

-- There should be some way to savestate here.

import Data.Array.IArray
import Data.Array.Unboxed
import Data.Array.ST
import Data.Array.MArray
import Data.STRef
import Data.Word
import Control.Monad.ST
import Control.Monad
import Data.ByteString (ByteString)
import Data.Traversable
import Control.Applicative
import Data.Maybe
import Data.Bits

data MBCType = MBC0 | MBC1 | MBC2 | MBC3 | MBC5 | MBCR
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
  romBank :: {-# UNPACK #-} !Word8,
  ramBank :: {-# UNPACK #-} !Word8,
  ramBankMode :: {-# UNPACK #-} !Bool,
  ramEnabled :: {-# UNPACK #-} !Bool
  }

data CartridgeST s = CartridgeST {
  mbcType' :: MBCType,
  hasBattery' :: {-# UNPACK #-} !Bool,
  romBanks' :: Array Word16 (UArray Word16 Word8),
  ramBanks' :: Array Word8 (STUArray s Word16 Word8),
  clockState' :: Maybe (STRef s ClockState),
  clockStateL :: Maybe (STRef s ClockState),
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

readMBC0 :: RMBC s
readMBC1 :: RMBC s
readMBC2 :: RMBC s
readMBC3 :: STRef s ClockState -> STRef s ClockState -> RMBC s
readMBC5 :: RMBC s
readMBCR :: RMBC s

writeMBC0 :: WMBC s
writeMBC1 :: WMBC s
writeMBC2 :: WMBC s
writeMBC3 :: STRef s ClockState -> STRef s ClockState -> WMBC s
writeMBC5 :: WMBC s
writeMBCR :: WMBC s

tickClock = advanceClock 1

readCart = undefined
writeCart = undefined
thawCart cart =
  CartridgeST (mbcType cart) (hasBattery cart) (romBanks cart) <$>
  traverse thaw (ramBanks cart) <*> traverse newSTRef (clockState cart) <*>
  traverse newSTRef (clockState cart) <*>
  newSTRef (BankSelInf 0 0 False False)
freezeCart cart =
  Cartridge (mbcType' cart) (hasBattery' cart) (romBanks' cart) <$>
  traverse freeze (ramBanks' cart) <*> traverse readSTRef (clockState' cart)
readData cart = f (romBanks' cart) (ramBanks' cart) (selInfo cart) where
  f = case mbcType' cart of
        MBC0 -> readMBC0
        MBC1 -> readMBC1
        MBC2 -> readMBC2
        MBC3 -> readMBC3 (fromJust $ clockState' cart)
                (fromJust $ clockStateL cart)
        MBC5 -> readMBC5
        MBCR -> readMBCR
writeData cart = f (ramBanks' cart) (selInfo cart) where
    f = case mbcType' cart of
          MBC0 -> writeMBC0
          MBC1 -> writeMBC1
          MBC2 -> writeMBC2
          MBC3 -> writeMBC3 (fromJust $ clockState' cart)
                  (fromJust $ clockStateL cart)
          MBC5 -> writeMBC5
          MBCR -> writeMBCR
isRumbling cart = if mbcType' cart /= MBCR
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

readMBC0 rom ram _ x =
    if x < 0x8000
    then return $ (rom ! 0) ! x
    else readArray (ram ! 0) $ x .&. 0x1FFF
writeMBC0 _ ram _ x = when (x .&. 0xE000 == 0xA000) .
                      writeArray (ram ! 0) (x .&. 0x1FFF)

selRam :: BankSelInf -> Word8
selRam bsi = if ramBankMode bsi then ramBank bsi else 0

readMBC1 rom ram sel x = do
  bsi <- readSTRef sel
  if testBit x 15
    then if ramEnabled bsi
         then readArray (ram ! selRam bsi) $ x .&. 0x1FFF
         else return 0xFF
    else return $
         let romB = if testBit x 14
                    then (if ramBankMode bsi
                          then 0
                          else ramBank bsi) `shiftL` 5 .|. romBank bsi
                    else 0 in
           (rom ! fromIntegral romB) ! (x .&. 0x3FFF)
                           
writeMBC1 rom ram sel x y =
  if testBit x 15
  then when (x .&. 0xE000 == 0xA000) $ do
    bsi <- readSTRef sel
    when (ramEnabled bsi) $ writeArray (ram ! selRam bsi) (x .&. 0x1FFF) y
  else modifySTRef' sel $ case x `shiftR` 13 of
         0 -> \s -> s {ramEnabled = y .&. 0x0F == 0x0A}
         1 -> \s -> s {romBank = case y .&. 0x1F of
                          0 -> 1
                          x -> x}
         2 -> \s -> s {ramBank = y .&. 0x02}
         3 -> \s -> s {ramBankMode = testBit y 0}
         
readMBC2 rom ram sel x = do
  bsi <- readSTRef sel
  if testBit x 15
    then if ramEnabled bsi
         then readArray (ram ! 0) $ x .&. 0x01FF
         else return 0xFF
    else return $
         let romB = if testBit x 14 then romBank bsi else 0 in
           (rom ! fromIntegral romB) ! (x .&. 0x3FFF)
writeMBC2 rom ram sel x y =
  let y' = y .&. 0x0F in
    if testBit x 15
    then when (x .&. 0xFE00 == 0xA000) $ do
      bsi <- readSTRef sel
      when (ramEnabled bsi) $ writeArray (ram ! 0) (x .&. 0x01FF) y'
    else unless (testBit x 14) $
         if testBit x 13
         then when (testBit x 8) $ modifySTRef' sel $
              \s -> s {ramEnabled = not $ ramEnabled s}
         else unless (testBit x 8) $ modifySTRef' sel $
              \s -> s {romBank = if y' == 0 then 1 else y'}

writeSTRef' :: STRef s a -> a -> ST s ()
writeSTRef' r v = v `seq` writeSTRef r

readMBC3 ck ckR rom ram sel x = do
  bsi <- readSTRef sel
  if testBit x 15
    then if ramEnabled bsi
         then if testBit (ramBank bsi) 3
              then (case clearBit (ramBank bsi) 3 of
                      0 -> seconds
                      1 -> minutes
                      2 -> hours
                      3 -> fromIntegral . daysetc
                      4 -> fromIntegral . (`shiftR` 8) . daysetc) <$>
                   readSTRef (if ramBankMode bsi then ckR else ck)
              else readArray (ram ! ramBank bsi) (x .&. 0x1FFF)
         else return 0xFF
    else return $
    (rom ! if testBit x 14 then fromIntegral $ romBank bsi else 0) !
    (x .&. 0x3FFF)
    
writeMBC3 ck ckR rom ram sel x y = do
  bsi <- readSTRef sel
  if testBit x 15
    then when (x .&. 0xE000 == 0xA000 && ramEnabled bsi) $
         if testBit (ramBank bsi) 3
         then do
           let f = case clearBit (ramBank bsi) 3 of
                 0 -> \s -> s {seconds = y}
                 1 -> \s -> s {minutes = y}
                 2 -> \s -> s {hours = y}
                 3 -> \s -> s {daysetc = daysetc x .&. 0xFF .|. y `shiftL` 8}
                 4 -> \s -> s {daysetc = daysetc x .&. 0xFF00 .|. y}
           modifySTRef' f ckR
           modifySTRef' f ck
         else writeArray (ram ! ramBank bsi) (x .&. 0x1FFF) y
    else do
    writeSTRef' sel $ case x `shiftR` 13 of
                         0 -> bsi {ramEnabled = y .&. 0x0F == 0x0A}
                         1 -> bsi {romBank = case clearBit y 7 of
                                          0 -> 1
                                          z -> z}
                         2 -> bsi {ramBank = if y .&. 0x0F == 0x0C
                                                 then 0x0C
                                                 else y .&. 0x0B}
                         3 -> bsi {ramBankMode = testBit y 0}
    when (x `shiftR` 13 == 3 && testBit y 0 && not (ramBankMode bsi)) $
      writeSTRef ckR =<< readSTRef ck

rMBC5 :: (Word8 -> Word8) -> RMBC s
wMBC5 :: (Word8 -> Word8) -> WMBC s

readMBC5 = rMBC5 id
writeMBC5 = wMBC5 id

readMBCR = rMBC5 (.&. 7)
writeMBCR = wMBC5 (.&. 7)

rMBC5 f = \rom ram sel x -> do
  bsi <- readSTRef sel
  if testBit x 15
    then if ramEnabled bsi
         then readArray (ram ! f (ramBank bsi)) $ x .&. 0x1FFF
         else return 0xFF
    else return $ let romB =
                        if testBit x 14
                        then (if (ramBankMode bsi) then (`setBit` 8) else id) $
                             fromIntegral $ romBank bsi
                        else 0 in
                    (rom ! romB) ! (x .&. 0x3FFF)
wMBC5 f = \rom ram sel x ->
            if testBit x 15
            then when (x .&. 0xE000 == 0xA000) . \y -> do
              when (ramEnabled bsi) $
                writeArray (ram ! f (ramBank bsi)) (x .&. 0x1FFF) y
            else modifySTRef' sel .
                 case x `shiftR` 13 of
                   0 -> \y s -> s {ramEnabled = y .&. 0x0F == 0x0A}
                   1 -> if testBit x 12
                        then \y s -> {ramBankMode = testBit y 0}
                        else \y s -> s {romBank = y}
                   2 -> \y s -> s {ramBank = y .&. 0x0F}
                   3 -> const id
