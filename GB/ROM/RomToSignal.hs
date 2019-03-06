module GB.ROM.RomToSignal (signalizeROM) where
import Data.Array.IArray
import GB.Util.Base
import Data.Word
import Data.Bits

signalizeROM :: (Signalish s, IArray a Word8, Ix i) =>
                a i Word8 -> [s] -> [s]

signalizeROM = flip mmux . fmap signalizeByte . elems

signalizeByte :: (Signalish s) => Word8 -> s

signalizeByte = flip fmap [7, 6 .. 0] . (fromBool .) . testBit
