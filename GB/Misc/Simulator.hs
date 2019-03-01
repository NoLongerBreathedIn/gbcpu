{-# LANGUAGE OverloadedStrings #-}

module GB.Misc.Simulator (FullGB, GBInputs(..), GBOutputs(..),
                          tickCPU, createGB, addCart, removeCart,
                          resetGB, tickCart) where

import GB.Misc.Cartridge
import GB.Util.SimState
import Data.Word
import Data.Array.ST
import Data.Array.IArray
import Data.Bits
import GB.Util.NetList
import System.Random
import Control.Applicative
import Control.Monad.ST
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Map (Map)
import Data.Text (Text)
import Control.Arrow

data ChanVolts = ChanVolts
                 {-# UNPACK #-} !Double
                 {-# UNPACK #-} !Double
                 {-# UNPACK #-} !Double
                 {-# UNPACK #-} !Double
data ScreenPosn = ScreenPosn
                {-# UNPACK #-} !Word8
                {-# UNPACK #-} !Word8
                {-# UNPACK #-} !Word8

(<$$>) :: (Functor f) => f (a -> b) -> a -> f b
(<$$>) = flip (fmap . flip id)
infixl 4 <$$>

data FullGB s = FullGB {
  cart :: STRef s (Maybe (CartridgeST s)),
  cpuInt :: SNL s,
  channelVoltages :: STRef s ChanVolts,
  screenPosn :: STRef s ScreenPosn,
  vram :: STUArray s Word16 Word8,
  wram :: Array Word8 (STUArray s Word16 Word8),
  }

data GBInputs = GBInputs {
  sckI :: Bool,
  serialI :: Bool,
  voltageIn :: {-# UNPACK #-} !Double,
  buttons :: {-# UNPACK #-} !Word8, -- DULR start select B A
  irI :: Bool
  }

data GBOutputs = GBOutputs {
  sckO :: Bool,
  serialO :: Bool,
  irO :: Bool,
  stopped :: Bool,
  lcdR :: {-# UNPACK #-} !Word8,
  lcdG :: {-# UNPACK #-} !Word8,
  lcdB :: {-# UNPACK #-} !Word8,
  lcdX :: {-# UNPACK #-} !Word8,
  lcdY :: {-# UNPACK #-} !Word8,
  voltageL :: {-# UNPACK #-} !Double,
  voltageR :: {-# UNPACK #-} !Double
  }

gbInternals :: NetList
gbInternals = undefined

createGB :: (RandomGen g) => g -> ST s (FullGB s)
addCart :: FullGB s -> CartridgeST s -> ST s Bool
removeCart :: FullGB s -> ST s (Maybe CartridgeST s)
tickCPU :: FullGB s -> GBInputs -> ST s GBOutputs
resetGB :: FullGB s -> ST s ()
tickCart :: FullGB s -> ST s ()
tickCart gb = do
  mCart <- readSTRef $ cart gb
  maybe (return ()) tickClock

futz :: SNL s -> ST s (SNL s)

resetGB = futz . cpuInt

which :: c -> c -> Bool -> c
which _ c False = c
which c _ True = c

boolsToInt :: (Bits a) => [Bool] -> a

boolsToInt = foldl' (flip (which (`setBit` 0) id) . (`shiftL` 1)) zeroBits

word8ToBools :: Word8 -> [Bool]
word8ToBools a = testBit a <$> [7, 6 .. 0]

resetMap0 :: Map Text [Bool]
resetMap1 :: Map Text [Bool]

clockMapO0 :: Map Text [Bool]
clockMapI0 :: Map Text [Bool]
clockMapO1 :: Map Text [Bool]
clockMapI1 :: Map Text [Bool]

futz n = do
  simulate_ resetMap0 n
  simFull n
  simulate_ resetMap1 n
  return n
  
resetMap0 = M.fromDistinctAscList [("ci", [False]),
                                   ("co", [False]),
                                   ("reset", [False])]
resetMap1 = M.singleton "reset" [True]

clockMapO0 = M.singleton "co" [False]
clockMapO1 = M.singleton "co" [True]
clockMapI0 = M.singleton "ci" [False]
clockMapI1 = M.singleton "ci" [True]

createGB g =
  FullGB <$> newSTRef Nothing <*> (futz =<< simReadyNL g gbInternals) <*>
  newSTRef (ChanVolts 0 0 0 0) <*> newSTRef False <*>
  newSTRef (ScreenPosn 0 0) <*> newArray_ (0, bit 14 - 1) <*>
  sequenceA (listArray (0,7) $ repeat $ newArray_ (0, bit 12 - 1))

addCart (FullGB cr _ _ _ _ _) c = 
  maybe (return True << writeSTRef cr (Just c))
  (const $ return False) =<< readSTRef cr

removeCart (FullGB cr _ _ _ _ _) = do
  c <- readSTRef cr
  writeSTRef cr Nothing
  return c

handleRead :: Map Text [Bool] -> GBInputs -> Maybe (CartridgeST s) ->
              STUArray s Word16 Word8 ->
              Array Word8 (STUArray s Word16 Word8) -> ST s (Map Text [Bool])

handleWrite :: Map Text [Bool] -> Maybe (CartridgeST s) ->
               STUArray s Word16 Word8 ->
               Array Word8 (STUArray s Word16 Word8) -> Double ->
               STRef s ChanVolts -> STRef s ScreenPosn -> ST s GBOutputs

tickCPU gb is = do
  results <- checkOutputs $ cpuInt gb
  mCart <- readSTRef $ cart gb
  iRead <- handleRead results is mCart (vram gb) (wram gb)
  simulate_ iRead $ cpuInt gb
  simulate_ clockMapI1 $ cpuInt gb
  simulate_ clockMapI0 $ cpuInt gb
  simulate_ clockMapO1 $ cpuInt gb
  iWrite <- simulate clockMapO0 $ cpuInt gb
  handleWrite iWrite mCart (vram gb) (wram gb) (voltageIn is)
    (channelVoltages gb) (screenPosn gb)

getVRamAddress :: Map Text [Bool] -> Word16
getVRamData :: Map Text [Bool] -> Word8
getRamAddress :: Map Text [Bool] -> Word16
getRamData :: Map Text [Bool] -> Word8
getWRamBank :: Map Text [Bool] -> Word8
getReadFlags :: Map Text [Bool] -> (Bool, Bool)
getWriteFlags :: Map Text [Bool] -> (Bool, Bool)
getP1Out :: Map Text [Bool] -> Word8
updateSound :: Map Text [Bool] -> Double -> STRef s ChanVolts ->
               ST s (Double, Double)

updateSound = undefined

handleRead gbRes is mCart vRam wRam = do
  let vramAddr = getVRamAddress gbRes
  let ramAddr = getRamAddress gbRes
  let wramBankSel = getWRamBank gbRes
  let (readVRam, readRam) = getReadFlags gbRes
  vl <- if readVRam then readArray vRam vramAddr else return 0
  vh <- if readVRam then readArray vRam (vramAddr .|. bit 13) else return 0
  let wramBank = wRam ! if testBit ramAddr 13 then wramBankSel else 0
  ramRead <- if readRam then
               if ramAddr > 0xBFFF
               then readArray wramBank (ramAddr .&. 0x0FFF)
               else maybe (return 0) (flip readData ramAddr) mCart
             else return 0
  let p1o = getP1Out gbRes
  let p1 = p1o .|. complement (buttons is)
  let p1i = p1 .&. p1 `shiftR` 4
  return $ M.fromDistinctAscList [("d_i", word8ToBools ramRead),
                                  ("iri", irI is && head (gbRes M.! "irc")),
                                  ("md_i", word8ToBools vh ++ word8ToBools vl),
                                  ("p1_i", drop 4 $ word8ToBools p1i),
                                  ("sck_i", sckI is),
                                  ("si", serialI is)]

stBit :: (Bits a) => Bool -> Int -> a -> a
stBit True = flip setBit
stBit False = const id

handleWrite gbRes mCart vRam wRam vin cvs scrp = do
  let (writeVRam, writeRam) = getWriteFlags gbRes
  when writeVRam $ writeArray vRam (getVRamAddress gbRes) (getVRamData gbRes)
  when writeRam $ do
    let ramAddr = getRamAddress gbRes
    let ramData = getRamData gbRes
    let wramBank = if testBit ramAddr 13 then getWRamBank gbRes else 0
    if ramAddr > 0xBFFF
      then writeArray (wRam ! wramBank) (ramAddr .&. 0x0FFF) ramData
      else traverse (\a -> writeData a ramAddr ramData) mCart
  (leftSound, rightSound) <- updateSound gbRes vin cvs
  (ScreenPosn x y s) <- readSTRef scrp
  let horSync = head $ gbRes M.! "hs"
  let verSync = head $ gbRes M.! "vs"
  let pixClock = head $ gbRes M.! "pc"
  let s' = stBit horSync 1 $ stBit verSync 2 $ stBit pixClock 0 0
  let hs = horSync && not (testBit s 1)
  let y' = if verSync && not (testBit s 2) then 0 else if hs then y + 1 else y
  let x' = if hs then 0 else if not pixClock && testBit s 0 then x + 1 else x
  writeSTRef scrp $ ScreenPosn x' y' s'
  return $ GBOutputs {
      sckO = head $ gbRes M.! "sck_o",
      serialO = head $ gbRes M.! "si",
      irO = head $ gbRes M.! "iro",
      stopped = head $ gbRes M.! "stopped",
      lcdR = boolsToInt $ gbRes M.! "ldr",
      lcdG = boolsToInt $ gbRes M.! "ldg",
      lcdB = boolsToInt $ gbRes M.! "ldb",
      lcdX = x,
      lcdY = y,
      voltageL = leftSound,
      voltageR = rightSound
      }

getVRamAddress gbRes = boolsToInt $ (gbRes M.! "ms") ++ (gbRes M.! "ma")
getVRamData = boolsToInt . (M.! "md_o")
getRamAddress = boolsToInt . (M.! "a")
getRamData = boolsToInt . (M.! "d_o")
getWRamBank = boolsToInt . (M.! "s")
getReadFlags = not . head . (M.! "mrd") &&& not . head . (M.! "rd")
getWriteFlags = not . head . (M.! "mwr") &&& not . head . (M.! "wr")
getP1Out = gp1o . (M.! "p1_o") where
  gp1o [True, True] = 0xFF
  gp1o [False, True] = 0xF0
  gp1o [True, False] = 0x0F
  gp1o [False, False] = 0x00
  gp1o _ = error "values of β will give rise to dom!"
