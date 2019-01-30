{-# LANGUAGE DeriveFunctor #-}

module GB.CPU.Decoder (decode,
                       registers,
                       regOutput,
                       flags,
                       carry,
                       MicroInstruction(..),
                       CPUInputs(..),
                       RegisterOutputs(..),
                       RegisterSet,
                       CPURegisters(fIE, fpIE)) where

import GB.Lava.Signal
import GB.CPU.Alu
import GB.Util.Base
import Data.Array
import Control.Arrow (second)
import Data.Maybe
import Control.Applicative

data MicroInstruction a = MicroInstruction {setEights :: [a], -- 4
                                            aluR :: [a], -- 4
                                            aluL :: [a], -- 2
                                            aluC :: [a], -- 8
                                            fS :: [a], -- 3
                                            fR :: [a], -- 4
                                            fM :: [a], -- 4
                                            setSixteens :: [a], -- 8
                                            bus16 :: [a], -- 2
                                            iDec :: [a], -- 3
                                            miscFlags :: [a]} -- j, pei, di,
                          -- HALT, write
                          deriving (Functor, Show)

instance Applicative MicroInstruction where
  pure f = MicroInstruction x x x x x x x x x x x where
    x = repeat f
  liftA2 m (MicroInstruction a b c d e f g h i j k)
    (MicroInstruction n o p q r s t u v w x) =
    MicroInstruction (z a n) (z b o) (z c p) (z d q) (z e r) (z f s) (z g t)
    (z h u) (z i v) (z j w) (z k x) where
    z = zipWith m

data CPUInputs a = CPUInputs {instr :: [a],
                              memR :: [a],
                              inter :: a,
                              ivec :: [a]} -- 3
                   deriving (Functor)
  
data RegisterOutputs a = RegisterOutputs {pc :: [a], -- 16
                                          memA :: [a], -- 16
                                          memW :: [a]} -- 8
                         deriving (Functor)
                              

data RegisterSet a = RegisterSet {eights :: [a], -- 16
                                  sixteens :: [a], -- 9 (j first)
                                  eightSrc :: [a], -- 8
                                  sixteenBus :: [a], -- 16
                                  sixteenID :: [a], -- 16
                                  sixteenOther :: [a], -- 4 (sl 3)
                                  setpIE :: a,
                                  clearIE :: a,
                                  screwy :: a,
                                  newF :: [a]} -- 5
                     deriving (Functor)
                                
data CPURegisters a = CPURegisters {regA :: [a],
                                  regB :: [a],
                                  regC :: [a],
                                  regD :: [a],
                                  regE :: [a],
                                  regF :: [a], -- 5
                                  regH :: [a],
                                  regL :: [a],
                                  regIP :: [a], -- 16
                                  regSP :: [a], -- 16
                                  regMA :: [a], -- 16
                                  regMW :: [a],
                                  regj :: [a],
                                  fIE :: a,
                                  fpIE :: a,
                                  weirdM :: a}
                      deriving (Functor)

flags :: CPURegisters a -> [a]
carry :: CPURegisters a -> a

flags = tail . regF
carry = head . regF

regOutput st = RegisterOutputs (regIP st) (regMA st) (regMW st)

decode :: (Signalish a) => MicroInstruction a -> CPUInputs a ->
          CPURegisters a -> RegisterSet a
-- g (current output of ALU), set state

registers :: RegisterSet Signal -> Signal -> Signal -> Signal ->
             CPURegisters Signal
-- clocko, then clocki, then reset.

sixteenRegSimple :: Signal Bool -> Signal -> Signal -> Signal ->
                    Signal -> Signal -> [Signal] ->
                    [Either Bool Signal] ->
                    ([Signal], [Signal])
sixteenRegSimple setH setL setA co ci rs s8 sa b =
  (registerz co ci (setH ||| setA) rs $
    zipWith ((. Right) . (wrapPlain .) . muxb setH) h s8,
   registerz co ci (setL ||| setA) rs $
   zipWith ((. Right) . (wrapPlain .) . muxb setL) l s8) where
  (h, l) = splitAt 8 sa
  

sixteenReg :: Signal -> Signal -> Signal -> Signal -> Signal ->
              Signal -> Signal -> [Signal] -> [Signal] ->
              [Signal] -> ([Signal], [Signal])

sixteenReg setH setL setA wh co ci rs s8 s0 s1 =
  sixteenRegSimple setH setL setA co ci rs s8 $
  zipWith ((Right .) . mux2 wh) s0 s1

sixteenRegSP :: Signal -> Signal -> Signal -> Signal -> Signal ->
                Signal -> Signal -> [Signal] -> [Signal] ->
                [Signal] -> [Either Bool (Signal)] ->
                ([Signal], [Signal])

sixteenRegSP setH setL setA setB co ci rs s8 s2 s3 s1 =
  sixteenRegSimple setH setL (setA ||| setB) co ci rs s8 $
  zipWith (muxb setA) s1 $ zipWith ((Right .) . mux2 setB) s2 s3

registers set co ci rs = CPURegisters a b c d e f h l ip sp
                      ma mw rj ien pie wm where
  eightset = ensureLength 16 $ eights set
  eightsrc = ensureLength 8 $ eightSrc set
  sixteenset = ensureLength 9 $ sixteens set
  b16 = ensureLength 16 $ sixteenBus set
  idec = ensureLength 16 $ sixteenID set
  initEights = map (flip (register ck) `flip` eightsrc) $ eightset
  a = initEights !! 7
  b = initEights !! 0
  c = initEights !! 1
  d = initEights !! 2
  e = initEights !! 3
  f = registerAW co ci $ ensureLength 5 $ newF set
  (h, l) = sixteenReg (eightset !! 4) (eightset !! 5) (sixteenset !! 1)
           (sixteenset !! 2) co ci high eightsrc idec b16
  ip = uncurry (++) $
    sixteenRegSP (eightset !! 8) (eightset !! 9) (sixteenset !! 7)
    (sixteenset !! 8) co ci rs eightsrc idec b16 $ map Right rj
  sp = uncurry (++) $
    sixteenReg (eightset !! 14) (eightset !! 15) (sixteenset !! 3)
    (sixteenset !! 4) co ci high eightsrc idec b16
  ma = uncurry (++) $
       sixteenRegSP (eightset !! 12) (eightset !! 13) (sixteenset !! 5)
       (sixteenset !! 6) co ci high eightsrc idec b16 $
       (replicate 8 $ Left True) ++ map Right c
  mw = initEights !! 6
  rj = uncurry (++) $ sixteenRegSimple (eightset !! 10) (eightset !! 11)
       (sixteenset !! 0) co ci high eightsrc $
       (replicate 8 $ Left True) ++
       map Right (ensureLength 4 $ sixteenOther set) ++
       (replicate 3 $ Left True)
  [ien] = registerz co ci (pie ||| clearIE set) rs [neg $ clearIE set]
  [pie, wm] = registerAWz co ci rs [setpIE set, screwy set]


ensureLength :: Int -> [a] -> [a]
ensureLength i = elems . listArray (1, i)

decode mi inp st = RegisterSet eset sset aluo b16 id16 o16
                     spie cie wm fnew where
  [opA, opB, kL, cL, kR, cR, cf, co] = aluC mi
  eset = foldr (\s l -> map (&&& s) l ++ map (&&& neg s) l) [fromBool True] $
         setEights mi
  sset = (miscFlags mi !! 0) : ss
  (aluo, aluz, aluh, g) = alu opA opB kL cL kR cR cf co alul alur
  spie = miscFlags mi !! 1
  cie = miscFlags mi !! 2
  fa = map (&&& (fM mi !! 1)) $ memR inp
  fb = head fa : zipWith (|||) (tail fa) (fS mi)
  fc = zipWith (&&&) (fR mi) $ map (wrapPlain . fromJust) $
    zipWith3 muxc (fM mi) (map (Just . Right) $ flags st)
    [Just $ Right aluz, Nothing, Just $ Right aluh, Just $ Right g]
  fnew = g : zipWith (|||) fb fc
  b16 = mmux (bus16 mi) [regB st ++ regC st, regD st ++ regE st,
                         regH st ++ regL st, regSP st]
  id16 = incDec (head $ iDec mi) $
         mmux (tail $ iDec mi) [regIP st, regSP st, regH st ++ regL st,
                                regMA st]
  alul = mmux (aluL mi) [regA st, instr inp, regH st, regL st]
  (daah, daal) = daa (regF st !! 2) (regF st !! 3) (regF st !! 4) (regA st)
  ralu = array (0, 15) $ map (second $ map (Just . Right))
         [(0, regB st), (1, regC st), (2, regD st), (3, regE st),
          (4, regH st), (5, regL st), (6, memR inp), (7, regA st),
          (8, take 8 $ regIP st), (9, drop 8 $ regIP st),
          (10, instr inp),
          (14, take 8 $ regSP st), (15, drop 8 $ regSP st)]
  ralub = ralu //
    [(11, map Just [Left False, Right daah, Right daah, Left False,
                    Left False, Right daal, Right daal, Left False]),
     (12, map Just $ (map Right $ tail $ regF st) ++ replicate 4 (Left False)),
     (13, replicate 8 Nothing)]
  alur = mmuxc (aluR mi) $ elems ralub
  interrupted = inter inp &&& fIE st
  o16 = interrupted :
    zipWith (mux2 interrupted) (take 3 $ drop 2 $ instr inp) (ivec inp)
  p16 = setSixteens mi
  naffip = (p16 !! 6) |!| (p16 !! 7)
  nincip = (p16 !! 6) !|| (p16 !! 7) ||| ors (iDec mi)
  wm = weirdM st &&& naffip ||| (miscFlags mi !! 3) &&&
       neg (fpIE st ||| fIE st)
  -- Weird mode on if HALT and interrupts off,
  -- otherwise off if would affect IP,
  -- otherwise no change.
  ss = uncurry (++) $ second (map (&&& (nincip ||| neg (weirdM st)))) $
       splitAt 6 p16
  -- If weird mode on and would increment IP, don't.
