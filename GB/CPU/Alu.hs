module GB.CPU.Alu (alu, daa) where
import GB.Util.Base
import Data.Function (on)

-- Arguments
-- (note: arg for BIT/SET/RES is 0bdef, c means carry flag, C means !c)
-- inst   op kL cL kR cR cf co L R
-- ADC  r 11 1  0  1  0  c  X  A r
-- ADD  r 11 1  0  1  0  0  X  A r
-- AND  r 10 1  0  1  0  X  0  A r
-- BIT  r 01 d  e  f  1  X  1  X r
-- CP   r 11 1  0  1  1  1  X  A r
-- DAA  r 11 1  0  1  n  n  X  A DAA(n,h,c,A)
-- OR   r 10 1  1  1  1  X  1  A r
-- RES  r 01 d  e  f  1  0  1  X r
-- RL   r 01 1  0  0  c  0  0  X r
-- RLC  r 01 1  0  1  0  0  0  X r
-- RR   r 01 0  c  1  0  1  0  X r
-- RRC  r 01 1  0  1  0  1  0  X r
-- SBC  r 11 1  0  1  1  C  X  X r
-- SET  r 01 d  e  f  1  1  1  X r
-- SL   r 01 1  0  0  0  0  0  X r
-- SRA  r 01 X  X  X  0  1  1  X r
-- SRL  r 01 0  0  1  0  1  0  X r
-- SUB  r 11 1  0  1  1  1  X  A r
-- SWAP r 01 X  X  X  0  0  1  X r
-- XOR  r 00 1  0  1  0  X  X  A r
-- LD *,r 00 0  0  1  0  X  X  X r



alu :: (Signalish a) => a -> a -> a -> a -> a -> a -> a -> a -> [a] -> [a] ->
       ([a], a, a, a)

-- opH, opL, keepLeft, compLeft, keepRight, compRight, carry, compResult
-- output is:
-- result, z, h, c

misc :: (Signalish a) => a -> a -> a -> a -> a -> a -> [a] -> [a] -> ([a], a)

alu opA opB kL cL kR cR c co l r = (result, zero, half, carry)
  where modL = map ((^^^ cL) . (&&& kL)) l
        modR = map ((^^^ cR) . (&&& kR)) r
        modLM = map ((^^^ cL) . (&&& kL)) l
        resultXor = zipWith (^^^) modL modR
        resultAnd = map (^^^ co) $ zipWith (&&&) modL modR
        (resultLSum, half) = (adder c `on` drop 4) modL modR
        (resultHSum, carrySum) = (adder c `on` take 4) modL modR
        resultSum = resultHSum ++ resultLSum
        resultB = zipWith (mux2 opB) resultAnd resultSum
        (resultMisc, carryMisc) = misc kL cL kR cR co c r (modLM ++ modR)
        carry = mux2 opA carryMisc carrySum
        resultA = zipWith (mux2 opB) resultXor resultMisc
        result = zipWith (mux2 opA) resultA resultB
        zero = neg $ mux2 (ands [cR, co, neg opA, opB]) (ors result) carryMisc


shift :: (Signalish a) => a -> [a] -> ([a], a)

shift d s = (zipWith (mux2 d) leftRes rightRes, mux2 d leftCar rightCar)
  where (leftCar:leftRes) = take 9 s
        (rightCar:fl) = take 9 $ reverse s
        rightRes = reverse fl

swap :: [a] -> [a]
swap = uncurry (flip (++)) . splitAt 4

bit :: (Signalish a) => [a] -> [a] -> a

bit [] [a] = a
bit (s:ss) as = bit ss $ uncurry (flip $ zipWith (mux2 s)) $ splitAt l as
  where l = length as `div` 2

setI :: (Signalish a) => [a] -> [a]
setI = foldr (\s l -> map (&&& s) l ++ map (&&& neg s) l) [fromBool True]

setAndTest :: (Signalish a) => a -> [a] -> [a] -> ([a], a)

setAndTest b s a = (zipWith (flip flip b . mux2) (setI s) a, bit s a)

sra :: [a] -> ([a], a)
sra s@(a:_) = (a:h, t) where
  (h, [t]) = splitAt 7 s

misc kL cL kR cR co c a sh = (result, carry)
  where (resultS, carryS) = shift c sh
        (resultA, carryA) = sra a
        resultW = swap a
        (resultB, zeroB) = setAndTest c [kL, cL, kR] a
        result = zipWith (mux2 co) resultS resultQ
        carry = mux2 co carryS carryQ
        resultQ = zipWith (mux2 cR) resultP resultB
        carryQ = mux2 cR carryA zeroB
        resultP = zipWith (mux2 c) resultW resultA

daa :: (Signalish a) => a -> a -> a -> [a] -> (a, a)

compToNine :: (Signalish a) => a -> [a] -> a
compToNine l [a, b, c, d] = a &&& (b ||| c ||| d &&& l)

daa n h c a = (ch &&& iN ||| c, cl &&& iN ||| h)
  where iN = neg n
        (ah, al) = splitAt 4 a
        cl = compToNine (fromBool True) al
        ch = compToNine cl ah
