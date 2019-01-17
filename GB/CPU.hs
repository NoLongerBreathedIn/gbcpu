module GB.CPU (cpuChunk) where
import Lava hiding (neg)
import GB.Util.Base
import GB.Util.Clean

data CPUOutputs = CPUOutputs { progCounter :: [Signal Bool],
                               memoryAccess :: [Signal Bool],
                               memoryByte :: [Signal Bool],
                               write :: Signal Bool,
                               inHalt :: Signal Bool,
                               inStop :: Signal Bool } deriving (Eq, Show)

fullCPU :: CPUInputs (Signal Bool) -> Signal Bool -> Signal Bool -> CPUOutputs
-- inputs clock zero

fullCPU inp ck rs = CPUOutputs (pc regOut) (memA regOut) (memW regOut)
                    (miscFlags mi !! 4)
                    (inH &&& neg interrupted &&& (fIE regC ||| fpIE regC))
                    inS where
  (stN, mi) = encode stC inp (carry regC) (fIE regC) (flags regC)
  inHS = neg (stC !! 0) &&& (stC !! 1) &&& (stC !! 2)
  -- 3F is stop, 3E is halt, nothing else starts with 3
  inH = inHS &&& neg (stC !! 6)
  inS = inHS &&& (stC !! 6)
  stC = registerAW ck rs stN
  regOut = regOutput regC
  interrupted = inter p
  regC = registers (decode mi inp regC) ck rs

fullCPUParens :: (([Signal Bool], [Signal Bool], Signal Bool, [Signal Bool]),
                  (Signal Bool, Signal Bool)) ->
                 ([Signal Bool], [Signal Bool], [Signal Bool],
                  Signal Bool, Signal Bool, Signal Bool)

inputFutz :: ([Signal Bool], [Signal Bool], Signal Bool, [Signal Bool]) ->
             CPUInputs (Signal Bool)

outputFutz :: CPUOutputs -> ([Signal Bool], [Signal Bool], [Signal Bool],
                             Signal Bool, Signal Bool, Signal Bool)

inputFutz (a, b, c, d) = CPUInputs a b c d
outputFutz (CPUOutputs a b c d e f) = (a, b, c, d, e, f)

fullCPUParens = outputFutz . uncurry (uncurry . f . inputFutz)

cpuChunk :: NetList
cpuChunk = listify "lr35902" fullCPUParens
           ((listVar 8 "instr", listVar 8 "memR",
              singleVar "irq", listVar 3 "ivec"),
             (Var "clock", singleVar "reset"))
           (listVar 16 "pc", listVar 16 "memA", listVar 16 "memW",
            singleVar "wt", singleVar "halted", singleVar "stopped")
           
