module GB.CPU (cpuChunk) where
import GB.Lava.Signal hiding (neg)
import GB.Util.Base
import GB.Util.NetList

data CPUOutputs = CPUOutputs { progCounter :: [Signal],
                               memoryAccess :: [Signal],
                               memoryByte :: [Signal],
                               write :: Signal,
                               inHalt :: Signal,
                               inStop :: Signal } deriving (Eq, Show)

fullCPU :: CPUInputs (Signal) -> Signal -> Signal -> CPUOutputs
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

fullCPUParens :: (([Signal], [Signal], Signal, [Signal]),
                  (Signal, Signal)) ->
                 ([Signal], [Signal], [Signal],
                  Signal, Signal, Signal)

inputFutz :: ([Signal], [Signal], Signal, [Signal]) ->
             CPUInputs (Signal)

outputFutz :: CPUOutputs -> ([Signal], [Signal], [Signal],
                             Signal, Signal, Signal)

inputFutz (a, b, c, d) = CPUInputs a b c d
outputFutz (CPUOutputs a b c d e f) = (a, b, c, d, e, f)

fullCPUParens = outputFutz . uncurry (uncurry . f . inputFutz)

cpuChunk :: NetList
cpuChunk = listify fullCPUParens
           ((("instr", 8), ("memR", 8),
              "irq", ("ivec", 3)),
             ("clock", "reset"))
           (("pc", 16), ("memA", 16), ("memW", 8),
            "wt", "halted", "stopped")
           
