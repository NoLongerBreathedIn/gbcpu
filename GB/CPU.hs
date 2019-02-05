module GB.CPU (cpuChunk) where
import GB.Lava.Signal
import GB.Util.Base
import GB.Util.NetList
import GB.CPU.Encoder
import GB.CPU.Decoder

data CPUOutputs = CPUOutputs { progCounter :: [Signal],
                               memoryAccess :: [Signal],
                               memoryByte :: [Signal],
                               write :: Signal,
                               inHalt :: Signal,
                               inStop :: Signal } deriving (Show)

fullCPU :: CPUInputs Signal -> Signal -> Signal -> Signal -> CPUOutputs
-- inputs clocko clocki reset

fullCPU inp co ci rs = CPUOutputs (pc regOut) (memA regOut) (memW regOut)
                       (miscFlags mi !! 4)
                       (inH &&& neg interrupted &&& (fIE regC ||| fpIE regC))
                       inS where
  (stN, mi) = encode stC inp (carry regC) (fIE regC) (flags regC)
  inHS = neg (stC !! 0) &&& (stC !! 1) &&& (stC !! 2)
  -- 3F is stop, 3E is halt, nothing else starts with 3
  inH = inHS &&& neg (stC !! 6)
  inS = inHS &&& (stC !! 6)
  stC = registerAWz co ci rs stN
  regOut = regOutput regC
  interrupted = inter inp
  regC = registers (decode mi inp regC) co ci rs

fullCPUParens :: (([Signal], [Signal], Signal, [Signal]),
                  ((Signal, Signal), Signal)) ->
                 ([Signal], [Signal], [Signal],
                  Signal, Signal, Signal)

inputFutz :: ([Signal], [Signal], Signal, [Signal]) ->
             CPUInputs Signal

outputFutz :: CPUOutputs -> ([Signal], [Signal], [Signal],
                             Signal, Signal, Signal)

inputFutz (a, b, c, d) = CPUInputs a b c d
outputFutz (CPUOutputs a b c d e f) = (a, b, c, d, e, f)

fullCPUParens = outputFutz . uncurry (uncurry . uncurry . fullCPU . inputFutz)

cpuChunk :: NetList
cpuChunk = listify fullCPUParens
           ((varList "instr" 8, varList "memR" 8,
              varSing "irq", varList "ivec" 3),
             ((varSing "clocko", varSing "clocki"), varSing "reset"))
           (varList "pc" 16, varList "memA" 16, varList "memW" 8,
            varSing "wt", varSing "halted", varSing "stopped")
           
