module GB.Lava.VhdlNew
  ( writeVhdlClk
  , writeVhdlNoClk
  , writeVhdlInputClk
  , writeVhdlInputNoClk
  , writeVhdlInputOutputClk
  , writeVhdlInputOutputNoClk
  )
 where

import Lava.Signal
import Lava.Netlist
import Lava.Generic
import Lava.Sequent
import Lava.Error
import Lava.LavaDir

import Data.List
  ( intersperse
  , nub
  )

import System.IO
  ( stdout, openFile, IOMode(..), hPutStr, hClose,
  BufferMode (..)
  , hSetBuffering
  )
import System.Directory (removeFile)
import Data.IORef

--import IOBuffering
--  ( noBuffering
--  )

--import IOExts
--  ( IORef
--  , newIORef
--  , readIORef
--  , writeIORef
--  )

import System.Exit (ExitCode(..))

--import System
--  ( system
--  , ExitCode(..)
--  )

----------------------------------------------------------------
-- write vhdl

writeVhdlClk :: (Constructive a, Generic b) => String -> (a -> b) -> IO ()
writeVhdlClk = writeVhdl True

writeVhdlNoClk :: (Constructive a, Generic b) => String -> (a -> b) -> IO ()
writeVhdlNoClk = writeVhdl False

writeVhdl :: (Constructive a, Generic b) => Bool -> String -> (a -> b) -> IO ()
writeVhdl clocked name circ =
  do writeVhdlInput clocked name circ (var "inp")

writeVhdlInputClk :: (Generic a, Generic b) => String -> (a -> b) -> a -> IO ()
writeVhdlInputClk = writeVhdlInput True

writeVhdlInputNoClk :: (Generic a, Generic b) => String -> (a -> b) -> a -> IO ()
writeVhdlInputNoClk = writeVhdlInput False

writeVhdlInput :: (Generic a, Generic b) => Bool -> String -> (a -> b) -> a -> IO ()
writeVhdlInput clocked name circ inp =
  do writeVhdlInputOutput clocked name circ inp (symbolize "outp" (circ inp))

writeVhdlInputOutputClk :: (Generic a, Generic b)
                     => String -> (a -> b) -> a -> b -> IO ()
writeVhdlInputOutputClk = writeVhdlInputOutput True

writeVhdlInputOutputNoClk :: (Generic a, Generic b)
                     => String -> (a -> b) -> a -> b -> IO ()
writeVhdlInputOutputNoClk = writeVhdlInputOutput False


writeVhdlInputOutput :: (Generic a, Generic b)
                     => Bool -> String -> (a -> b) -> a -> b -> IO ()
writeVhdlInputOutput clocked name circ inp out =
  do writeItAll clocked name inp (circ inp) out

writeItAll :: (Generic a, Generic b) => Bool -> String -> a -> b -> b -> IO ()
writeItAll clocked name inp out out' =
     writeDefinitions clocked file name inp out out'
  where
    file = name ++ ".vhd"
  
----------------------------------------------------------------
-- definitions

halve :: [a] -> ([a],[a])
halve = (`div` 2) . length >>= splitAt

writeDefinitions :: (Generic a, Generic b)
                 => Bool -> FilePath -> String -> a -> b -> b -> IO ()
writeDefinitions clocked file name inp out out' =
  do firstHandle  <- openFile firstFile WriteMode
     secondHandle <- openFile secondFile WriteMode
     var <- newIORef 0
     

     hPutStr firstHandle $ unlines $
       [ "library ieee;"
       , ""
       , "use ieee.std_logic_1164.all;"
       , ""
       , "entity"
       , "  " ++ name
       , "is"
       , "port"
       , "  ( "
       , if clocked then "    clk : in std_logic ;" else " "
       , "    "] ++   -- , "  -- inputs"] ++
       [ "    " ++ v ++ " : in std_logic" | VarBool v <- [head inps]] ++
       [ "  ; " ++ v ++ " : in std_logic"
       | VarBool v <- tail inps
       ] ++
       [ ""
       , "  " -- outputs
       ] ++
       [ "  ; " ++ v ++ " : out std_logic"
       | VarBool v <- outs'
       ] ++
       [ "  );"
       , "end " ++ name ++ ";"
       , ""
       , "architecture"
       , "  structural"
       , "of"
       , "  " ++ name
       , "is"
       ]

       
     hPutStr secondHandle $ unlines $
       [ "begin"
       ]
     
     let new =
           do n <- readIORef var
              let n' = n+1; v = "w" ++ show n'
              writeIORef var n'
              hPutStr firstHandle ("  signal " ++ v ++ " : std_logic;\n")
              return v

         defXor a t x@[_, _] = define a (Or x) >> define t (And x)
         defXor a t (x:ys@[_,_]) = do
           [a0, t0, w0] <- sequence $ replicate 3 new
           defXor a0 t0 ys
           define a $ Or [x, a0]
           define w0 $ And [x, a0]
           define t $ Or [t0, w0]
         defXor a t xs = do
           [a0, t0, a1, t1, ab, te] <- sequence $ replicate 6 new
           let (x0, x1) = halve xs
           defXor a0 t0 x0
           defXor a1 t1 x1
           define a $ Or [a0, a1]
           define ab $ And [a0, a1]
           define te $ Or [t0, t1]
           define t $ Or [ab, te]
         define v s =
           case s of
             Bool True     -> port "vdd" v []
             Bool False    -> port "gnd" v []
             Inv x         -> port "invG" v [x]

             And []        -> define v $ Bool True
             And [x]       -> port "wire" v  [x]
             And [x,y]     -> port "andG" v [x,y]
             And xs        -> do [w0, w1] <- sequence [new, new]
                                 let (r0, r1) = halve xs
                                 define w0 $ And r0
                                 define w1 $ And r1
                                 define v $ And [w0, w1]


             Or  []        -> define v $ Bool False
             Or  [x]       -> port "wire" v  [x]
             Or  [x,y]     -> port "orG" v [x,y]
             Or  xs        -> do [w0, w1] <- sequence [new, new]
                                 let (r0, r1) = halve xs
                                 define w0 $ Or r0
                                 define w1 $ Or r1
                                 define v $ Or [w0, w1]


             Xor  []       -> define v $ Bool False
             Xor  [x]      -> port "wire" v [x]
             Xor  [x,y]    -> port "xorG" v [x,y]
             Xor  xs       -> do [a, t, nt] <- sequence $ replicate 3 new
                                 defXor a t xs
                                 define nt $ Inv t
                                 define v $ And [a, nt]
               

             VarBool s     -> port "wire" v [s]
             DelayBool x y -> if clocked then port "dff" v [y] else wrong Lava.Error.DelayEval
             If x y z      -> port "mux2" v [x, z, y];
             
             _             -> wrong Lava.Error.NoArithmetic
            


            
         port name v args =
           hPutStr secondHandle $
           "  " ++ make 9 ("c_" ++ v)
           ++ " : entity work." ++ make 5 name
           ++ " port map (" ++ concat (intersperse ", " (args ++ [v]))
           ++ ");\n"    

     outvs <- netlistIO new define (struct out)
     hPutStr secondHandle $ unlines $
       [ ""
       , "  " -- , "  -- naming outputs"
       ]
     
     sequence
       [ define v' (VarBool v)
       | (v,v') <- flatten outvs `zip` [ v' | VarBool v' <- outs' ]
       ]
     
     hPutStr secondHandle $ unlines $
       [ "end structural;"
       ]
     
     hClose firstHandle
     hClose secondHandle
     
     --system ("cat " ++ firstFile ++ " " ++ secondFile ++ " > " ++ file)
     cat firstFile secondFile file
     --system ("rm " ++ firstFile ++ " " ++ secondFile)
     removeFile firstFile
     removeFile secondFile
 where
  sigs x = map unsymbol . flatten . struct $ x
  
  inps  = sigs inp
  outs' = sigs out'
 
  firstFile  = file ++ "-1"
  secondFile = file ++ "-2"

  make n s = take (n `max` length s) (s ++ repeat ' ')

cat a b c = readFile a >>= writeFile c >> readFile b >>= appendFile c

----------------------------------------------------------------
-- the end.

