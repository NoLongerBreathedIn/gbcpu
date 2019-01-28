{-# LANGUAGE DeriveFunctor #-}

module GB.Lava.Signal (Signal, Sig(..),
                       bool, low, high,
                       inv, and2, or2, xor2,
                       impl, nand2, nor2, xnor2,
                       mux, delay, dff, delayZ, dffZ,
                       var, varPosn) where

import Data.Reify
import Data.Traversable
import Control.Applicative
import Control.Arrow

data Sig a = Bool Bool
           | Inv a
           | And a a
           | Or a a
           | Xor a a
           | Var String Int
           | Delay a a -- clock value
           | DelayZ a a a
           | DelayW a a a -- clock write value
           | DelayWZ a a a a
           | Mux a a a
           deriving (Functor, Show)
newtype Signal = Signal { getSignal :: Sig Signal }
  deriving (Show)

instance Traversable Sig where
  sequenceA s = case s of
    x@(Bool _) -> pure x
    And x y -> And <$> x <*> y
    Or x y -> Or <$> x <*> y
    Xor x y -> Xor <$> x <*> y
    x@(Var _) -> pure x
    Delay x y -> Delay <$> x <*> y
    DelayW x y z -> DelayW <$> x <*> y <*> z
    DelayZ x y z -> DelayZ <$> x <*> y <*> z
    DelayWZ x y z w -> DelayWZ <$> x <*> y <*> z <*> w
    Mux x y z -> Mux <$> x <*> y <*> z

instance Foldable Sig where
  foldMap = foldMapDefault

instance MuRef Signal where
  type DeRef Signal = Sig
  mapDeRef = traverse

bool :: Bool -> Signal
low, high :: Signal
inv :: Signal -> Signal
and2, or2, xor2, impl, nand2, nor2, xnor2 :: (Signal, Signal) -> Signal
mux, dff, delayZ :: Signal -> Signal -> Signal -> Signal
delay :: Signal -> Signal -> Signal
dffZ :: Signal -> Signal -> Signal -> Signal -> Signal
var :: String -> Signal
varPosn :: (String, Int) -> Signal

bool = Signal . Bool
low = bool False
high = bool True
inv = Signal . Inv

foo :: (Signal -> Signal -> Sig Signal) -> (Signal, Signal) -> Signal
and2 = foo And
or2 = foo Or
xor2 = foo Xor

foo = (Signal .) . uncurry
impl = or2 . first inv
nand2 = inv . and2
nor2 = inv . nor2
xnor2 = inv . xor2
mux = ((Signal .) .) . Mux
delay = (Signal .) . Delay
dff = ((Signal .) .) . DelayW
delayZ = ((Signal .) .) . DelayZ
dffZ = (((Signal .) .) .) . DelayWZ
var = Signal . flip Var 0
varPosn = Signal . uncurry Var
