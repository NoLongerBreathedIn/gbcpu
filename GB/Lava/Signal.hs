{-# LANGUAGE DeriveFunctor, TypeFamilies #-}

module GB.Lava.Signal (Signal, Sig(..),
                       bool, low, high,
                       inv, and2, or2, xor2,
                       impl, nand2, nor2, xnor2,
                       mux, dff, dffZ,
                       var, varPosn, delay) where

import Data.Reify
import Data.Traversable
import Control.Applicative
import Control.Arrow

data Sig a = Bool Bool
           | Inv a
           | And a a
           | Or a a
           | Xor a a
           | Impl a a
           | Var String Int
           | Dff a a -- write value
           | DffZ Bool a a a -- zero-set-to-high write zero value
           | SRLatch Bool a a -- set-has-priority set reset
           | Mux a a a
           | Delay a
           deriving (Functor, Show)
newtype Signal = Signal { getSignal :: Sig Signal }
  deriving (Show)

instance Traversable Sig where
  sequenceA s = case s of
    Bool a -> pure $ Bool a
    And x y -> And <$> x <*> y
    Or x y -> Or <$> x <*> y
    Xor x y -> Xor <$> x <*> y
    Implies x y -> Implies <$> x <*> y
    Var a b -> pure $ Var a b
    Dff x y -> Dff <$> x <*> y
    DffZ c x y z -> DffZ a b c <$> x <*> y <*> z
    Mux x y z -> Mux <$> x <*> y <*> z
    SRLatch c x y -> SRLatch a b c <$> x <*> y
    Delay x -> Delay <$> x

instance Foldable Sig where
  foldMap = foldMapDefault

instance MuRef Signal where
  type DeRef Signal = Sig
  mapDeRef = (. getSignal) . traverse

bool :: Bool -> Signal
low, high :: Signal
inv :: Signal -> Signal
and2, or2, xor2, impl, nimpl, nand2, nor2, xnor2 :: (Signal, Signal) -> Signal
mux :: Signal -> Signal -> Signal -> Signal
dff :: Signal -> Signal -> Signal
dffZ, dffO :: Signal -> Signal -> Signal -> Signal
srRP, srSP :: Signal -> Signal -> Signal
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
impl2 = foo Impl

foo = (Signal .) . uncurry
impl = impl2
nimpl = inv . impl2
nand2 = inv . and2
nor2 = inv . nor2
xnor2 = inv . xor2
mux = ((Signal .) .) . Mux
dff = (Signal .) . Dff
dffZ = ((Signal .) .) . DffZ False
sr = (Signal .) . SRLatch False
srS = (Signal .) . SRLatch True
dffO :: ((Signal .) .) . DffZ True
var = Signal . flip Var 0
varPosn = Signal . uncurry Var
delay = Signal . Delay
