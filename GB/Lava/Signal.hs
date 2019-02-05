{-# LANGUAGE DeriveFunctor, TypeFamilies #-}

module GB.Lava.Signal (Signal, Sig(..),
                       bool, low, high,
                       inv, and2, or2, xor2,
                       impl, nand2, nor2, xnor2,
                       mux, dff, dffZ,
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
           | Dff a a -- write value
           | DffZ a a a -- write zero value
           | Mux a a a
           deriving (Functor, Show)
newtype Signal = Signal { getSignal :: Sig Signal }
  deriving (Show)

instance Traversable Sig where
  sequenceA s = case s of
    Bool a -> pure $ Bool a
    And x y -> And <$> x <*> y
    Or x y -> Or <$> x <*> y
    Xor x y -> Xor <$> x <*> y
    Var a b -> pure $ Var a b
    Dff x y -> Dff <$> x <*> y
    DffZ x y z -> DffZ <$> x <*> y <*> z
    Mux x y z -> Mux <$> x <*> y <*> z

instance Foldable Sig where
  foldMap = foldMapDefault

instance MuRef Signal where
  type DeRef Signal = Sig
  mapDeRef = (. getSignal) . traverse

bool :: Bool -> Signal
low, high :: Signal
inv :: Signal -> Signal
and2, or2, xor2, impl, nand2, nor2, xnor2 :: (Signal, Signal) -> Signal
mux :: Signal -> Signal -> Signal -> Signal
dff :: Signal -> Signal -> Signal
dffZ :: Signal -> Signal -> Signal -> Signal
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
dff = (Signal .) . Dff
dffZ = ((Signal .) .) . DffZ
var = Signal . flip Var 0
varPosn = Signal . uncurry Var
