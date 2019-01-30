{-# LANGUAGE FlexibleInstances #-}
module GB.Util.Base where

import GB.Lava.Signal
import Data.List (foldl')
import Control.Arrow (first)
import Control.Monad (join)
import Data.Function (on, fix)

if' :: Bool -> a -> a -> a
if' True = const
if' False = const id

class Signalish s where
  (^^^) :: s -> s -> s
  (&&&) :: s -> s -> s
  (|||) :: s -> s -> s
  (&!&) :: s -> s -> s
  (|!|) :: s -> s -> s
  (^!^) :: s -> s -> s
  (!||) :: s -> s -> s
  neg :: s -> s
  fromBool :: Bool -> s
  huh :: s
  mux2 :: s -> s -> s -> s
  
instance Signalish Bool where
  (^^^) = (/=)
  (&&&) = (&&)
  (|||) = (||)
  (&!&) = (||) `on` not
  (|!|) = (&&) `on` not
  (^!^) = (==)
  (!||) = (||) . not
  neg = not
  fromBool = id
  huh = True
  mux2 = flip . if'
  
instance Signalish Signal where
  (^^^) = curry xor2
  (&&&) = curry and2
  (|||) = curry or2
  (&!&) = curry nand2
  (|!|) = curry nor2
  (^!^) = curry xnor2
  (!||) = curry impl
  neg = inv
  fromBool = bool
  huh = fromBool True
  mux2 = mux

infixl 7 &&&
infixl 5 |||
infixl 6 ^^^
infix 7 &!&
infix 5 |!|
infix 6 ^!^
infixr 4 !||

xors :: (Signalish a) => [a] -> a
ors :: (Signalish a) => [a] -> a
xnors :: (Signalish a) => [a] -> a
ands :: (Signalish a) => [a] -> a
nors :: (Signalish a) => [a] -> a
nands :: (Signalish a) => [a] -> a

comb :: (a -> a -> a) -> [a] -> [a] -> a

xors = comb (^^^) []
ors = comb (|||) []
ands = comb (&&&) []
xnors = neg . xors
nors = neg . ors
nands = neg . ands

comb f as [] = comb f [] as
comb _ [] [a] = a
comb f as [b] = comb f [] (b : as)
comb f as (b:c:ds) = comb f (f b c : as) ds

fullAdd :: (Signalish a) => a -> a -> a -> (a, a)
fullAdd a b c = (xors [a, b, c], a &&& (b ||| c) ||| b &&& c)

halfAdd :: (Signalish a) => a -> a -> (a, a)
halfAdd a b = (a ^^^ b, a &&& b)

row :: (a -> b -> (c, a)) -> a -> [b] -> ([c], a)
row f a = foldl' (uncurry $ (. f) . (.) . first . flip (:)) ([], a) . reverse

adder :: (Signalish a) => a -> [a] -> [a] -> ([a], a)

adder = (.zip) . (.) . row (uncurry . fullAdd)

incDec :: (Signalish a) => a -> [a] -> [a]
incDec a = map (^^^ a) . fst . row halfAdd (fromBool True) . map (^^^ a)

muxb :: (Signalish a) => a -> Either Bool a -> Either Bool a -> Either Bool a

wrapPlain :: (Signalish a) => Either Bool a -> a
wrapPlain = either fromBool id

muxb m (Left a) (Left b) = if a == b then Left a
                           else Right $ if a then neg m else m
muxb m (Right a) (Right b) = Right $ mux2 m a b

muxb m (Left False) (Right x) = Right $ m &&& x
muxb m (Right x) (Left True) = Right $ m ||| x
muxb m l r = muxb (neg m) r l

type SO a = Maybe (Either Bool a)
ijSO :: Bool -> SO a
ijSO = Just . Left

muxc :: (Signalish a) => a -> Maybe (Either Bool a) ->
        Maybe (Either Bool a) -> Maybe (Either Bool a)
muxc _ Nothing x = x
muxc _ x Nothing = x
muxc m (Just x) (Just y) = Just (muxb m x y)

mmux :: (Signalish a) => [a] -> [[a]] -> [a]

mmux [] [a] = a
mmux (s:ss) as = mmux ss $
                 uncurry (zipWith $ zipWith $ mux2 s) $ splitAt l as
  where l = length as `div` 2

-- Note that these only work with actual signals.
-- Note: Rising edge.
registerz :: Signal -> Signal -> Signal -> Signal -> [Signal] -> [Signal]
-- clocko clocki write zero data
registerAWz :: Signal -> Signal -> Signal -> [Signal] -> [Signal]
-- clocko clocki zero data

registerz = (. (&&&)) . (.) . registerAWz
registerAWz = map . liftA2 (.) `on` dffZ

register :: Signal -> Signal -> Signal -> [Signal] -> [Signal]
registerAW :: Signal -> Signal -> [Signal] -> [Signal]
register = (. (&&&)) . (.) . registerAW
registerAW = (map .) . (.) `on` dff
