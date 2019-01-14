module GB.Util.Symbolic (Symb(..), assign, equivalent, assignList, decode,
                         equivalentWhen, findIneq, findIneqWhen, decodeMany,
                         nonMatch, countPoss) where

import GB.Util.Base (Signalish,
                     (&&&),
                     (|||),
                     (!||),
                     (|!|),
                     (&!&),
                     (^^^),
                     (^!^),
                     neg,
                     fromBool,
                     huh)

import Data.Function (on)
import Data.List (delete)
import Control.Arrow (first, second)
import Control.Applicative ((<|>))
import Control.Monad.State
import Data.Maybe

data Symb a = Xor (Symb a) (Symb a)
            | And (Symb a) (Symb a)
            | Not (Symb a)
            | Inj Bool
            | Symb a
            | Unknown
  deriving (Eq, Show)

instance Signalish (Symb a) where
  (!||) = (neg .) . (. neg) . (&&&) -- x -> y <=> !(x && !y)
  (|||) = (neg .) . (&&&) `on` neg -- x || y <=> !(!x && !y)
  (|!|) = (&&&) `on` neg -- x |!| y <=> !x && !y
  (&!&) = (neg .) . (&&&) -- x &!& y <=> !(x && y)
  (^!^) = (^^^) . neg -- x <-> y <=> !x ^^ y
  fromBool = Inj
  neg Unknown = Unknown
  neg (Not x) = x -- !!x <=> x
  neg (Inj x) = Inj (not x) -- !T <=> F, !F <=> T
  neg x = Not x
  Inj True &&& x = x -- T && x <=> x
  Inj False &&& _ = Inj False -- F && x <=> F
  x &&& Inj True = x -- x && T <=> x
  _ &&& Inj False = Inj False -- x && F <=> F
  Unknown &&& _ = Unknown
  _ &&& Unknown = Unknown
  x &&& y = And x y
  Unknown ^^^ _ = Unknown
  _ ^^^ Unknown = Unknown
  Inj True ^^^ x = neg x -- T ^^ x <=> !x
  Inj False ^^^ x = x -- F ^^ x <=> x
  x ^^^ Inj True = neg x -- x ^^ T <=> !x
  x ^^^ Inj False = x -- x ^^ F <=> x
  Not x ^^^ y = neg (x ^^^ y) -- !x ^^ y <=> !(x ^^ y)
  x ^^^ Not y = neg (x ^^^ y) -- x ^^ !y <=> !(x ^^ y)
  x ^^^ y = Xor x y
  huh = Unknown

assign :: (Signalish b) => (a -> b) -> Symb a -> b
assign f (Xor x y) = assign f x ^^^ assign f y
assign f (And x y) = assign f x &&& assign f y
assign f (Not x) = neg (assign f x)
assign _ (Inj x) = fromBool x
assign _ Unknown = huh
assign f (Symb a) = f a

get_first :: Symb a -> Maybe a
get_first (Xor x y) = get_first x <|> get_first y
get_first (And x y) = get_first x <|> get_first y
get_first (Not x) = get_first x
get_first (Inj _) = Nothing
get_first (Symb a) = Just a

findIneq :: (Eq a) => Symb a -> Symb a -> Maybe [(a, Bool)]

findIneq x y = findIneqH x y []

findIneqH Unknown _ _ = Nothing
findIneqH _ Unknown _ = Nothing

findIneqH x y l = case get_first x <|> get_first y of
  Nothing -> if x == y then Nothing else Just l
  Just a -> (findIneqH `on` assign (repl a False)) x y ((a,False):l) <|>
            (findIneqH `on` assign (repl a True)) x y ((a,True):l)

equivalent :: (Eq a) => Symb a -> Symb a -> Bool

equivalent x y = maybe True (const False) $ findIneq x y

on3 :: (a -> a -> a -> b) -> (c -> a) -> (c -> c -> c -> b)
on3 f g = (`on` g) . f . g

findIneqWhen :: (Eq a) => Symb a -> Symb a -> Symb a -> Maybe [(a, Bool)]
findIneqWhen c x y = fiwh c x y []

fiwh :: (Eq a) => Symb a -> Symb a -> Symb a ->
        [(a, Bool)] -> Maybe [(a, Bool)]

fiwh _ Unknown _ _ = Nothing
fiwh _ _ Unknown _ = Nothing
fiwh Unknown x y l = findIneqH x y l

fiwh c x y l =
  if equivalent c (Inj True)
  then findIneqH x y l
  else findIneq c (Inj False) >> case get_first x <|> get_first y of
    Nothing -> if x == y then Nothing else Just l
    Just a -> (fiwh `on3` assign (repl a False)) c x y ((a, False):l) <|>
              (fiwh `on3` assign (repl a True)) c x y ((a, True):l)

equivalentWhen :: (Eq a) => Symb a -> Symb a -> Symb a -> Bool

equivalentWhen c x y = maybe True (const False) $ findIneqWhen c x y

repl :: (Eq a) => a -> Bool -> a -> Symb a
repl a x b = if a == b then Inj x else Symb b

if' :: Bool -> a -> a -> a
if' True = const
if' False = const id

assignList :: (Eq a) => [(a, Symb a)] -> Symb a -> Symb a
replList :: (Eq a) => a -> [(a, Symb a)] -> Symb a
replList a = foldr (uncurry (if' . (a ==))) (Symb a)

assignList = assign . flip replList

decode :: String -> Symb Char
decodeMany :: String -> [Symb Char]
decTok :: State String (Maybe (Symb Char))

which :: a -> a -> Bool -> a
which a _ True = a
which _ a False = a

multi :: State a (Maybe b) -> State a [b]
multi = fix . (. (maybe (return ([])) . (. (:)) . flip fmap)) . (>>=)
  

decTok = do
  n <- gets null
  if n then return Nothing else do
    h <- gets head
    modify' tail
    case h of
      '&' -> liftM2 (&&&) <$> decTok <*> decTok
      '|' -> liftM2 (|||) <$> decTok <*> decTok
      '^' -> liftM2 (^^^) <$> decTok <*> decTok
      ' ' -> decTok
      'x' -> return $ Just Unknown
      '!' -> fmap neg <$> decTok
      '0' -> return $ Just $ Inj False
      '1' -> return $ Just $ Inj True
      c -> return $ Just $ Symb c

decode = fromJust . evalState decTok
decodeMany = evalState $ multi decTok

nonMatch :: Symb a -> Symb a -> Symb a
nonMatch Unknown _ = Inj False
nonMatch _ Unknown = Inj False
nonMatch a b = a ^^^ b

countPoss :: (Eq a) => [a] -> Symb a -> Integer
countPoss _ Unknown = error "Hey, tried to count unknown!"
countPoss l x
  | equivalent x (Inj True) = 2 ^ length l
  | equivalent x (Inj False) = 0
  | otherwise = cp (get_first x) l x

cp :: (Eq a) => Maybe a -> [a] -> Symb a -> Integer

cp Nothing _ _ = error "Can't happen"
cp (Just a) l x = m `seq` n `seq` m + n where
  m = countPoss l' $ assign (repl a True) x
  n = countPoss l' $ assign (repl a False) x
  l' = filter (/= a) l
