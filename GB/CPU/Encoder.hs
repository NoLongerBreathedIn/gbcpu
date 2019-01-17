{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-missing-fields #-} -- Ideally, just ignore aluL noninit.
-- Probably should turn this off to check, passing through
-- grep 'not initialised:' | grep -v ': aluL$'

module GB.CPU.Encoder (encode, compz) where

import GB.CPU.Decoder hiding (decode, registers, output, CPURegisters)
import GB.Util.Base
import Control.Applicative
import Control.Arrow (first, second)
import Data.Maybe (fromJust)

encode :: (Signalish a) => [a] -> CPUInputs a -> a -> a -> [a] ->
          ([a], MicroInstruction a)
-- state, inputs, c, fIE, F; output is state' and mi.

newtype PR f a = PR {unPR :: ([a], f a)}

instance Functor f => Functor (PR f) where
  fmap f = PR . first (map f) . second (fmap f) . unPR

instance Applicative f => Applicative (PR f) where
  pure f = PR (repeat f, pure f)
  liftA2 f (PR (a,b)) (PR (c, d)) = PR (zipWith f a c, liftA2 f b d)

compz :: (Signalish a) => a -> a -> a -> a -> a -> a -> a -> a
compz f k l p q fC fZ = let [c, n] = zipWith (mux2 f) [p, q] [k, l] in
                          n ^^^ mux2 c fZ fC

encode st inp carr ie flag =
  let ins = instr inp in
    let {?f = st !! 0;
         ?g = st !! 1;
         ?h = st !! 2;
         ?i = st !! 3;
         ?j = st !! 4;
         ?k = st !! 5;
         ?l = st !! 6;
         ?m = ins !! 0;
         ?n = ins !! 1;
         ?o = ins !! 2;
         ?p = ins !! 3;
         ?q = ins !! 4;
         ?r = ins !! 5;
         ?s = ins !! 6;
         ?t = ins !! 7;
         ?irq = inter inp &&& ie;
         ?fN = flag !! 1;
         ?fC = flag !! 3;
         ?c = carr} in
      let {?y = compz ?f ?k ?l ?p ?q ?fC (flag !! 0);
           ?u = ?o &&& ?p &&& neg ?q;
           ?v = ?r &&& ?s &&& neg ?t} in
        let ?z = neg ?y in
          let (a, b) = unPR $ ((wrapPlain . fromJust) .) . muxc ?f <$>
                (muxc ?g <$> (muxc ?h <$> (muxc ?i <$>
                                           (muxc ?j <$> (muxc (?irq !|| ?l) <$>
                                                         PR (encodeInt 0) <*>
                                                         PR encodeBase) <*>
                                            PR encodeRet) <*> PR encodeLDA) <*>
                              (muxc ?j <$> (muxc ?k <$> PR (encodeInt 1) <*>
                                            PR encodeJr) <*>
                                PR encodeCB)) <*> PR encodeNop) <*>
                PR (encodeNextH, encodeMostH {setEights = encodeSetH}) in
            (a, b {aluL = [neg $ ?m ||| ?s ||| ?f ||| ?h,
                           mux2 ?f (?h ||| neg (?l ||| ?m ||| ?s)) (neg ?g)]})

rep :: Int -> a -> [a] -> [a]
rep 0 = const id
rep n = (.) <$> (:) <*> rep (n - 1)

sel :: a -> a -> Bool -> a
sel a _ True = a
sel _ a False = a

inj :: Int -> SO a
inj = flip (if' <$> (/= 2) <*> ijSO . (/= 0)) Nothing

onj :: a -> SO a
onj = Just . Right

x :: Maybe a
x = Nothing

lF :: SO a
lF = Just $ Left False
lT :: SO a
lT = Just $ Left True

encodeInt :: (Signalish a) => Int -> ([SO a], MicroInstruction (SO a))
encodeInt p =
  (inj <$> [p,p,1-p,0,0,p,p], MicroInstruction {
      setEights = inj <$> [0,1,1,0],
      aluR = inj <$> [1,0,0,p],
      aluC = (inj <$> [0,0,0,0,1,0]) ++ repeat x,
      fS = repeat lF,
      fR = repeat lT,
      fM = repeat lF,
      setSixteens = lF : x : (inj <$> [1,0,1,1,0,0]),
      bus16 = inj <$> [1,1],
      iDec = inj <$> [1,0,1],
      miscFlags = inj <$> [1-p,0,1-p,0,p]})

encodeNop :: (Signalish a) => ([SO a], MicroInstruction (SO a))
encodeNop =
  (repeat $ inj 0, MicroInstruction {
      setEights = inj <$> [0,1,1,0],
      aluR = repeat x,
      aluC = repeat x,
      fS = repeat lF,
      fR = repeat lT,
      fM = repeat lF,
      setSixteens = map (fmap $ Left . (/= 0))
                    [Just 0, x, Just 0, x, x, x, Just 0, Just 0],
      bus16 = repeat x,
      iDec = repeat x,
      miscFlags = repeat $ inj 0})

encodeLDA :: (Signalish a, ?j :: a, ?k :: a, ?l :: a) =>
             ([SO a], MicroInstruction (SO a))
encodeLDA =
  ([lF,lF,lF,onj $ neg ?k, onj $ ?j &&& neg ?k,
    onj ?l, onj $ ?k |!| ?l], MicroInstruction {
      setEights = Just <$> [Right $ neg ?k, Left True, Right ?k,
                            Right $ neg ?l &&& (?k !|| ?j)],
      aluR = Just <$> [Right $ neg ?k, Right ?k, Left True, Left False],
      aluC = inj <$> [0,0,0,0,1,0,2,2],
      fS = repeat lF,
      fR = repeat lT,
      fM = repeat lF,
      setSixteens = map inj [0,2,0,2,0,0] ++ [onj $ neg ?l, lF],
      bus16 = repeat x,
      iDec = repeat lF,
      miscFlags = [x,lF,lF,lF,Just $ Right $ neg ?j &&& ?k]})

encodeRet :: (Signalish a, ?k :: a, ?l :: a) =>
             ([SO a], MicroInstruction (SO a))

encodeRet =
  (rep 4 lF (Just . Right <$> [nl, ?k &&& nl, nl]), MicroInstruction {
      setEights = inj <$> [1,0,0,0],
      aluR = inj <$> [0,1,1,0],
      aluC = inj <$> [0,0,0,0,1,0,2,2],
      fS = repeat lF,
      fR = repeat lT,
      fM = repeat lF,
      setSixteens = map inj [0,2] ++ Just (Right nl) :
        map inj [0,1,0,0,0],
      bus16 = repeat x,
      iDec = inj <$> [0,0,1],
      miscFlags = x : Just (Right $ ?k &&& ?l) : repeat lF})
  where nl = neg ?l

encodeJr :: (Signalish a, ?l :: a, ?m :: a, ?c :: a) =>
             ([SO a], MicroInstruction (SO a))
encodeJr =
  (sel nl lF . (/= 0) <$> [0,0,1,0,0,1,1], MicroInstruction {
      setEights = [lT,lF,nl,nl],
      aluR = [lT,lF,lF,nl],
      aluC = [lT,lT,nl, Just (Right $ ?m &&& ?l),
              lT,lF, Just (Right $ ?c &&& ?l), x],
      fS = repeat lF,
      fR = repeat lT,
      fM = repeat lF,
      setSixteens = [lF, x, lF, x, x, x, lF, Just $ Right $ ?l],
      bus16 = repeat x,
      iDec = repeat x,
      miscFlags = x : repeat lF})
  where nl = Just $ Right $ neg ?l

encodeNextH :: (Signalish a, ?g :: a, ?h :: a,
                ?i :: a, ?j :: a, ?k :: a, ?l :: a,
                ?y :: a, ?z :: a) => [SO a]
encodeNextH =
  zipWith ((Just .) . muxb ?g) (zipWith (muxb ?h) encodeNext4 encodeNext5)
  (zipWith (muxb ?h) encodeNext6 encodeNext7)

encodeNext4 :: (Signalish a, ?i :: a, ?j :: a, ?k :: a, ?l :: a,
                ?y :: a, ?z :: a) => [Either Bool a]
encodeNext4 =
  Right <$> [(neg ?i &&& ?j) |!| (?i ||| ?k) &&& ?l,
              ands [?i,neg ?j,?k,?l], (?i |!| ?y) &&& ?j, ianl,
              ianl &&& ?j, mux2 ?i (mux2 ?j (?k ^^^ ?l) ?z) (?k &&& neg ?l),
              neg $ mux2 (?j !|| ?i) ?y ?l]
  where ianl = ?i &&& neg ?l

encodeNext5 :: (Signalish a, ?i :: a, ?j :: a,
                ?k :: a, ?l :: a) => [Either Bool a]
encodeNext5 = [foo, Left False, Right nianl, Left False, Right $ nianl &&& ?j,
               Right $ ands [?k, neg ?l, ?i !|| ?j], foo]
  where foo = Right $ neg ?l &&& (neg ?i ||| ?j &&& ?k)
        nianl = ?i |!| ?l

encodeNext6 :: (Signalish a, ?i :: a, ?j :: a,
                ?k :: a, ?l :: a) => [Either Bool a]
encodeNext6 = [foo, foo, Left False, Left False] ++
  map Right [?j &&& neg (?i ||| ?k &&& ?l),
             bar ?l, bar (neg ?l)]
  where foo = Right $ neg ?i &&& (?k !|| ?j)
        bar = (neg ?i &&&) . (?j &&& ?k |||) . (neg ?k &&&)

encodeNext7 :: (Signalish a, ?i :: a, ?j :: a, ?k :: a, ?l :: a,
                ?z :: a) => [Either Bool a]
encodeNext7 = Right <$> [njoz, njoz, nj, ?i &&& nj, ?j !|| ?i &&& ?z,
                         mux2 ?j ?k ?z, nj &&& ?l]
  where njoz = ?j !|| ?z
        nj = neg ?j

encodeMostH :: (Signalish a, ?g :: a, ?h :: a,
                ?i :: a, ?j :: a, ?k :: a, ?l :: a,
                ?y :: a, ?z :: a, ?m :: a, ?fC :: a) =>
               MicroInstruction (SO a)

encodeMostH = muxc ?h <$> (muxc ?g <$> encodeMost4 <*> encodeMost6) <*>
              (muxc ?g <$> encodeMost5 <*> encodeMost7)

encodeMost4 :: (Signalish a, ?i :: a, ?j :: a, ?k :: a, ?l :: a,
                ?y :: a, ?m :: a, ?fC :: a) => MicroInstruction (SO a)
encodeMost4 = MicroInstruction {
  aluR = onj <$> [?i !|| ?k, ?i ||| ?k &&& neg ?j,
                  ?j !|| ?i, mux2 ?i ?j (neg ?l)],
  aluC = (onj <$> let ikj = mux2 ?i ?j ?k in [ikj, ikj, mux2 ?i ?j (neg ?l),
                                             ands [?i, ?k, ?l, ?m]]) ++
         [onj $ ?i !|| ?k ||| ?l, lF, onj $ ?i &&& ?l &&& ?fC, x],
  fS = repeat lF,
  fR = rep 2 (onj $ ?i &&& ?k !|| ?l) $ replicate 2 lT,
  fM = rep 2 lF $ replicate 2 $ onj $ ands [?i, ?k, neg ?l],
  setSixteens = concat (replicate 2 [lF, x]) ++
                map Just [Right ?k, Left False,
                          Right $ mux2 ?i (mux2 ?j (?k ^!^ ?l) ?y) ?l,
                          Left False],
  bus16 = repeat x,
  iDec = map Just $ Left False : replicate 2 (Right $ ?k &&& (?j |!| ?l)),
  miscFlags = [x,lF,lF,lF, onj $ neg ?j &&& mux2 ?k (?i &&& ?l) (neg ?i)]}

encodeMost5 :: (Signalish a, ?i :: a, ?j :: a, ?k :: a, ?l :: a) =>
               MicroInstruction (SO a)
encodeMost5 = MicroInstruction {
  aluR = inj <$> [1,0,1,0],
  aluC = inj <$> [0,0,0,0,1,0,2,2],
  fS = repeat lF,
  fR = repeat lT,
  fM = repeat lF,
  setSixteens = map inj [0,2,0,2,1,1] ++ [onj $ ?i &&& ?j &&& ?k !|| ?l, lF],
  bus16 = inj <$> [1,0],
  iDec = repeat lF,
  miscFlags = x : repeat lF}

encodeMost6 :: (Signalish a, ?i :: a, ?j :: a, ?k :: a, ?l :: a, ?fC :: a) =>
               MicroInstruction (SO a)
encodeMost6 = MicroInstruction {
  aluR = [lT,lF,onj $ ?k !|| ?i, onj $ neg ?i &&& ?k &&& ?l],
  aluC = [onj $ ?i &&& (?j &&& ?l !|| ?k), onj $ ?i &&& (?j !|| ?k &&& ?l),
          onj ?i, onj $ ands [?i,?j,?k,neg ?l], lT, onj $ ?i &&& ?k,
          onj $ ?j ||| ?k ^^^ ?l &&& ?fC, onj ?k],
  fS = [onj $ ?i &&& ?k &&& (?j !|| ?l), onj $ ?i &&& ?j &&& (?k |!| ?l), lF],
  fR = lT : onj (neg ?i) : repeat (onj $ ?i &&& ?j !|| ?k &&& ?l),
  fM = [onj ?i, lF, onj ?i, onj ?i],
  setSixteens = [lF, x, onj $ neg ?i &&& ?j &&& ?k, lF,
                 lT, lT, onj $ ?k !|| ?i, onj $ (?i |!| ?j) &&& ?k],
  bus16 = inj <$> [1,1],
  iDec = let foo = onj $ neg ?i &&& ?k in [foo, lF, foo],
  miscFlags = rep 4 lF [onj $ neg ?i &&& ?k &&& ?l]}
  
encodeMost7 :: MicroInstruction (SO a)
encodeMost7 = MicroInstruction {
  aluR = inj <$> [1,0,1,0],
  aluC = inj <$> [0,0,0,0,1,0,2,2],
  fS = repeat lF,
  fR = repeat lT,
  fM = repeat lF,
  setSixteens = inj <$> [0,2,0,2,2,2,1,0],
  bus16 = repeat x,
  iDec = repeat lF,
  miscFlags = repeat lF}
  
encodeSetH :: (Signalish a, ?g :: a, ?h :: a, ?i :: a,
               ?j :: a, ?k :: a, ?l :: a) => [SO a]
encodeSetH =
  zipWith ((Just .) . muxb ?g) (zipWith (muxb ?h)  encodeSet4 encodeSet5)
  (zipWith (muxb ?h) encodeSet6 (encodeSet7 ?j))

encodeSet4 :: (Signalish a, ?i :: a, ?j :: a,
               ?k :: a, ?l :: a) => [Either Bool a]
encodeSet4 = Right <$> [mux2 ?k (?i &!& ?l) (?i ^^^ ?j), ?j !|| ?i,
                        ands [?i, neg ?k, ?l] ||| mux2 ?j ?k (neg ?i),
                        ?j &&& (?i &!& ?k) ||| neg ?l &&& (?k !|| ?i)]

encodeSet5 :: (Signalish a, ?i :: a, ?j :: a,
               ?k :: a, ?l :: a) => [Either Bool a]
encodeSet5 = Right <$> [ands [neg ?i, ?j, ?k], ?j, ?k, ?i ^!^ ?l]

encodeSet6 :: (Signalish a, ?i :: a, ?j :: a,
               ?k :: a, ?l :: a) => [Either Bool a]
encodeSet6 = [Right $ ?i |!| ?k, Right $ ?i ||| ?k, Left True,
              Right $ ?k ||| ?l !|| ?i &&& nands [?j,?k,?l]]

encodeSet7 :: (Signalish a) => a -> [Either Bool a]
encodeSet7 = (map Left [True, False, True] ++) . (:[]) . Right . neg

encodeCB :: (Signalish a, ?l :: a, ?m :: a, ?n :: a,
             ?o :: a, ?p :: a, ?q :: a,
             ?r :: a, ?s :: a, ?t :: a,
             ?u :: a, ?v :: a, ?fC :: a) =>
            ([SO a], MicroInstruction (SO a))
encodeCB =
  (lF:lF:take 5 (concat $ repeat $
                 Just . Right <$> [?v &&& neg ?l, ?l &&& mvnn]),
   MicroInstruction {
      setEights = lF : map (Just . Right)
        [?r ||| nman, ?s ||| nman, ?t &&& mvnn],
      aluR = lF : map (Just . Right) [?r, ?s, ?t],
      aluC = lF : lT : map (Just . Right)
        [mux2 mvn (?p &!& ?q) ?o, (mvn ||| ?fC &&& (neg ?o &&& ?q)) &&& ?p,
         mvn ||| ?o ||| ?p !|| ?q, mvn ||| ?p &&& ?fC &&& (?o |!| ?q),
         ?q &&& neg ?m ||| ?n, mvn ||| ?o &&& (?p ^^^ ?q)],
      fS = [lF, Just $ Right nman, lF],
      fR = lT : map (Just . Right) [?m, ?m, ?u !|| mvn],
      fM = [Just $ Right $ neg ?m, lF, lF,
            Just $ Right $ neg mvn &&& (?v !|| ?l)],
      setSixteens = [lF,x,lF,x,lT,lT,Just $ Right $ neg $ mux2 ?l ?v mvnn,lF],
      bus16 = inj <$> [1,0],
      iDec = repeat lF,
      miscFlags = x : repeat lF})
  where mvnn = ?n !|| ?m
        nman = ?n &&& neg ?m
        mvn = ?m ||| ?n

encodeBase :: (Signalish a, ?k :: a, ?l :: a, ?m :: a, ?n :: a,
               ?o :: a, ?p :: a, ?q :: a, ?u :: a,
               ?r :: a, ?s :: a, ?t :: a, ?v :: a,
               ?fC :: a, ?fN :: a, ?y :: a, ?c :: a) =>
              ([SO a], MicroInstruction (SO a))
encodeBase =
  (zipWith ((Just .) . muxb mxn)
   (zipWith ((Right .) . mux2 ?m) encodeNext0 encodeNext3) encodeNext12,
   MicroInstruction {
      setEights = zipWith ((Just .) . muxb mxn)
        (zipWith ((Right .) . mux2 ?m) encodeSet0 encodeSet3) encodeSet12,
      aluR = zipWith ((Just .) . muxb mxn)
        (zipWith ((Right .) . mux2 ?m) encodeAR0 encodeAR3)
        (Left False : map Right [?r,?s,?t]),
      aluC = zipWith (muxc ?n) (zipWith (muxc ?m . Just) encodeAC0 encodeAC2)
        (zipWith (muxc ?m) encodeAC1 encodeAC3),
      fS = onj <$> encodeBaseFS,
      fR = onj . (||| ?n &&& (?m !|| ?r ||| ?s ||| ?k ||| (?u &!& ?t)))
           . (&&& neg ?n) <$> encodeFR02,    
      fM = onj <$> encodeBaseFM,
      setSixteens = Just <$> encodeBase16,
      bus16 = onj <$> [?m ||| ?n ||| ?o,
                       (?m ^!^ ?n) &&& mux2 ?m (neg ?o &&& ?p) ?r],
      iDec = zipWith (((Just . fmap (&&& neg mxn)) .) . muxb ?m)
        encodeID0 encodeID3,
      miscFlags = onj <$> [neg ?l, foo &&& ?q, foo &&& neg ?q,
                           ands [neg ?m, ?n, ?u, ?v],
                           ands [man, ?l, ?r, ?k ^!^ ?s]]})
  where mxn = ?m ^^^ ?n
        man = ?m &&& ?n
        foo = ands [man, neg ?r, ?s, ?t, ?o]

encodeNext0 :: (Signalish a, ?l :: a, ?o :: a, ?p :: a, ?q :: a,
                ?r :: a, ?s :: a, ?t :: a, ?v :: a) => [a]
encodeNext0 =
  [ralov ||| neg ?r &&& (neg ?q &&& (?s ^^^ ?t) |||
   (?s |!| ?t) &&& (?o ||| ?q &&& neg ?p)),
   nors [?r, ?s, ?t, ?o, ?q] &&& ?p,
   ?v ||| (?r |!| ?s) &&& mux2 ?t pano (neg ?q),
   ralov ||| nors [?q, ?t, ?r] &&& (?s ||| pano),
   nors [?r, ?s, ?q] &&& (?o ||| ?p &&& neg ?t) |||
    ?o &&& (?v ||| nors [?r, ?s, ?t]),
   ?p &&& ((?r ||| ?s ||| ?t &&& ?q) !|| ?v),
   (?l |!| ?r) &&& (?s ||| ?q &&& (?o ||| ?t)) ||| ?q &&& ?v |||
    ?p &&& neg (?q ||| mux2 ?r ?t ?s) &&& (?r ^!^ ?o)]
  where ralov = ?r &&& ?l ||| ?v
        pano = ?p &&& neg ?o

encodeNext12 :: (Signalish a, ?u :: a, ?v :: a, ?l :: a, ?n :: a) =>
             [Either Bool a]
encodeNext12 = Left False : map Right
               [e,e,d,e,d,?v &&& neg ?l &&& (?u &!& ?n)]
  where e = d &&& ?v
        d = ?u &&& ?n

encodeNext3 :: (Signalish a, ?o :: a, ?p :: a, ?q :: a, ?u :: a, ?y :: a,
                ?r :: a, ?s :: a, ?t :: a, ?k :: a, ?l :: a) => [a]
encodeNext3 =
  [neg ?t &&& (?o ^^^ ?s ||| ?u ||| ?r) |||
    mux2 ?r (?s &&& (?o |!| ?q)) (?k ||| ?q &&& neg ?s),
   ?r &&& neg ?t ||| mux2 ?s (?q &&& (?r ||| ?o &&& ?p &&& ?t))
    (?o ||| ?t &&& (?q ||| ?r) !|| ?k),
   (?o |!| ?r) &&& ?s &&& (?t !|| ?q) ||| (?s |!| ?t) &&& ?r,
   neg ?t &&& (?r ||| ?o &&& (?s !|| ?p ||| ?q)),
   mux2 ?t (?k ||| ?o &&& mux2 ?r ((?s !|| ?q) &&& ?p) ?s)
    (?q &&& mux2 ?r (?l ||| neg ?o &&& ?s) (neg ?s)),
   ?l &&& (?k ||| mux2 ?t (?y ||| ?o) (neg ?p &&& ?q) !|| ?r) |||
    neg ?t &&& (?p &&& (?r ||| neg ?o &&& ?s) ||| ?o &&& ?q &&& (?r |!| ?s)),
   mux2 ?l (?r ||| mux2 ?s ?o (?o ^^^ ?q !|| ?t)) (ors [?k, ?o, ?t, ?y]) !||
    (?q ^^^ ?t) &&& neg ?k &&& (?r ||| neg ?s &&& ?t) |||
    ?r &&& (?s &&& ?t ||| ?l)]

encodeSet0 :: (Signalish a, ?l :: a, ?o :: a, ?p :: a, ?q :: a,
               ?r :: a, ?s :: a, ?t :: a) => [a]
encodeSet0 =
  [neg ?r &&& ?t &&& mux2 ?s (neg ?q) (?o &&& ?p),
   ?r ||| ?t !|| ?o ||| ?r ^!^ ?s,
   ?p &&& ?s ||| mux2 ?r (neg ?t) (?p ||| ?s),
   mux2 (?r &&& neg ?s) (?l ^^^ ?t) ?q]

encodeSet12 :: (Signalish a, ?l :: a, ?m :: a,
                ?o :: a, ?p :: a, ?q :: a, ?v :: a) => [Either Bool a]
encodeSet12 =
  Left False : map Right [?o ||| ?m, ?p ||| ?m,
                           mux2 ?m ?q (?o &&& ?p &&& ?q |!| ?v &&& neg ?l)]

encodeSet3 :: (Signalish a, ?k :: a, ?l :: a, ?o :: a, ?p :: a, ?q :: a,
               ?r :: a, ?s :: a, ?t :: a) => [a]
encodeSet3 = [neg ?s &&& mux2 ?t ?k (?l &&& ?q), rnlkt ||| ?o,
              rnlkt ||| ?t &&& ?p &&& neg ?q,
              ?k &&& (?s |!| ?t) ||| ?l &&& neg ?r &&&
              (?o &&& ?p ||| neg ?k &&& (?o ||| ?t))] where
  rnlkt = ?l &&& (?k ||| ?t) !|| ?r
   
encodeAR0 :: (Signalish a, ?l :: a, ?o :: a, ?p :: a, ?q :: a,
              ?r :: a, ?s :: a, ?t :: a) => [a]
encodeAR0 = [mux2 ?r (mux2 ?t (neg ?s) (?o &&& ?p)) (?s &&& ?o),
             rot !|| ?o ^^^ ras, rot !|| ?p ||| ras,
             neg ?l &&& (?q ||| ?s)] where
  rot = ?r ||| ?t
  ras = ?r &&& ?s

encodeAR3 :: (Signalish a, ?k :: a, ?l :: a, ?o :: a, ?p :: a,
              ?r :: a, ?s :: a, ?t :: a) => [a]
encodeAR3 =
  [?s &&& ?t ||| ands [?l, ?o, ?p, ?r],
   ?r !|| neg ?s &&& ?o,
   ?r !|| ?p &&& (?s |!| ?k &&& ?o),
   ?r &&& ?k ||| neg ?l &&& (?r !|| ?s ||| ?o &&& ?p)]

encodeAC0 :: (Signalish a, ?l :: a, ?fN :: a, ?fC :: a, ?c :: a,
              ?o :: a, ?p :: a, ?q :: a,
              ?r :: a, ?s :: a, ?t :: a) => [Either Bool a]
encodeAC0 =
  map Right [nrat ||| ?r &&& (?s !|| ?o &&& (?p |!| ?q)),
             ?r &&& neg (?s &&& ?o &&& (?p ||| ?q)) ||| nrat,
             ras &&& (?p &&& ?q !|| ?o) ||| (?r |!| ?s) &&& ?t,
             ?t &&& mux2 ?s ?r (?q &&& (?r !|| neg ?p &&& ?o |||
                                        ?fC &&& ?p &&& neg ?o)),
             ras &!& mux2 ?q ?p ?o,
             ras &&& mux2 ?o (?p &&& neg ?q &&& ?fC) ((?p |!| ?q) &&& ?fN),
             ?t !|| neg ?r &&& ?l &&& ?c ||| ?s &&& (?l ||| ?r ^^^ ?q !||
                                                    ?r &&& ?o &&& ?fN)]
  ++ [Left False]
  where nrat = neg ?r &&& ?t
        ras = ?r &&& ?s

encodeAC1 :: [SO a]
encodeAC1 = inj <$> [0,0,0,0,1,0,2,2]

encodeAC2 :: (Signalish a, ?o :: a, ?p :: a, ?q :: a, ?fC :: a) => [SO a]
encodeAC2 =
  Just <$> [Right $ ?o &&& ?q !|| ?p, Right $ ?o !|| ?p &&& ?q,
            Left True, Right $ ?o &&& ?p &&& neg ?q,
            Left True, Right ?p, Right $ ?p ^^^ ?fC &&& ?q ||| ?o,
            Right ?p]

encodeAC3 :: (Signalish a, ?k :: a, ?l :: a,
              ?r :: a, ?s :: a, ?c :: a) => [SO a]
encodeAC3 = [onj ras, onj ras, lF, lF, lT, lF, onj $ ?l !|| ?k ||| ?c, x]
  where ras = ?r &&& ?s

encodeBaseFS :: (Signalish a, ?fC :: a, ?m :: a, ?n :: a,
                 ?o :: a, ?p :: a, ?q :: a,
                 ?r :: a, ?s :: a, ?t :: a) => [a]
encodeBaseFS =
  [neg ?n &&& mux2 ?m (?r &&& ?t &&& (?s !|| ?o &&& neg ?p &&& ?q))
    (?q &&& (?o !|| ?p)),
   (?n |!| ?p) &&& ?o &&& mux2 ?m (?r &&& ?s &&& ?t &&& ?q) (neg ?q),
   ands [?o, ?p, ?r, ?s, ?t, ?fC &&& ?q |!| (?m ||| ?n)]]

encodeFR02 :: (Signalish a, ?l :: a, ?m :: a,
               ?o :: a, ?p :: a, ?q :: a,
               ?r :: a, ?s :: a, ?t :: a, ?v :: a) => [a]
encodeFR02 =
  [?r &&& ?s &&& ?t !|| ?o ||| ?m,
   neg ?m &&& (?l ||| ?r !|| ?s &&& (?r &&& ?t !|| ?o &&& neg ?p)),
   (rst |!| ?m) ||| mopq, (rst &&& ?o &&& ?p |!| ?m) ||| mopq]
  where rst = ?r &&& ?s &&& ?t
        mopq = ?m &&& (?o !|| ?p &&& ?q) ||| ?v &&& neg ?l

encodeBaseFM :: (Signalish a, ?k :: a, ?l :: a, ?m :: a, ?n :: a,
                 ?o :: a, ?p :: a, ?q :: a, ?u :: a,
                 ?r :: a, ?s :: a, ?t :: a, ?v :: a) => [a]
encodeBaseFM =
  [neg ?n &&& (?m ||| ?r &&& (?s !|| ?t &&& (?p |!| ?q))),
   ands [?m, ?n, ?t, ?u, nors [?r, ?s, ?k]],
   neg ?n &&& (?m ||| neg ?s &&& (?r ||| ?l &&& ?t)),
   neg ?n &&& mux2 ?m (?t &&& (?r ^!^ ?s) &&& (?r &&& neg ?o ||| ?r ^^^ ?q))
    (?v !|| ?l)]

encodeBase16 :: (Signalish a, ?k :: a, ?l :: a, ?m :: a, ?n :: a,
                 ?o :: a, ?p :: a, ?q :: a, ?u :: a, ?y :: a,
                 ?r :: a, ?s :: a, ?t :: a, ?v :: a) => [Either Bool a]
encodeBase16 =
  Right (ands [?s, ?o, nors [?m, ?n, ?l, ?r, ?t]]) : Left False :
  map Right [?m &&& ?n &&&
              (ors [?r, ?s, ?t, ?o, ?l, ?y] !|| ?r &&& ?k |||
                ?t &&& mux2 ?s (?r ||| ?l ||| ?o &&& neg ?p &&& ?q !||
                               (?r ^^^ ?k ^^^ ?l) &&& neg ?q) ?l),
             ands [?o, ?q, ?t, ?r |!| ?k],
             ?m &&& ?n &&& (?l ||| ?o) !|| ?t,
             ?m &&& ?n !|| ?r ||| neg ?t &&& ?o,
             mux2 ?m
              (mux2 ?n
                (mux2 ?r (?s ||| ?q &&& ?t !|| ?l) (?u !|| ?s))
                (?u ^^^ ?v !|| ?l))
              (mux2 ?n
                (?v !|| ?l)
                (neg ?t &&& (?r ||| ?o &&& (?s !|| ?l) ||| ?l &&& ?y) |||
                 mux2 ?r (?t &&& (?s ||| ?k) ||| mux2 ?o ?s ?q)
                  (?q &&& neg ?s))),
             ands [?m, ?n, ?t, ?o, ?q, nors [?r, ?s, ?p]]]
              
               

encodeID0 :: (Signalish a, ?l :: a, ?o :: a, ?p :: a,
              ?r :: a, ?s :: a) => [Either Bool a]
encodeID0 = [Right $ rnolas &&& ?p, Right $ rnolas &&& ?o, Left False] where
  rnolas = (?r |!| ?l) &&& ?s

encodeID3 :: (Signalish a, ?k :: a, ?l :: a, ?o :: a, ?q :: a,
              ?r :: a, ?s :: a, ?t :: a) => [Either Bool a]
encodeID3 =
  [Right $ ?r &&& (?l ||| ?t &&& neg ?q),
   Left False,
   Right $ (?l ^!^ ?r) &&& (?o ||| ?s !|| ?l) ||| ?t &&& nors [?k, ?q, ?s]]
