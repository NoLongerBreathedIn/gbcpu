{-# LANGUAGE FlexibleInstances, OverloadedStrings, TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Main where
import GB.Util.Symbolic
import GB.Util.Base
import GB.CPU.Decoder (MicroInstruction(..), CPUInputs(..))
import GB.CPU.Encoder
import Data.Yaml hiding (encode, decode)
import Data.Text (unpack)
import Data.Aeson.Types (typeMismatch)

import Control.Monad
import Control.Applicative
import Control.DeepSeq
import Control.Monad.Fail
import Control.Arrow (first, second, (***))
import Data.Maybe
import Data.Function (on)
import Data.List (intersect, unzip4, foldl')
import System.Environment (getArgs)

encoded :: ([Symb Char], MicroInstruction (Symb Char))

encoded = encode (map Symb "fghijkl")
          (CPUInputs (map Symb "mnopqrst") [] (Symb 'I') []) (Symb 'c')
                     (Inj True) $ map Symb "ZNHC"

correct :: [Symb Char] -> [Symb Char] -> Symb Char ->
           ([Symb Char], MicroInstruction (Symb Char)) ->
           ([Symb Char], MicroInstruction (Symb Char))

corr :: String -> String -> Symb Char ->
        ([Symb Char], MicroInstruction (Symb Char)) ->
        ([Symb Char], MicroInstruction (Symb Char))

corr = correct `on` decodeMany

correction st inst int = let zz = compz (Symb 'f') (Symb 'k') (Symb 'l')
                               (Symb 'p') (Symb 'q') (Symb 'C') (Symb 'Z') in
                           assignList (zip ['f'..'t'] $ st ++ inst) .
                           assignList [('y', zz), ('z', neg zz), ('I', int),
                                       ('u', Symb 'o' &&& Symb 'p' &&&
                                             neg (Symb 'q')),
                                       ('v', Symb 'r' &&& Symb 's' &&&
                                             neg (Symb 't'))]

newtype PairFunc f g a = PF {unPF :: (f a, g a)}

instance (Functor f, Functor g) => Functor (PairFunc f g) where
  fmap c = PF . first (fmap c) . second (fmap c) . unPF

correct st inst int = unPF . fmap (correction st inst int) . PF

data TestCase = TestCase {testName :: String,
                          testState :: [Symb Char],
                          testInstr :: [Symb Char],
                          testIF :: Symb Char,
                          testCond :: Symb Char,
                          testMisc :: MicroInstruction (Symb Char),
                          testNS :: [Symb Char],
                          testBM :: String}
                deriving (Show)
namesOfSymbs :: [Symb a] -> [a]
namesOfSymbs = mapMaybe (\case
                            Symb a -> Just a
                            _ -> Nothing)

instance FromJSON (Symb Char) where
  parseJSON (String s) = return $ decode $ unpack s
  parseJSON (Number s) = return $ decode $ show s
  parseJSON (Bool s) = return $ Inj s
  parseJSON other = typeMismatch "Symb Char" other

extractState v = v .: "state"
extractInstr v = v .:? "instr" .!= map Symb "mnopqrst"

ifElse :: a -> a -> Bool -> a
ifElse a _ True = a
ifElse _ a False = a

fixupBitsM :: String -> String
fixupBitsM = concatMap (\c -> if c == 'z' then "ZC" else [c])

instance FromJSON TestCase where
  parseJSON = withObject "TestCase" $ \v -> TestCase . unpack <$>
    v .: "test" <*> extractState v <*> extractInstr v <*>
    v .:? "iFlag" .!= Symb 'I' <*> v .:? "cond" .!= Inj True <*>
    (MicroInstruction <$> v .:? "set8" .!= [] <*>
      v .:? "aluR" .!= [] <*> v .:? "aluL" .!= [] <*>
    v .:? "aluC" .!= [] <*> v .:? "fS" .!= replicate 3 (Inj False) <*>
    v .:? "fR" .!= replicate 3 (Inj True) <*>
    v .:? "fM" .!= replicate 3 (Inj False) <*>
    v .:? "set16" .!= [] <*> v .:? "b16" .!= [] <*> v .:? "id" .!= [] <*>
    v .:? "miscFlags" .!= (Unknown : replicate 4 (Inj False))) <*>
    v .:? "nextState" .!= [] <*>
    (fixupBitsM <$> ((v .:? "bitsM" .!=) =<< (intersect ['f'..'t'] <$>
     (namesOfSymbs <$> ((++) <$> extractInstr v <*> extractState v)))))

test :: TestCase -> (Maybe (String, [(String, [(Char, Bool)])]),
                     [(Integer, Integer)], Integer, [Bool])

testA :: String -> Symb Char -> [Symb Char] -> [Symb Char] ->
         MicroInstruction (Symb Char) -> MicroInstruction (Symb Char) ->
         (Maybe (String, [(String, [(Char, Bool)])]), [Bool])

check :: a -> a -> Symb Char -> (a -> [Symb Char]) -> String ->
         (Maybe (String, [(Char, Bool)]), [Bool])

ck :: String -> [Symb Char] -> [Symb Char] -> Symb Char ->
         (Maybe (String, [(Char, Bool)]), [Bool])

testA name cnd a b c d =
  (\r -> case catMaybes r of
           [] -> Nothing
           x -> Just (name, x)) *** concat $ unzip $ ck "nextState" a b cnd :
  map (uncurry $ check c d cnd) [(setEights, "set8"),
                                  (aluR, "aluR"),
                                  (aluL, "aluL"),
                                  (aluC, "aluC"),
                                  (fS, "fS"),
                                  (fR, "fR"),
                                  (fM, "fM"),
                                  (setSixteens, "set16"),
                                  (bus16, "b16"),
                                  (iDec, "id"),
                                  (miscFlags, "miscFlags")]

which :: a -> a -> Bool -> a
which a _ True = a
which _ a False = a

flattenmi :: MicroInstruction a -> [a]

flattenmi (MicroInstruction a b c d e f g h i j k) =
  a ++ b ++ c ++ d ++ e ++ f ++ g ++ h ++ i ++ j ++ k

check a b c f s = ck s (f a) (f b) c

ck s x y c = ((s,) <$> msum r, isNothing <$> r)
  where r = zipWith (findIneqWhen c) (x ++ repeat Unknown) y

countTests :: TestCase -> Integer
countTests tc = sum $ which 0 1 . null <$> testNS tc :
  map ($ testMisc tc) [setEights, aluR, aluL, aluC, fS, fR, fM,
                       setSixteens, bus16, iDec, miscFlags]
  
test tc@(TestCase name state inst ifl cnd mi ns bm) =
  (tr, [(sf, st), (sbf, sbt)], countTests tc, bf)
  where (a, c) = correct state inst ifl (ns, mi)
        (b, d) = correct state inst ifl encoded
        cnd' = correction state inst ifl cnd
        (tr, bf) = testA name cnd' a b c d
        bt = foldl' (flip ((+) . ifElse 0 1 . (== Unknown))) 0 $
          a ++ flattenmi c
        sf = countPoss bm $ cnd' &&& ors nonm
        nonm = zipWith nonMatch a b ++ flattenmi (liftA2 nonMatch c d)
        st = countPoss bm cnd'
        sbf = sum $ countPoss bm . (cnd' &&&) <$> nonm
        sbt = bt * st

bmt :: Int
bmt = length $ uncurry (++) $ second flattenmi encoded

trueList :: [Bool]
trueList = replicate bmt True
      
runTest :: String -> IO (Maybe ([(Integer, Integer)], [Bool]))
runTest fname = decodeFileEither fname >>= do
  either ((>> return Nothing) .
          (putStrLn ("While parsing " ++ fname ++ ":") >>) .
          putStr . prettyPrintParseException) $ \tests -> do
    let (trs, ss, tcs, bfs) = unzip4 $ test <$> tests
    let sb = foldl' ((force .) . zipWith (both2 (+))) (replicate 2 (0,0)) ss
    let bf = foldl' (zipWith (&&)) trueList bfs
    let tfs = catMaybes trs
    let ct = toInteger $ length trs
    let tt = sum tcs
    let cf = toInteger $ length tfs
    let tf = sum $ toInteger . length . snd <$> tfs
    unless (null tfs) $ putStrLn $ "In '" ++ fname ++ "':"
    forM_ tfs $ \(caseName, testFails) -> do
      putStrLn $ "\tTest case '" ++ caseName ++ "':"
      forM_ testFails $ \(testName, reasons) -> do
        putStrLn $ "\t\tTest '" ++ testName ++ "' fails"
        unless (null reasons) $ do
          putStr "\t\t\t(when all of '"
          let rTrue = fst <$> filter snd reasons
          let rFalse = fst <$> filter (not . snd) reasons
          unless (null rTrue) $ do
            putStr $ rTrue ++ "' are true"
            unless (null rFalse) $ putStr " and all of '"
          unless (null rFalse) $ putStr $ rFalse ++ "' are false"
          putStrLn ")"
    unless (null tfs) $ putStrLn ""
    let bs = length $ filter id bf
    let [(sf, st), (gf, gt)] = sb
    putStrLn $ "\nTotals for '" ++ fname ++ "':"
    printTotals ct cf tt tf bs bmt st sf gt gf
    return $ Just ((cf, ct):(tf,tt):sb, bf)

printTotals ct cf tt tf bs bmt st sf gt gf = do
  putStrLn $ " Test cases passed: " ++ show (ct - cf) ++ '/' : show ct
  putStrLn $ " Tests passed: " ++ show (tt - tf) ++ '/' : show tt
  putStrLn $ " Bits passed: " ++ show bs ++ '/' : show bmt
  putStrLn $ " Scenarios passed: " ++ show (st - sf) ++ '/' : show st
  putStrLn $ " Total non-failures: " ++ show (gt - gf) ++ '/' : show gt


both2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
both2 = (uncurry (***) .) . join (***)

-- cases, tests, bits in error, scenarios in error, scenario-bits

main :: IO ()
main = do
  testResults <- mapM runTest =<< getArgs
  let readables = catMaybes testResults
  let ([(cf, ct), (tf, tt), (sf, st), (gf, gt)], br) = foldl'
        (force . uncurry (***) . (zipWith (both2 (+)) *** zipWith (&&)))
        (replicate 4 (0,0), trueList) readables
  let bs = length $ filter id br
  let readCount = length readables
  let totCount = length testResults
  when (readCount /= totCount) $ putStrLn $
    show (totCount - readCount) ++ " file" ++
    (if readCount == totCount - 1 then "" else "s") ++
    " unreadable (" ++ show totCount ++ " total)"
  when (totCot > 1 && readCount /= 0) $ do
    putStrLn "Totals:"
    printTotals ct cf tt tf bs bmt st sf gt gf
