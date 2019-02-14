module GB.Memory.IO where

import Data.Word
import GB.Util.Base
import GB.Memory.HMem
import Control.Arrow ((***))
import GB.Lava.Signal
import Data.Array

ensureLength :: Int -> [a] -> [a]
ensureLength x = elems . listArray (1,x)

joypad :: [Signal] -> HMemSpec

joypad_mix :: [Signal] -> [Signal] -> [Signal]

-- down up left right start select b a
joypad_mix [_, e5, e4] =
  uncurry (zipWith (&-&)) . (fmap (e4 |-|) *** fmap (e5 |-|)) . splitAt 4

joypad buttons_out =
  (0, \_ _ co ci w _ wdata ->
      let es = register co ci w (take 2 $ drop 2 wdata)
      in (replicate 2 high ++ es ++ buttons_out,
          fallingEdge (ands buttons_out) : es))

serial :: Signal -> Signal -> [HMemSpec]

serialRegister :: [Signal] -> Signal -> Signal -> Signal -> Signal ->
                  [Signal] -> ([Signal], [Signal])
serialControl :: Signal -> Signal -> Signal -> Signal -> Signal -> Signal ->
                 [Signal] -> ([Signal], [Signal])
serial si sc = [(1, \dat _ co ci w _ wd ->
                    serialRegister (dat ! 2) si co ci w wd),
                (2, \_ cgb co ci w z wd ->
                    serialControl cgb sc co ci w z wd)]

serialControl cgb sc co ci w z wd =
  (transfer : replicate 5 high ++ [cgb !|| cksp, ckt],
    [transfin, tck, transfer, tcko, tcki]) where
  ciw = ci &-& w
  [transfer] = registerAWz co ciw (neg transfin) [head wd]
  [ckt] = registerAW co ciw $ drop 7 wd
  [cksp] = registerAWz co ciw cgb [wd !! 6]
  tcko = fallingEdge tck
  tcki = risingEdge tck
  acks = ensureLength 7 $ registerAW co ci $ zipWith (^-^) acks $
    drop 1 acks ++ [high]
  tck = transfer !|| mux2 ckt sc (mux2 cksp (head acks) (acks !! 5))
  counter = registerz tcko tcki transfer z $
            ((counter !! 2) ^-^ ((counter !! 1) !|| head counter)) :
            take 2 counter
  transfin = delay $ transfer &-& nands counter

serialRegister [_, _, transfer, tcko, tcki] si co ci w wd =
  (regOut, [head regOut]) where
  [cko, cki] = zipWith (mux2 transfer) [co, ci &-& w] [tcko, tcki]
  regOut = ensureLength 8 $ registerAW cko cki $ zipWith (mux2 transfer) wd $
           tail regOut ++ [si]
