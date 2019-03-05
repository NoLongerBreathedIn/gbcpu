module GB.GameBoy where
data GBType = DMG | CGB | GBA

-- DMG: on start up, A = 1, B = 0
-- CGB: on start up, A = 17, B is even
-- GBA: on start up, A = 17, B is odd

-- IO register stuck bits (on, off)
-- IF: E0, FF
-- TAC: F8, FF
-- STAT: 80, FF
-- DMGEmuInd: FE, FF (FF, FF if DMG)
-- TIMA: returns 0 while reloading
-- HDMA1-4: FF, FF
-- HDMA5: high bit is 1 iff copy complete (FF, FF if DMG)
-- SVBK: F8, FF (FF, FF if DMG)
-- VBK: FE, FF (FF, FF if DMG)
-- BGPI, OBPI: 40, FF (FF, FF if DMG)
-- BGPD, OBPD: 00, FF (FF, FF if DMG)
-- KEY1: 7E, 7F if normal speed; FE, FF if double speed; FF, FF if DMG
-- RP: 3C, FF
-- UNK2/3: FF, FF on DMG; else 00, FF
-- UNK4: FF, FF if DMG; else 00, FF
-- UNK5: FF, FF on DMG; else 8F, FF
