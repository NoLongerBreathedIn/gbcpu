{-# LANGUAGE RecursiveDo #-}
module GB.ROM.DMGRom (dmgROM) where

import Data.Array.Unboxed
import Data.Word
import GB.ROM.RomMake

dmgROM :: UArray Word16 Word8

dmgROM = makeROM 0 255 $ mdo
  let cartLogo = 0x0104		-- start of cart logo
  let vramLogo = 0x8010		-- start of logo spot in vram
  let lockup = 0xFE		-- jr lockup is worse than halt.
  ld16	RegSF	0xFFFE		-- 00: 31 FE FF	: ld  sp,$fffe
  xur	RegA			-- 03: AF	: xor a
  ld16	RegHL	0x9FFF		-- 04: 21 FF 9F	: ld  hl,$9fff
  zeroVRAM <- label		-- zeroVRAM:
  sta	LocHLD			-- 07: 32	: ld  (hl-),a
  bat	7	RegH		-- 08: CB 7C	: bit 7,h
  jrlc	CondNZ	zeroVRAM	-- 0A: 20 FB	: jr  NZ,zeroVRAM
  ld16	RegHL $ lReg regAENA	-- 0C: 21 26 FF	: ld  hl,$ff00+AUDENA
  ld8	RegC	regA1LEN	-- 0F: 0E 11	: ld  c,AUD1LEN
  ld8	RegA	0x80		-- 11: 3E 80	: ld  a,$80
  sta	LocHLD			-- 13: 32	: ld  (hl-),a ;hl is now AUDOUT
  sth	LocC			-- 14: E2	: ldh (c),a
  inc	RegC			-- 15: 0C	: inc c ;now AUD1ENV
  ld8	RegA	0xF3		-- 16: 3E F3	: ld  a,$f3
  sth	LocC			-- 18: E2	: ldh (c),a
  sta	LocHLD			-- 19: 32	: ld  (hl-),a
  ld8	RegA	0x77		-- 1A: 3E 77	: ld  a,$77
  ld	LocHL	RegA		-- 1C: 77	: ld  (hl),a
  ld8	RegA	0xFC		-- 1D: 3E FC	: ld  a,$fc
  sth $ Loc8 regBGP		-- 1F: E0 47	: ldh (BGP),a
  ld16	RegDE	cartLogo	-- 21: 11 04 01	: ld  de,cartLogo
  ld16	RegHL	vramLogo	-- 24: 21 10 80	: ld  hl,vramLogo
  vramConv <- label		-- vramConv:
  lda	LocDE			-- 27: 1A	: ld  a,(de)
  call  storeVRamA		-- 28: CD 95 00	: call storeVRamA
  call  storeVRamB		-- 2B: CD 96 00	: call storeVRamB
  inc16	RegDE			-- 2E: 13	: inc de
  ld	RegA	RegE		-- 2F: 7B	: ld  a,e
  cpi	0x34			-- 30: FE 34	: cp  $34
  jrlc	CondNZ	vramConv	-- 32: 20 F3	: jr  nz,vramConv
  ld16	RegDE	registered	-- 34: 11 D8 00	: ld  de,registered
  ld8   RegB	0x08		-- 37: 06 08	: ld  b,$8
  vramConvB <- label		-- vramConvB:
  lda	LocDE			-- 39: 1A	: ld  a,(de)
  inc16	RegDE			-- 3A: 13	: inc de
  sta	LocHLI			-- 3B: 22	: ld  (hl+),a
  inc16 RegHL			-- 3C: 23	: inc hl
  dec   RegB			-- 3D: 05	: dec b
  jrlc	CondNZ	vramConvB	-- 3E: 20 F9	: jr  nz,vramConvB
  ld8	RegA	0x19		-- 40: 3E 19	: ld  a,$19
  sta16	0x9910			-- 42: EA 10 99	: ld  ($9910),a
  ld16	RegHL	0x992C		-- 45: 21 2F 99	: ld  hl,$992f
  tileA <- label		-- tileA:
  ld8	RegC	0x0C		-- 48: 0E 0C	: ld  c,$0c
  tileB <- label		-- tileB:
  dec	RegA			-- 4A: 3D	: dec a
  jrlc	CondZ	scrollInit	-- 4B: 28 08	: jr  z,scrollInit
  sta	LocHLD			-- 4D: 32	: ld  (hl-),a
  dec	RegC			-- 4E: 0D	: dec c
  jrlc	CondNZ	tileB		-- 4F: 20 F9	: jr  nz,tileB
  ld8	RegL	0x0F		-- 51: 2E 0F	: ld  l,$0f
  jrl	tileA			-- 53: 18 F3	: jr  tileA
  scrollInit <- label		-- scrollInit:
  ld	RegH	RegA		-- 55: 67	: ld  h,a
  ld8	RegA	0x64		-- 56: 3E 64	: ld  a,$64
  ld	RegD	RegA		-- 58: 57	: ld  d,a
  sth $ Loc8 regSCY		-- 59: E0 42	: ldh (SCY),a
  ld8	RegA	0x91		-- 5B: 3E 91	: ld  a,$91
  sth $ Loc8 regLCDC		-- 5D: E0 40	: ldh (LCDC),a
  inc	RegB			-- 5F: 04	: inc b
  waitFrame <- label		-- waitFrame:
  ld8	RegE	0x02		-- 60: 1E 02	: ld  e,$02
  waitFrameA <- label		-- waitFrameA:
  ld8	RegC	0x0C		-- 62: 0E 0C	: ld  c,$0c
  waitFrameB <- label		-- waitFrameB:
  ldh $ Loc8 regLY		-- 64: F0 44	: ldh a,(LY)
  cpi	0x90			-- 66: FE 90	: cp  $90
  jrlc	CondNZ	waitFrameB	-- 68: 20 FA	: jr  nz,waitFrameB
  dec	RegC			-- 6A: 0D	: dec c
  jrlc	CondNZ	waitFrameB	-- 6B: 20 F7	: jr  nz,waitFrameB
  dec	RegE			-- 6D: 1D	: dec e
  jrlc	CondNZ	waitFrameA	-- 6E: 20 F2	: jr  nz,waitFrameA
  ld8	RegC	regA1LOW	-- 70: 0E 13	: ld  c,AUD1LOW
  inc	RegH			-- 72: 24	: inc h
  ld	RegA	RegH		-- 73: 7C	: ld  a,h
  ld8	RegE	0x83		-- 74: 1E 83	: ld  e,$83
  cpi	0x62			-- 76: FE 62	: cp  $62
  jrlc	CondZ	sound		-- 78: 28 06	: jr  z,sound
  ld8	RegE	0xC1		-- 7A: 1E C1	: ld  e,$c1
  cpi	0x64			-- 7C: FE 64	: cp  $64
  jrlc	CondNZ	scroll		-- 7E: 20 08	: jr  nz,scroll
  sound <- label		-- sound:
  ld	RegA	RegE		-- 80: 7B	: ld  a,e
  sth	LocC			-- 81: E2	: ldh (c),a
  inc	RegC			-- 82: 0C	: inc c ;now AUD1HIGH
  ld8	RegA	0x87		-- 83: 3E 87	: ld  a,$87
  sth	LocC			-- 85: E2	: ldh (c),a
  scroll <- label		-- scroll:
  ldh $ Loc8 regSCY		-- 86: F0 42	: ldh a,(SCY)
  sub	RegB			-- 88: 90	: sub b
  sth $ Loc8 regSCY		-- 89: E0 42	: ldh (SCY),a
  dec	RegD			-- 8B: 15	: dec d
  jrlc	CondNZ	waitFrame	-- 8C: 20 D2	: jr  nz,waitFrame
  dec	RegB			-- 8E: 05	: dec b
  jrlc	CondNZ	logoCheck	-- 8F: 20 4F	: jr  nz,logoCheck
  ld8	RegD	0x20		-- 91: 16 20	: ld  d,$20
  jrl	waitFrame		-- 93: 18 CB	: jr  waitFrame
  storeVRamA <- label		-- storeVRamA:
  ld	RegC	RegA		-- 95: 4F	: ld  c,a
  storeVRamB <- label		-- storeVRamB:
  ld8	RegB	0x04		-- 96: 06 04	: ld  b,$04
  storeVRamL <- label		-- storeVRamL:
  push	RegBC			-- 98: C5	: push bc
  rl	RegC			-- 99: CB 11	: rl  c
  rla				-- 9B: 17	: rla
  pop	RegBC			-- 9C: C1	: pop bc
  rl	RegC			-- 9D: CB 11	: rl  c
  rla				-- 9F: 17	: rla
  dec	RegB			-- A0: 05	: dec b
  jrlc	CondNZ	storeVRamL	-- A1: 20 F5	: jr  nz,storeVRamL
  sta	LocHLI			-- A3: 22	: ld  (hl+),a
  inc16	RegHL			-- A4: 23	: inc hl
  sta	LocHLI			-- A5: 22	: ld  (hl+),a
  inc16 RegHL			-- A6: 23	: inc hl
  ret				-- A7: C9	: ret
  romLogo <- label		-- romLogo:
  bytes [0xCE, 0xED, 0x66, 0x66,-- A8: CE ED 66 66
         0xCC, 0x0D, 0x00, 0x0B,-- AC: CC 0D 00 0B
         0x03, 0x73, 0x00, 0x83,-- B0: 03 73 00 83
         0x00, 0x0C, 0x00, 0x0D,-- B4: 00 0C 00 0D
         0x00, 0x08, 0x11, 0x1F,-- B8: 00 08 11 1F
         0x88, 0x89, 0x00, 0x0E,-- BC: 88 89 00 0E
         0xDC, 0xCC, 0x6E, 0xE6,-- C0: DC CC 6E E6
         0xDD, 0xDD, 0xD9, 0x99,-- C4: DD DD D9 99
         0xBB, 0xBB, 0x67, 0x63,-- C8: BB BB 67 63
         0x6E, 0x0E, 0xEC, 0xCC,-- CC: 6E 0E EC CC
         0xDD, 0xDC, 0x99, 0x9F,-- D0: DD DC 99 9F
         0xBB, 0xB9, 0x33, 0x3E]-- D4: BB B9 33 3E
  registered <- label		-- registered:
  bytes [0x3C, 0x42, 0xB9, 0xA5,-- D8: 3C 42 B9 A5
         0xB9, 0xA5, 0x42, 0x3C]-- DC: B9 A5 42 3C
  logoCheck <- label		-- logoCheck:
  ld16	RegHL	cartLogo	-- E0: 21 04 01	: ld  hl,cartLogo
  ld16  RegDE	romLogo		-- E3: 11 A8 00	: ld  de,romLogo
  checkLoop <- label		-- checkLoop:
  lda	LocDE			-- E6: 1A	: ld  a,(de)
  inc16	RegDE			-- E7: 13	: inc de
  cp	LocHL			-- E8: BE	: cp  (hl)
  jrc	CondNZ	lockup		-- E9: 20 FE	: jr  nz,$fe
  inc16	RegHL			-- EB: 23	: inc hl
  ld	RegA	RegL		-- EC: 7D	: ld  a,l
  cpi	0x34			-- ED: FE 34	: cp  $34
  jrlc	CondNZ	checkLoop	-- EF: 20 F5	: jr  nz,checkLoop
  ld8	RegB	0x19		-- F1: 06 19	: ld  b,19
  ld	RegA	RegB		-- F3: 78	: ld  a,b
  checkB <- label		-- checkB:
  add	LocHL			-- F4: 86	: add (hl)
  inc16	RegHL			-- F5: 23	: inc hl
  dec	RegB			-- F6: 05	: dec b
  jrlc	CondNZ	checkB		-- F7: 20 FB	: jr  nz,checkB
  add	LocHL			-- F9: 86	: add (hl)
  jrc	CondNZ	lockup		-- FA: 20 FE	: jr  nz,$fe
  ld8	RegA	0x01		-- FC: 3E 01	: ld  a,$01
  sth $ Loc8 regBIOS		-- FE: E0 50	: ldh (BIOS),a
