{-# LANGUAGE RecursiveDo #-}
module GB.ROM.CGBRom (cgbROM, gbaROM) where

import Data.Array.Unboxed
import Data.Word
import GB.ROM.RomMake

cgbROM :: UArray Word16 Word8
gbaROM :: UArray Word16 Word8

cgbaROM :: Bool -> UArray Word16 Word8

cgbROM = cgbaROM False
gbaROM = cgbaROM True

cgbaROM fakeGBA = makeROM $ mdo
  let lockup = 0xFE
  ld16	RegSP	0xFFFE		-- 00: 31 FE FF	: ld  sp,$fffe
  ld8	RegA	0x02		-- 03: 3E 02	: ld  a,$02
  jrl	systemSetup		-- 05: 18 74	: jr  systemSetup
  hdma1 <- label		-- hdma1:
  bytes [0xD3, 0x00, 0x98, 0xA0,
         0x12]			-- 07: D3 00 98 A0 12
  hdma2 <- label		-- hdma2:
  bytes [0xD3, 0x00, 0x80, 0x00,
         0x40]			-- 0C: D3 00 80 00 40
  bytes [0x1E, 0x53, 0xD0, 0x00,-- 11: 1E 53 D0 00
         0x1F, 0x42, 0x1C, 0x00,-- 15: 1F 42 1C 00
         0x14, 0x2A, 0x4D, 0x19,-- 19: 14 2A 4D 19
         0x8C, 0x7E, 0x00, 0x7C,-- 1C: 8C 7E 00 7C
         0x31, 0x6E, 0x4A, 0x45,-- 21: 31 6E 4A 45
         0x52, 0x4A, 0x00, 0x00,-- 25: 52 4A 00 00
         0xFF, 0x53, 0x1F, 0x7C,-- 29: FF 53 1F 7C
         0xFF, 0x03, 0x1F, 0x00,-- 2C: FF 03 1F 00
         0xFF, 0x1F, 0xA7, 0x00,-- 31: FF 1F A7 00
         0xEF, 0x1B, 0x1F, 0x00,-- 35: EF 1B 1F 00
         0xEF, 0x1B, 0x00, 0x7C,-- 39: EF 1B 00 7C
         0x00, 0x00, 0xFF, 0x03]-- 3C: 00 00 FF 03
  romLogo <- label		-- romLogo:
  bytes [0xCE, 0xED, 0x66, 0x66,-- 41: CE ED 66 66
         0xCC, 0x0D, 0x00, 0x0B,-- 45: CC 0D 00 0B
         0x03, 0x73, 0x00, 0x83,-- 49: 03 73 00 83
         0x00, 0x0C, 0x00, 0x0D,-- 4C: 00 0C 00 0D
         0x00, 0x08, 0x11, 0x1F,-- 51: 00 08 11 1F
         0x88, 0x89, 0x00, 0x0E,-- 55: 88 89 00 0E
         0xDC, 0xCC, 0x6E, 0xE6,-- 59: DC CC 6E E6
         0xDD, 0xDD, 0xD9, 0x99,-- 5C: DD DD D9 99
         0xBB, 0xBB, 0x67, 0x63,-- 61: BB BB 67 63
         0x6E, 0x0E, 0xEC, 0xCC,-- 65: 6E 0E EC CC
         0xDD, 0xDC, 0x99, 0x9F,-- 69: DD DC 99 9F
         0xBB, 0xB9, 0x33, 0x3E]-- 6C: BB B9 33 3E
  registered <- label		-- registered:
  bytes [0x3C, 0x42, 0xB9, 0xA5,-- 71: 3C 42 B9 A5
         0xB9, 0xA5, 0x42, 0x3C]-- 75: B9 A5 42 3C
  loc_7A <- label		-- What are these bytes for?
  byte 0x58			-- 79: 58
  byte 0x43			-- 7A: 43
  systemSetup <- label		-- systemSetup:
  sth $ Loc8 regSVBK		-- 7B: E0 70	: ldh (SVBK),a
  ld8	RegA	0xFC		-- 7D: 3E FC	: ld  a,$fc
  sth $ Loc8 regBGP		-- 7F: E0 47	: ldh (BGP),a
  call	setupSound		-- 81: CD ## ##	: call setupSound
  call	clear8kb8000		-- 84: CD ## ##	: call clear8kb8000
  ld8	RegH	0xD0		-- 87: 26 D0	: ld  h,$D0
  call	clear8kbHL		-- 89: CD ## ##	: call clear8kbHL
  ld16	RegHL	0xFE00		-- 8C: 21 00 FE	: ld  hl,$fe00
  ld8	RegC	0xA0		-- 8F: 0E A0	: ld  c,$a0
  xur	RegA			-- 91: AF	: xor a
  clearOAM <- label		-- clearOAM:
  sta	LocHLI			-- 92: 22	: ld  (hl+),a
  dec	RegC			-- 93: 0D	: dec c
  jrlc	CondNZ	clearOAM	-- 94: 20 FC	: jr  nz,clearOAM
  ld16	RegDE	0x0104		-- 96: 11 04 01	: ld  de,$0104
  ld16	RegHL	0x8010		-- 99: 21 10 80	: ld  hl,$8010
  ld	RegC	RegH		-- 9C: 4C	: ld  c,h
  copyLogo <- label		-- copyLogo:
  lda	LocDE			-- 9D: 1A	: ld  a,(de)
  sth	LocC			-- 9E: E2	: ldh (c),a
  inc	RegC			-- 9F: 0C	: inc c
  call	vramConvA		-- A0: CD ## ##	: call vramConvA
  call  vramConvB		-- A3: CD ## ##	: call vramConvB
  inc16	RegDE			-- A6: 13	: inc de
  ld	RegA	RegE		-- A7: 7B	: ld  a,e
  cpi	0x34			-- A8: FE 34	: cp  $34
  jrlc	CondNZ	copyLogo	-- AA: 20 F1	: jr  nz,copyLogo
  ld16	RegDE	0x0072		-- AC: 11 72 00	: ld  de,$0072
  ld8	RegB	0x08		-- AF: 06 08	: ld  b,$08
  copyRTile <- label		-- copyRTile:
  lda	LocDE			-- B1: 1A	: ld  a,(de)
  inc16	RegDE			-- B2: 13	: inc de
  sta	LocHLI			-- B3: 22	: ld  (hl+),a
  inc16	RegHL			-- B4: 23	: inc hl
  dec	RegB			-- B5: 05	: dec b
  jrlc	CondNZ	copyRTile	-- B6: 20 F9	: jr  nz,copyRTile
  call	loadGBCheckHeader	-- B8: CD ## ##	: call loadGBCheckHeader
  ld8	RegA	0x01		-- BB: 3E 01	: ld  a,$01
  sth $ Loc8 regVBK		-- BD: E0 4F	: ldh (VBK),a
  ld8	RegA	0x91		-- BF: 3E 91	: ld  a,$91
  sth $ Loc8 regLCDC		-- C1: E0 40	: ldh (LCDC),a
  ld16	RegHL	0x98B2		-- C3: 21 B2 98	: ld  hl,$98B2
  ld16	RegBC	0x4E44		-- C6: 01 44 4E	: ld  bc,$4E44
  call	shininessMaybe		-- C9: CD ## ##	: call shininessMaybe
  xur	RegA			-- CC: AE	: xor a
  sth $ Loc8 regVBK		-- CD: E0 4F	: ldh (VBK),a
  ld16	RegBC	0x1880		-- CF: 01 80 18	: ld  bc,$1880
  ld16	RegHL	0x0042		-- D2: 21 42 00	: ld  hl,$0042
  checkHead <- label		-- checkHead:
  ldh	LocC			-- D5: F2	: ldh a,(c)
  inc	RegC			-- D6: 0C	: inc c
  cp	LocHL			-- D7: BE	: cp  (hl)
  jrc	CondNZ	lockup		-- D8: 20 FE	: jr  nz,@
  inc16	RegHL			-- DA: 23	: inc hl
  dec	RegB			-- DB: 05	: dec b
  jrlc	CondNZ	checkHead	-- DC: 20 F7	: jr  nz,checkHead
  ld16	RegHL	0x0134		-- DE: 21 34 01	: ld  hl,$0134
  ld8	RegB	0x19		-- E1: 06 19	: ld  b,$19
  ld	RegA	RegB		-- E3: 78	: ld  a,b
  checkSum <- label		-- checkSum:
  add	LocHL			-- E4: 86	: add (hl)
  inc	RegL			-- E5: 2C	: inc l
  dec	RegB			-- E6: 05	: dec b
  jrlc	CondNZ checkSum		-- E7: 20 FB	: jr  nz,checkSum
  add	LocHL			-- E9: 86	: add (hl)
  jrc	CondNZ lockup		-- EA: 20 FE	: jr  nz,@
  call	loadVRamPalettes	-- EC: CD ## ##  : call loadVRamPalettes
  call	setSysMode		-- EF: CD ## ##	: call setSysMode
  xur	RegA			-- F2: AF	: xor a
  sth $ Loc8 regSVBK		-- F3: E0 70	: ldh (SVBK),a
  ld8	RegA	0x11		-- F5: 3E 11	: ld  a,$11
  let ros = if fakeGBA then stb else rsb
  ros	0	RegB		-- F7: CB 80	: res 0,b (CGB)
  				-- F7: CB C0 : set 0,b (GBA)
  jrl	0x00FE			-- F9: 18 03	: jr  disboot
  nop				-- FB: 00	: nop
  nop				-- FC: 00	: nop
  nop				-- FD: 00	: nop
  move	0xFE			-- disboot:
  sth $ Loc8 regBIOS		-- FE: E0 50	: ldh (BIOS),a
  copyRegion 0 0x100		-- then repeat all that for space reasons
  -- To be continued.
