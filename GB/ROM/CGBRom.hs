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
  ld16	RegSP	0xFFFE		-- 00:	: ld  sp,$fffe
  ld8	RegA	0x02		-- 03:	: ld  a,$02
  jp	systemSetup		-- 05:	: jp  systemSetup
  hdma1 <- label		-- hdma1:
  bytes [0xD3, 0x00, 0x98, 0xA0,
         0x12]			-- 08: D3 00 98 A0 12
  hdma2 <- label		-- hdma2:
  bytes [0xD3, 0x00, 0x80, 0x00,
         0x40]			-- 0D: D3 00 80 00 40
  bytes [0x1E, 0x53, 0xD0, 0x00,-- 12: 1E 53 D0 00
         0x1F, 0x42, 0x1C, 0x00,-- 16: 1F 42 1C 00
         0x14, 0x2A, 0x4D, 0x19,-- 1A: 14 2A 4D 19
         0x8C, 0x7E, 0x00, 0x7C,-- 1D: 8C 7E 00 7C
         0x31, 0x6E, 0x4A, 0x45,-- 22: 31 6E 4A 45
         0x52, 0x4A, 0x00, 0x00,-- 26: 52 4A 00 00
         0xFF, 0x53, 0x1F, 0x7C,-- 2A: FF 53 1F 7C
         0xFF, 0x03, 0x1F, 0x00,-- 2D: FF 03 1F 00
         0xFF, 0x1F, 0xA7, 0x00,-- 32: FF 1F A7 00
         0xEF, 0x1B, 0x1F, 0x00,-- 36: EF 1B 1F 00
         0xEF, 0x1B, 0x00, 0x7C,-- 3A: EF 1B 00 7C
         0x00, 0x00, 0xFF, 0x03]-- 3D: 00 00 FF 03
  romLogo <- label		-- romLogo:
  bytes [0xCE, 0xED, 0x66, 0x66,-- 42: CE ED 66 66
         0xCC, 0x0D, 0x00, 0x0B,-- 46: CC 0D 00 0B
         0x03, 0x73, 0x00, 0x83,-- 4A: 03 73 00 83
         0x00, 0x0C, 0x00, 0x0D,-- 4D: 00 0C 00 0D
         0x00, 0x08, 0x11, 0x1F,-- 52: 00 08 11 1F
         0x88, 0x89, 0x00, 0x0E,-- 56: 88 89 00 0E
         0xDC, 0xCC, 0x6E, 0xE6,-- 5A: DC CC 6E E6
         0xDD, 0xDD, 0xD9, 0x99,-- 5D: DD DD D9 99
         0xBB, 0xBB, 0x67, 0x63,-- 62: BB BB 67 63
         0x6E, 0x0E, 0xEC, 0xCC,-- 66: 6E 0E EC CC
         0xDD, 0xDC, 0x99, 0x9F,-- 6A: DD DC 99 9F
         0xBB, 0xB9, 0x33, 0x3E]-- 6D: BB B9 33 3E
  registered <- label		-- registered:
  bytes [0x3C, 0x42, 0xB9, 0xA5,-- 72: 3C 42 B9 A5
         0xB9, 0xA5, 0x42, 0x3C]-- 76: B9 A5 42 3C
  byte 0x58			-- 7A: 58
  byte 0x43			-- 7B: 43
  systemSetup <- label		-- systemSetup:
  sth $ Loc8 regSVBK		-- 7C:	: ldh (SVBK),a
  ld8	RegA	0xFC		-- 7E:	: ld  a,$fc
  sth $ Loc8 regBGP		-- 80:	: ldh (BGP),a
  call	setupSound		-- 82:	: call setupSound
  call	clear8kb8000		-- 85:	: call clear8kb8000
  ld8	RegH	0xD0		-- 88:	: ld  h,$D0
  call	clear8kbHL		-- 8A:	: call clear8kbHL
  ld16	RegHL	0xFE00		-- 8D:	: ld  hl,$fe00
  ld8	RegC	0xA0		-- 90:	: ld  c,$a0
  xur	RegA			-- 92:	: xor a
  clearOAM <- label		-- clearOAM:
  sta	LocHLI			-- 93:	: ld  (hl+),a
  dec	RegC			-- 94:	: dec c
  jrlc	CondNZ	clearOAM	-- 95:	: jr  nz,clearOAM
  ld16	RegDE	0x0104		-- 97:	: ld  de,$0104
  ld16	RegHL	0x8010		-- 9A:	: ld  hl,$8010
  ld	RegC	RegH		-- 9D:	: ld  c,h
  copyLogo <- label		-- copyLogo:
  lda	LocDE			-- 9E:	: ld  a,(de)
  sth	LocC			-- 9F:	: ldh (c),a
  inc	RegC			-- A0:	: inc c
  call	vramConvA		-- A1:	: call vramConvA
  call  vramConvB		-- A4:	: call vramConvB
  inc16	RegDE			-- A7:	: inc de
  ld	RegA	RegE		-- A8:	: ld  a,e
  cpi	0x34			-- A9:	: cp  $34
  jrlc	CondNZ	copyLogo	-- AB:	: jr  nz,copyLogo
  ld16	RegDE	0x0072		-- AD:	: ld  de,$0072
  ld8	RegB	0x08		-- B0:	: ld  b,$08
  copyRTile <- label		-- copyRTile:
  lda	LocDE			-- B2:	: ld  a,(de)
  inc16	RegDE			-- B3:	: inc de
  sta	LocHLI			-- B4:	: ld  (hl+),a
  inc16	RegHL			-- B5:	: inc hl
  dec	RegB			-- B6:	: dec b
  jrlc	CondNZ	copyRTile	-- B7:	: jr  nz,copyRTile
  call	loadGBCheckHeader	-- B9:	: call loadGBCheckHeader
  ld8	RegA	0x01		-- BC:	: ld  a,$01
  sth $ Loc8 regVBK		-- BE:	: ldh (VBK),a
  ld8	RegA	0x91		-- C0:	: ld  a,$91
  sth $ Loc8 regLCDC		-- C2:	: ldh (LCDC),a
  ld16	RegHL	0x98B2		-- C4:	: ld  hl,$98B2
  ld16	RegBC	0x4E44		-- C7:	: ld  bc,$4E44
  call	shininessMaybe		-- CA:	: call shininessMaybe
  xur	RegA			-- CD:	: xor a
  sth $ Loc8 regVBK		-- CE:	: ldh (VBK),a
  ld16	RegBC	0x1880		-- D0:	: ld  bc,$1880
  ld16	RegHL	0x0042		-- D3:	: ld  hl,$0042
  checkHead <- label		-- checkHead:
  ldh	LocC			-- D6:	: ldh a,(c)
  inc	RegC			-- D7:	: inc c
  cp	LocHL			-- D8:	: cp  (hl)
  jrc	CondNZ	lockup		-- D9:	: jr  nz,@
  inc16	RegHL			-- DB:	: inc hl
  dec	RegB			-- DC:	: dec b
  jrlc	CondNZ	checkHead	-- DD:	: jr  nz,checkHead
  ld16	RegHL	0x0134		-- DF:	: ld  hl,$0134
  ld8	RegB	0x19		-- E2:	: ld  b,$19
  ld	RegA	RegB		-- E4:	: ld  a,b
  checkSum <- label		-- checkSum:
  add	LocHL			-- E5:	: add (hl)
  inc	RegL			-- E6:	: inc l
  dec	RegB			-- E7:	: dec b
  jrlc	CondNZ checkSum		-- E8:	: jr  nz,checkSum
  add	LocHL			-- EA:	: add (hl)
  jrc	CondNZ lockup		-- EB:	: jr  nz,@
  call	loadVRamPalettes	-- ED:  : call loadVRamPalettes
  call	setSysMode		-- F0:	: call setSysMode
  xur	RegA			-- F3:	: xor a
  sth $ Loc8 regSVBK		-- F4:	: ldh (SVBK),a
  ld8	RegA	0x11		-- F6:	: ld  a,$11
  let ros = if fakeGBA then stb else rsb
  ros	0	RegB		-- F8:	: res 0,b (CGB)
  				-- F8:  : set 0,b (GBA)
  jrl	disboot			-- FA:	: jr  disboot
  nop				-- FB:	: nop
  nop				-- FC:	: nop
  disboot <- label		-- disboot:
  sth $ Loc8 regBIOS		-- FE:	: ldh (BIOS),a
  copyRegion 0 0x100		-- then repeat all that for space reasons
  -- To be continued.
