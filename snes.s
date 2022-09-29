; TODO
; rewrite OAM building code to use SNES attributes (might need to edit C code too if it has built-in attribute)
; NMI handler, translate packed 1bpp into two separate VRAM locations, translate palettes, etc.
; support for other SNES buttons, maybe L/R/Select/Start reset?
; multitap support? the NES multitap support is vestigial now...
;
; uses BG3 for NES stuff
; but expand 1BPP tiles automatically into a larger address space
; use colour 0 with HDMA gradient (make a table of 2 SNES colours for each NES background colour? replace "blend" at higher level)
; use BG1 with a tiled texture for some subtle sand grain? maybe just some subtractive blend;
;  of a grit texture in a diagonal wood-grainy wave? could mabe have 2 x 2-bit multiple textures w/palette selectors?


.p816
.a8
.i8

.importzp _i, _k, _k, _l
.importzp _mx, _nx, _ox, _px, _ux, _vx
.importzp _ptr
.importzp ppu_post_mode
.importzp ptr1, ptr2, ptr3, ptr4 ; cc65 temporaries
.importzp tmp1, tmp2, tmp3, tmp4 ; cc65 temporaries

.import nes_apu : abs
.import ppu_2000 : abs
.import ppu_2005x : abs
.import _ppu_send_addr : abs
.import _ppu_send_count : abs
.import _ppu_send : abs
.import _palette : abs
.import _oam : abs

.import sound_update_long : far

.export snes_reset : far
.export snes_init : far
.export snes_nmi : far

.export snes_ppu_load_chr : far
.export snes_ppu_fill : far
.export snes_ppu_fill_att : far
.export snes_ppu_apply : far
.export snes_ppu_post : far

VRAM_CHR_BG3 = $0000
VRAM_NMT_BG3 = $2000
VRAM_NMT_BG1 = $2800
VRAM_CHR_BG1 = $3000
VRAM_CHR_OBJ = $4000

.segment "ZEROPAGE"

snes_post_mode: .res 1

.segment "SNESRAM"

snes_repack:    .res 1024      ; buffer for repacking attributes/CHR
snes_palette:   .res 512       ; buffer for translated palettes
snes_oam:       .res 256       ; buffer for translated oam
hdma_gradient:  .res (2*240)+3 ; Colour 0 gradient

.enum
	POST_OFF    = 1
	POST_NONE   = 2
	POST_UPDATE = 3
	POST_DOUBLE = 4
	; added SNES
	POST_SNES_CHR = 5
	POST_SNES_ATT = 6
.endenum

.segment "SNES"

; ===============
; Main SNES stuff
; ===============

snes_reset:
	.a8
	.i8
	; Already assumed:
	;   sei
	;   clc
	;   xce ; disable 6502 emulation
	;   stz a:$4200 ; disable NMI/IRQ
	;   rep #$30
	;   ldx #$01FF
	;   txs
	;   sep #$30
	; disable decimal
	cld
	; direct page = 0
	rep #$20
	.a16
	lda #0
	tcd
	sep #$20
	.a8
	; initialize registers $2100-2133 (PPU mostly)
	lda #^@reset_val
	pha
	plb
	ldx #<-1
	@loop_2100:
		inx
		ldy a:@reset_2100, X
		bmi :+
			lda z:_i
			sta a:$2100, Y
			bra @loop_2100
		:
			iny
			beq @end_2100
			lda a:@reset_val-$81, Y
			sta z:_i
			bra @loop_2100
		;
	@end_2100:
	; initialize registers $4201-420D
	lda #$FF
	sta a:$4201 ; I/O port
	ldx #2
	:
		stz a:$4200, X ; multiplier, divider, timers, DMA enable, SlowROM
		inx
		cpx #$0E
		bcc :-
	; set default data bank
	lda #$80
	pha
	plb
	rep #$20
	.a16
	; clear OAM with DMA
	stz a:$2012
	ldx #%00001010 ; 2-to-1, no increment
	stx a:$4300
	ldx #$04 ; $2104 OAMDATA
	stx a:$4301
	lda #.loword(@bin_F0)
	sta a:$4302
	ldx #^*
	stx a:$4304
	sty a:$4305
	ldx #>512
	stx a:$4306 ; 512 bytes $F0
	ldx #1
	stx a:$420B
	lda #.loword(@bin_00)
	sta a:$4302
	lda #32
	sta a:$4305 ; 32 bytes $00
	stx a:$420B
	; clear CGRAM with DMA
	ldy #0
	stx a:$2121 ; CGADD = 0
	ldx #$22 ; $2122 CGDATA
	stx a:$4301
	sty a:$4305
	ldx #>512
	stx a:$4306 ; 512 bytes
	ldx #1
	stx a:$420B
	; clear VRAM with DMA
	stz a:2116 ; VMADD
	ldx #%00001001 ; 2-to-2, no increment
	stx a:$4300
	ldx #$18 ; $2118-9 VMDATA
	stx a:$4301
	stz a:$4305 ; 64k
	ldx #1
	stx a:$420B
	; clear RAM with DMA (outside of NES area only)
	lda #$0800
	sta a:$2181
	ldx #$7E
	stx a:$2183 ; WMADD = $7E800
	ldx #%00001000 ; 1-to-1, no increment
	stx a:$4300
	ldx #$80 ; $2180 WMDATA
	stx a:$4301
	lda #$10000-$800
	sta a:$4305 ; 64k - 2k NES
	ldx #1
	stx $420B
	; WMADD = $7F000
	; $4305 = 0 = 64k
	ldx #1
	stx $420B
	; done
	sep #$20
	.a8
	; upload SPC program and return
	jsr spc_reset
	rtl
@reset_val: ;$80 + index = value
	;      $80 $81 $82 $83 $84 $85
	.byte $00,$01,$80,$8F,$30,$E0
@reset_2100: ; SNES book recommended default register values
	.byte $83,$00, $80 ; screen = $8F, switch to $00
	.byte $01,$02,$03,$05,$06 ; sprite settings, graphics mode, mosaic
	.byte $07,$08,$09,$0A,$0B,$0C ; tilemap address
	.byte $0D,$0D,$0E,$0E,$0F,$0F,$10,$10,$11,$11,$12,$12,$13,$13,$14,$14 ; scroll
	.byte $82,$15, $80,$16,$17 ; VRAM increment = $80, VRAM address = $00
	.byte $1A ; mode 7 settings
	.byte $1B, $81,$1B, $80,$1C,$1C,$1D,$1D,$1E, $81,$1E, $80,$1F,$1F,$20,$20 ; mode 7 ABCD = 1001, XY = 00
	.byte $21,$23,$24,$25,$26,$27,$28,$29,$2A,$2B,$2C,$2D,$2E,$2F ; colour / window / mask / logic / TS/SS
	.byte $84,$30, $80,$31, $85,$32 ; color math
	.byte $80,$33 ; screen settings
	.byte $FF ; end
@bin_00:
	.byte $00
@bin_F0:
	.byte $F0

snes_init:
	.a8
	.i8
	; setup PPU addresses
	lda #((>VRAM_NMT_BG1) & $FC) | $00
	sta a:$2107 ; BG1SC nametable, 1-screen
	lda #((>VRAM_NMT_BG3) & $FC) | $01
	sta a:$2109 ; BG3SC nametable, 2-screen horizontal
	lda #(VRAM_CHR_BG1 >> 12)
	sta a:$210B ; BG12NBA
	lda #(VRAM_CHR_BG3 >> 12)
	sta a:$210C ; BG34NBA
	lda #((VRAM_CHR_OBJ >> 13) | $00)
	sta a:$2101 ; OBJSEL 8x8 + 16x16 sprites
	; window to remove the left 8 pixels
	lda #%00100010
	sta a:$2123 ; W12SEL enable window 1 for all BG
	sta a:$2124 ; W23SEL
	lda #%00000010
	sta a:$2125 ; WOBJSEL enable window 1 for OBJ
	lda #0
	sta a:$2126 ; WH0 window 1 left
	lda #7
	sta a:$2127 ; WH1 window 1 right
	stz a:$212A ; WBGLOG OR logic
	stz a:$212B ; WOBJLOG OR logic
	lda #%00011111
	sta a:$212E ; TMW apply windows to all layers
	sta a:$212F ; TSW
	; subtractive colour math for BG1
	lda #%00000010
	sta a:$2130 ; CGWSEL subscreen
	lda #%10000100
	; TODO enable
	sta a:$2131 ; CGADSUB subtract from BG3 only
	; global screen settings
	lda #$04
	sta a:$2133 ; SETINI overscan 239 lines mode
	lda #%00010100
	sta a:$212C ; TM OBJ + BG3
	lda #%00000001
	sta a:$212D ; TS BG1
	lda #$01
	sta a:$2105 ; BGMODE mode 1
	stz a:$2115 ; VMAIN default increment on $2118
	; begin
	lda #$81
	sta a:$4200 ; NMITIMEN turn on NMI
	rtl

snes_nmi:
	.a8
	.i8
	lda z:snes_post_mode
	stz z:snes_post_mode
	stz z:ppu_post_mode
	bne :+
		jmp @end
	:
	cmp #POST_NONE
	bne :+
		jmp @post_on
	:
	cmp #POST_UPDATE
	beq @post_update
	cmp #POST_DOUBLE
	beq @post_double
	; otherwise POST_OFF
@post_off:
	lda #$8F
	sta a:$2100 ; INIDISP screen off
	jmp @end
@post_update: ; 32 linear bytes
	jsr @post_common
	; TODO
	stz a:_ppu_send_count
	jmp @post_on
@post_double: ; 64 linear bytes
	jsr @post_common
	; TODO
	; TODO NES NMI stuff, see below
	; types of data:
	; - nametable (1 or 2 rows)
	; - vertical nametable ?
	; - attribute (64 bytes)
	; - CHR 64 bytes?
	stz a:_ppu_send_count
	jmp @post_on
@post_common:
	rep #$20
	.a16
	; OAM DMA
	stz a:$2102 ; OAMADD
	ldx #%00000010 ; 1-to-2
	stx a:$4300
	ldx #$04 ; $2104 OAMDATA
	stx a:$4301
	lda #.loword(snes_oam)
	sta a:$4302
	ldx #^snes_oam
	stx a:$4304
	lda #256
	sta a:$4305
	ldx #1
	stx a:$420B ; DMA
	; palette DMAs
	; common setup
	ldx #$22 ; $2122 CGDATA
	stx a:$4301
	ldx #^snes_palette
	stx a:$4304
	stz a:$4305
	; BG3 palettes + BG1
	ldx #0
	stx a:$2121 ; CGADD = 0
	lda #.loword(snes_palette)
	sta a:$4302
	ldx #32*2
	stx a:$4305 ; 32 colours
	ldx #1
	stx a:$420B
	; OBJ palettes
	ldx #128
	stx a:$2121 ; CGADD = 128
	lda #.loword(snes_palette+(128*2))
	sta a:$4302
	ldx #64*2
	stx a:$4305 ; 64 colours
	ldx #1
	stx a:$420B
	sep #$20
	.a8
	rts
@post_on:
	; set horizontal scroll
	lda a:ppu_2005x
	sta a:$2111 ; BG3HOFS
	lda a:ppu_2000
	and #1
	sta a:$2111
	lda a:ppu_2005x
	sta a:$210D ; BG1HOFS
	lda a:ppu_2000
	and #1
	sta a:$210D
	; TODO set up HDMA?
	lda #$0F
	sta a:$2100 ; INIDISP screen on
	stz z:ppu_post_mode
@end:
	jsl sound_update_long ; updates nes_apu
	jsr spc_update
	rtl

; TODO delete this as we replace it
.if 0 ; NES nmi
	pha
	txa
	pha
	tya
	pha
	ldx #0
	lda ppu_post_mode
	stx ppu_post_mode ; signal the post is complete (after RTI)
	jeq @end
	cmp #POST_NONE
	jeq @post_none
	cmp #POST_UPDATE
	beq @post_update
	cmp #POST_DOUBLE
	beq @post_double
	; otherwise POST_OFF
@post_off:
	lda ppu_2001
	and #%11100001
	sta $2001
	jmp @end
@post_update:
	jsr @post_common
	lda ppu_2000
	sta $2000 ; set direction
	ldx #0
	cpx _ppu_send_count
	bcs :++
	:
		lda _ppu_send, X
		sta $2007
		inx
		cpx _ppu_send_count
		bcc :-
	:
	ldx #0
	stx _ppu_send_count
	jmp @post_on
@post_double:
	jsr @post_common ; direction remains horizontal
	ldx #0
	stx _ppu_send_count
	:
		lda _ppu_send, X
		sta $2007
		inx
		cpx #32
		bcc :-
	lda _ppu_send_addr+1
	eor #$04 ; flip horizontal nametable
	sta $2006
	lda _ppu_send_addr+0
	sta $2006
	:
		lda _ppu_send, X
		sta $2007
		inx
		cpx #64
		bcc :-
	jmp @post_on
@post_common:
	; OAM
	lda #0
	sta $2003
	lda #>_oam
	sta $4014
	; palettes
	lda #0
	sta $2000 ; set horizontal increment
	bit $2002
	ldx #>$3F00
	stx $2006
	;lda #<$3F00
	sta $2006
	ldx #0
	:
		lda _palette, X
		sta $2007
		inx
		cpx #32
		bcc :-
	; prepare address for send
	lda _ppu_send_addr+1
	sta $2006
	lda _ppu_send_addr+0
	sta $2006
	rts
@post_none:
@post_on:
	lda ppu_2000
	sta $2000
	lda ppu_2005x+0
	sta $2005
	lda ppu_2005y+0
	sta $2005
	lda ppu_2001
	sta $2001
@end:
	jsr sound_update
	pla
	tay
	pla
	tax
	pla
	rti
.endif


; ==========================
; SPC program for SNES sound
; ==========================

spc_bin:
	.incbin "spc/spc.bin"
	SPC_BIN_SIZE = * - spc_bin

; resets and uploads the SPC program
spc_reset:
	.a8
	.i8
	; APUIO0 = $FF will reset the SPC code if already active (ignored by BIOS loader)
	lda #$FF
	sta a:$2140
	; now use BIOS loader to load the SPC code
	rep #$10
	.i16
	; wait for warmup signal AABB
	ldy #0
	ldx #$BBAA
:
	cpx a:$2140 ; APUIO0/1
	beq :+
	dey
	bne :-
	bra @finish ; timeout, give up on sound
:
	lda #$01
	sta a:$2141 ; APUIO1 not-zero = wait for command setup
	; set write address to $0200
	lda #<$0200
	sta a:$2142 ; APUIO2
	lda #>$0200
	sta a:$2143 ; APUIO3
	lda #$CC
	sta a:$2140 ; APUIO0 CC for transfer program
:
	cmp a:$2140 ; CC echoed back = ready to go
	bne :-
	ldx #0
@send_byte:
	lda f:spc_bin, X
	sta a:$2141 ; APUIO1 data byte to send
	txa
	sta a:$2140 ; APUIO0 0 = send first byte, otherwise send incrementing value
:
	cmp a:$2140 ; APUIO0 wait for echo of incrementing value
	bne :-
	inx
	cpx #SPC_BIN_SIZE
	bcc @send_byte
	; finish and run
	stz a:$2141 ; APUI01 0 = ready to run
	; set run address to $0200
	lda #<$0200
	sta a:$2142 ; APUIO2
	lda #>$0200
	sta a:$2143 ; APUIO3
	lda a:$2140 ; APUIO0 += 2 to exit data transfer and begin run
	inc
	inc
	sta a:$2140
:
	cmp a:$2140
	bne :-
	; confirm initialization 17,18,19
:
	lda #17
	cmp a:$2141 ; APUIO1
	bne :-
	ldx #(19<<8)|18
	cpx a:$2142 ; APUIO2/3
	bne :-
	; kick off first frame
	lda #$20
	sta a:$2140 ; APUIO0
:
	cmp a:$2140 ; APUIO0
	bne :-
	; now ready to go
@finish:
	sep #$10
	.i8
	rts

; call once per frame to update the SPC's NES APU simulation (copies from nes_apu)
spc_update:
	.a8
	.i8
	ldx #100 ; timeout countdown (in case SPC has crashed, at least we can keep playing)
	lda #17
:
	cmp a:$2141 ; APUIO1
	beq :+
	dex
	bne :-
	rts ; timeout, give up
:
	; copy APU buffer
	ldx #0
	:
		lda f:@apu_list, X
		tay
		lda a:nes_apu, Y
		sta a:$2143 ; APUIO3 value to write
		sty a:$2140 ; APUIO0 register to write
		:
			cpy a:$2140 ; APUIO0 wait for echo
			bne :-
		inx
		cpx #@APU_LIST_COUNT
		bcc :--
	; send end-of-frame
	lda #$20
	sta a:$2140 ; APUIO0
	; don't bother to check for acknowledge
	rts
@apu_list: ; relevant NES APU registers to send each frame
	.byte $00, $02, $03
	.byte $04, $06, $07
	.byte $08, $0A, $0B
	.byte $0C, $0E, $0F
	.byte $15
@APU_LIST_COUNT = * - @apu_list

; ====================
; NESert Golfing Stuff
; ====================

; NES to SNES palette conversion
snes_pal0: ; gggrrrrr
	.byte $8C,$60,$24,$08,$0D,$0E,$4C,$68,$A4,$E0,$E0,$C0,$C0,$00,$00,$00
	.byte $D6,$41,$E8,$CE,$B5,$B7,$F6,$31,$6C,$C3,$C0,$C0,$A0,$00,$00,$00
	.byte $FF,$A9,$50,$17,$FE,$FF,$1F,$5E,$B9,$EF,$28,$25,$05,$4A,$00,$00
	.byte $FF,$97,$7A,$5C,$5F,$3F,$5F,$7F,$9D,$BA,$B7,$B5,$B5,$F7,$00,$00
snes_pal1: ; 0bbbbbgg
	.byte $31,$4C,$54,$4C,$34,$0C,$00,$00,$00,$00,$00,$10,$30,$00,$00,$00
	.byte $5A,$75,$7C,$7C,$64,$30,$00,$01,$01,$01,$01,$1D,$4D,$00,$00,$00
	.byte $7F,$7E,$7E,$7E,$7D,$69,$2E,$0A,$02,$02,$0F,$37,$67,$25,$00,$00
	.byte $7F,$7F,$7F,$7F,$7F,$7B,$63,$53,$4B,$47,$53,$63,$77,$5E,$00,$00
snes_oam_convert: ; vhp...cc -> vhPP0cc0 (duplicate+negate priority bit, select OBJ page 0)
	.repeat 256, I
		.byte ((I & $E0) | ((I>>1) & $10) | ((I & $03) << 1) | $00) ^ $30
	.endrepeat

snes_ppu_load_chr: ; ptr1=addr, ptr2=count, _ptr=data
	.a8
	.i8
	rep #$30
	.a16
	.i16
	lda z:ptr1
	cmp #$1000
	bcc @bg_chr
@obj_chr: ; relocated, promotes 2bpp-21 to 4bpp-0201
	clc
	adc #VRAM_CHR_OBJ-$1000
	sta a:$2116 ; VMADD
	sep #$20
	.a8
	ldy #0
:
	lda (_ptr), Y
	sta $2118 ; VMDATAL
	iny
	cpy z:ptr2
	bcc :-
	sep #$30
	.a8
	.i8
	rtl
@bg_chr: ; splits 8+8 byte planes across $0000 and $0800
	.a16
	.i16
	lda z:ptr1
	sta a:$2116 ; VMADD
	sep #$20
	.a8
	ldy #0
:
	lda (_ptr), Y
	sta $2118 ; VMDATAL
	iny
	cpy #8
	bcc :-
	rep #$20
	.a16
	lda z:ptr1
	eor #$0800
	sta a:$2116 ; VMADD
	sep #$20
	.a8
:
	lda (_ptr), Y
	sta $2118 ; VMDATAL
	iny
	cpy #16
	bcc :-
	rep #$20
	.a16
	lda z:ptr1
	clc
	adc #8
	sta z:ptr1 ; addr += 8
	lda z:_ptr
	clc
	adc #16
	sta z:_ptr ; ptr += 16
	lda z:ptr2
	sec
	sbc #16
	beq :+
	bcc :+
	sta z:ptr2 ; count -= 16
	bra @bg_chr
:
	sep #$30
	.a8
	.i8
	rtl

snes_ppu_fill: ; ptr1=addr, ptr2=count, tmp1=data
	.a8
	.i8
	rep #$30
	.a16
	.i16
	lda z:ptr1
	cmp #$1000
	bcc @bg_chr
	cmp #$2000
	bcs @nmt
@obj_chr: ; relocated, promotes 2bpp-21 to 4bpp-0201
	;clc
	adc #VRAM_CHR_OBJ-$1000
@nmt: ; not relocated
	sta a:$2116 ; VMADD
	ldx z:ptr2
	sep #$20
	.a8
	lda z:tmp1
:
	sta a:$2118 ; VMDATAL
	dex
	bne :-
	sep #$30
	.a8
	.i8
	rtl
@bg_chr: ; splits 2bpp-12 into 2bpp-01 and 2bpp-02
	.a16
	.i16
	sta a:$2116 ; VMADD
	lsr z:ptr2
	ldx z:ptr2
	sep #$20
	.a8
	lda z:tmp1
:
	sta a:$2118 ; VMDATAL
	dex
	bne :-
	rep #$20
	.a16
	lda z:ptr1
	eor #$0800
	sta a:$2116 ; VMADD
	ldx z:ptr2
	sep #$20
	.a8
	lda z:tmp1
:
	sta a:$2118 ; VMDATAL
	dex
	bne :-
	sep #$30
	.a8
	.i8
	rtl

snes_redirect_att: ; A16=addr in attribute area, returns adjusted address, clobbers ptr3
	.a16
	pha
	and #$FC00 ; preserve nametable
	sta z:ptr3
	pla
	and #$03FF
	sec
	sbc #$03C0 ; attribute address starts at $3C0
	asl
	asl
	asl
	asl
	ora z:ptr3 ; nametable + (attribute address * 16)
	rts
	.a8

snes_repack_att: ; A=count (multiple of 8, range 8-64), _ppu_send=data, clobbers tmp1,tmp2,ptr3,ptr4
	phb
	lsr
	lsr
	lsr
	pha
	sta z:tmp1 ; tmp1 = row count
	lda #^@remap
	pha
	plb
	rep #$10
	.i16
	ldx #0
	txa
	xba ; clear high byte of A (will transfer to Y below)
	stx z:ptr4 ; ptr4 = Y
@row:
	lda #8
	sta z:tmp2 ; tmp2 = tile count
	:
		ldy z:ptr4
		lda a:_ppu_send, Y
		iny
		sty z:ptr4
		pha
		and #3
		tay
		lda a:@remap, Y
		sta a:snes_repack+0, X
		sta a:snes_repack+1, X
		pla
		pha
		and #(3<<2)
		tay
		lda a:@remap, Y
		sta a:snes_repack+2, X
		sta a:snes_repack+3, X
		pla
		pha
		and #(3<<4)
		tay
		lda a:@remap, Y
		sta a:snes_repack+64, X
		sta a:snes_repack+65, X
		pla
		and #(3<<6)
		tay
		lda a:@remap, Y
		sta a:snes_repack+66, X
		sta a:snes_repack+67, X
		inx
		inx
		inx
		inx
		dec z:tmp2 ; next tile
		bne :-
	rep #$20
	.a16
	txa
	clc
	adc #96 ; skip 3 rows
	tax
	lda #0 ; clear high byte again
	sep #$20
	.a8
	dec z:tmp1 ; next row
	bne @row
	; double rows
	pla
	asl
	sta z:ptr3+0 ; ptr3 = doubled row count (16-bit)
	stz z:ptr3+1
	rep #$30
	.a16
	.i16
	ldx #.loword(snes_repack)
	ldy #.loword(snes_repack)+32
	clc
	:
		lda #32-1
		;mvn #^*,#^* ; note: clobbers data bank
		.byte $54, ^$80, ^$80 ; ca65 MVN syntax pre-V2.18 is inconsistent, doing it manually
		tya
		tax
		adc #32
		tay
		dec z:ptr3 ; next row
		bne :-
	; return
	sep #$30
	plb
	.a8
	.i8
	rts	
@remap: ; remaps low 1 bit of attribute to tile select, and low 2 bits of attribute to palette
.repeat 256, I
	.byte (((I<<2)|I|(I>>2)|(I>>4))&$0C) | ((I|(I>>2)|(I>>4)|(I>>6))&$01)
.endrepeat

snes_ppu_fill_att: ; ptr1=addr, ptr2=count, tmp1=data
	; copy to _ppu_send temporarily
	ldx #0
	lda z:tmp1
	:
		sta _ppu_send, X
		inx
		cpx z:ptr2+0
		bcc :-
	; repack attributes
	lda z:ptr2+0
	jsr snes_repack_att
	; send
	rep #$20
	.a16
	asl z:ptr2
	asl z:ptr2
	asl z:ptr2
	asl z:ptr2 ; count *= 16
	lda z:ptr1
	jsr snes_redirect_att
	sta a:$2116 ; VMADD
	sep #$20
	rep #$10
	.a8
	.i16
	lda #$80
	sta a:$2115 ; VMAIN increment on $2119
	ldx #0
:
	lda snes_repack, X
	sta a:$2119 ; VMDATAH
	inx
	cpx Z:ptr2
	bcc :-
	sep #$10
	.i8
	stz a:$2115 ; VMAIN default increment on $2118
	rtl

snes_ppu_apply: ; note: not used for OBJ CHR
	; TODO
	rtl
.if 0
	bit $2002
	lda _ppu_send_addr+1
	sta $2006
	lda _ppu_send_addr+0
	sta $2006
	lda ppu_2000
	sta $2000 ; set direction
	ldx #0
	cpx _ppu_send_count
	bcs :++
	:
		lda _ppu_send, X
		sta $2007
		inx
		cpx _ppu_send_count
		bcc :-
	:
	ldx #0
	stx _ppu_send_count
	rts
.endif

.macro PALETTE_TRANSLATE n_, s_
	ldx _palette+(n_)
	lda snes_pal0, X
	sta snes_palette+((s_)*2)+0
	lda snes_pal1, X
	sta snes_palette+((s_)*2)+1
.endmacro

snes_ppu_post:
	lda z:ppu_post_mode
	bne :+
		rtl
	:
	; translate palettes
	phb
	lda #^snes_pal0
	pha
	plb
	PALETTE_TRANSLATE  0,  0
	; BG is really 1bpp, only needs 1 colour per palette
	PALETTE_TRANSLATE  0+1,  0+1
	PALETTE_TRANSLATE  4+2,  4+1
	PALETTE_TRANSLATE  8+1,  8+1
	PALETTE_TRANSLATE 12+2, 12+1
	; OBJ translates 21 to 0201
	PALETTE_TRANSLATE 16+1,128+1
	PALETTE_TRANSLATE 16+2,128+4
	PALETTE_TRANSLATE 16+3,128+5
	PALETTE_TRANSLATE 20+1,144+1
	PALETTE_TRANSLATE 20+2,144+4
	PALETTE_TRANSLATE 20+3,144+5
	PALETTE_TRANSLATE 24+1,160+1
	PALETTE_TRANSLATE 24+2,160+4
	PALETTE_TRANSLATE 24+3,160+5
	PALETTE_TRANSLATE 28+1,176+1
	PALETTE_TRANSLATE 28+2,176+4
	PALETTE_TRANSLATE 28+3,176+5
	; copy and convert OAM
	ldy #0
	:
		lda _oam+3, Y ; X
		sta snes_oam+0, Y
		lda _oam+0, Y ; Y
		sta snes_oam+1, Y
		lda _oam+1, Y ; tile
		sta snes_oam+2, Y
		ldx _oam+2, Y ; attribute
		lda snes_oam_convert, X
		sta snes_oam+3, Y
		iny
		iny
		iny
		iny
		bne :-
	; TODO translate PPU update
	; TODO build HDMA gradient
	lda z:ppu_post_mode
	sta z:snes_post_mode ; TODO translate this too
	plb
	wai ; TODO wait for interrupt to proceed
	rtl
