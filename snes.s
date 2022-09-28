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

.segment "SNESRAM"

snes_repack:    .res 1024      ; buffer for repacking attributes/CHR
snes_palette:   .res 512       ; buffer for translated palettes
hdma_gradient:  .res (2*240)+3 ; Colour 0 gradient

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
	; clear RAM outside of NES area
	lda #0 ; set first byte of each region to 0, use MVN to copy-fill
	sta f:$7E0800
	sta f:$7F0000
	rep #$30
	.a16
	.i16
	ldx #$0800
	ldy #$0801
	lda #$10000-$802
	.byte $54,$7E,$7E ; MVN #$7E,#$7E (for compatibility with old assembler versions)
	ldx #$0000
	ldy #$0001
	lda #$10000-2
	.byte $54,$7F,$7F ; MVN #$7F,#$7F
	sep #$30
	.a8
	.i8
	; set default data bank
	lda #$80
	pha
	plb
	; clear VRAM with DMA
	stz a:2116
	stz a:2117
	lda #%00001001 ; 2-to-2, no increment
	sta a:$4300
	lda #$18 ; $2118-9 VMDATA
	sta a:$4301
	lda #<@bin_00
	sta a:$4302
	lda #>@bin_00
	sta a:$4303
	lda #^@bin_00
	sta a:$4304
	stz a:$4305
	stz a:$4306 ; 64k
	lda #1
	sta a:$420B
	; clear CGRAM with DMA
	stz a:$2121 ; CGADD
	lda #%00001010 ; 2-to-1, no increment
	sta a:$4300
	lda #$22 ; $2122 CGDATA
	sta a:$4301
	stz a:$4305
	lda #>512
	sta a:$4306 ; 512 bytes
	lda #1
	sta a:$420B
	; clear OAM with DMA
	stz a:$2012
	stz a:$2103
	lda #%00001000 ; 1-to-1, no increment
	sta a:$4300
	lda #$04 ; $2104 OAMDATA
	sta a:$4301
	lda #<(512+32)
	sta a:$4305
	lda #>(512+32)
	sta A:$4306 ; 512+32 bytes
	lda #1
	sta a:$420B
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
	lda #8
	sta a:$2126 ; WH0 window 1 left
	lda #255
	sta a:$2127 ; WH1 window 1 right
	stz a:$212A ; WBGLOG OR logic
	stz a:$212B ; WOBJLOG OR logic
	lda #%0001111
	sta a:$212E ; TMW apply windows to all layers
	sta a:$212F ; TSW
	; subtractive colour math for BG1
	lda #%11110010
	sta a:$2130 ; CGWSEL subscreen everwhere
	lda #%10000100
	; TODO enable
	;sta a:$2131 ; CGADSUB subtract from BG3 only
	; global screen settings
	lda #$04
	sta a:$2133 ; SETINI overscan 239 lines mode
	lda #%00010100
	sta a:$212C ; TM OBJ + BG3
	lda #%00000001
	sta a:$212D ; TS BG1
	lda #$01
	sta a:$2105 ; BGMODE mode 1
	; default increment on $2118
	stz a:$2115
	; begin
	lda #$81
	sta a:$4200 ; NMITIMEN turn on NMI
	rtl

snes_nmi:
	.a8
	.i8
	; TODO NES NMI stuff, see below
	; types of data:
	; - nametable (1 or 2 rows)
	; - vertical nametable ?
	; - attribute (64 bytes)
	; - CHR 64 bytes?
@end:
	;jsl sound_update_long ; updates nes_apu
	;jsr spc_update
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
	cpx a:$2142 ; APUIO2
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
@bg_chr: ; splits 8+8 byte planes across $0000 and $1000
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
	clc
	adc #$1000
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
	lda z:ptr2
	sec
	sbc #16
	bcs :+
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
	ora #$1000
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

snes_ppu_fill_att: ; ptr1=addr, ptr2=count, tmp1=data
	; TODO
	rtl

snes_ppu_apply: ; note: not used for attribute or OBJ CHR
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

snes_ppu_post:
	lda z:ppu_post_mode
	bne :+
		rtl
	:
	; TODO translate PPU update, palettes, CHR unpacking, etc.
	; build HDMA gradient?
	wai ; TODO wait for interrupt to proceed
	rtl
