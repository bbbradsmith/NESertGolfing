; TODO
; go through dgolf.s for direct register writes ($2000-$2007) and replace code
; rewrite OAM building code to use SNES attributes (might need to edit C code too if it has built-in attribute)
; NMI handler, translate packed 1bpp into two separate VRAM locations, translate palettes, etc.
; replace NES $2007 bulk uploads with dedicated CHR loading functions for unpacking/fixing/etc.
; video memory setup, etc.
; sky palette builder
; blended texture for ground
; support for other SNES buttons, maybe L/R/Select/Start reset?

.p816
.a8
.i8

.importzp _i, _k, _k, _l
.importzp _mx, _nx, _ox, _px, _ux, _vx

.import nes_apu : abs

.import sound_update_long : far

.export snes_reset : far
.export snes_init : far
.export snes_nmi : far

.segment "SNESRAM"

hdma_gradient: .res (2*240)+3 ; Colour 0 gradient buffer

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
	; initialize registers $2100-2133 (PPU mmostly)
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
@reset_2100:
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
	; TODO set up video RAM arrangements, display style, etc.
	;lda #%00011110 ; No emphasis, no greyscale, BG and sprite shown, no hidden column
	;sta ppu_2001
	;lda #%10001000 ; NMI on, 8-pixel sprites, BG page 0, Sprite page 1
	;sta ppu_2000
	;sta $2000 ; turn NMI on permanently
	
	; TODO
	; use BG3 for NES stuff
	; but expand 1BPP tiles automatically into a larger address space
	; use colour 0 with HDMA gradient (make a table of 2 SNES colours for each NES background colour? replace "blend" at higher level)
	; use BG1 with a tiled texture for some subtle sand grain? maybe just some subtractive blend;
	;  of a grit texture in a diagonal wood-grainy wave? could mabe have 4 2-bit multiple textures w/palette selectors, ferrari style

	rtl

snes_nmi:
	.a8
	.i8
	rep #$30
	.a16
	.i16
	pha
	phx
	phy
	phb
	sep #$30
	.a8
	.i8
	; TODO NES NMI stuff, see below
@end:
	jsl sound_update_long ; updates nes_apu
	jsr spc_update
	rep #$30
	.a16
	.i16
	plb
	ply
	plx
	pla
	rti
	.a8
	.i8

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

; TODO build HDMA gradient
; 1bpp CHR unpacking?
