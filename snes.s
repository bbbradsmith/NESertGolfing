;
; snes.s
; sNESert Golfing, by Brad Smith 2019-2022
; http://rainwarrior.ca
;

.p816
.a8
.i8

.importzp _i, _k, _k, _l
.importzp _mx, _nx, _ox, _px, _ux, _vx
.importzp _ptr
.importzp ppu_post_mode
.importzp _gamepad
.importzp _mouse1
.importzp _mouse2
.importzp _mouse3
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
.import reset_stub : far

.exportzp _snes_texture

.export snes_reset : far
.export snes_init : far
.export snes_nmi : far
.export snes_input_setup : far
.export snes_input_poll : far
.export snes_mouse_sense : far

.export snes_ppu_load_chr : far
.export snes_ppu_fill : far
.export snes_ppu_fill_2119 : far
.export snes_ppu_fill_att : far
.export snes_ppu_apply : far
.export snes_ppu_post : far

VRAM_CHR_BG3 = $0000
VRAM_NMT_BG3 = $2000
VRAM_NMT_BG1 = $2800
VRAM_CHR_BG1 = $3000
VRAM_CHR_OBJ = $4000

GRADIENT_TOP = 28 ; lines before start of gradient
GRADIENT_SPLIT = 112 ; should be multiple of 16 (113-GRADIENT_TOP) < split < 128

.segment "ZEROPAGE"

snes_post_mode: .res 1
mouse_sense_cycles: .res 1
mouse_index: .res 1
multitap_on: .res 1
input_temp: .res 10 ; JOY1, JOY2, Mouse / JOY3, JOY4, JOY5
gamepad_axlr: .res 1
snes_graphics: .res 1
snes_inidisp: .res 1 ; mirror of INIDISP
_snes_texture: .res 1
snes_texture_last: .res 1

.segment "SNESRAM"

snes_repack:    .res 1024      ; buffer for repacking attributes/CHR
snes_palette:   .res 512       ; buffer for translated palettes
snes_oam:       .res 256       ; buffer for translated oam
hdma_gradient0: .res (4*240)+4 ; Colour 0 gradient
hdma_gradient1: .res (4*240)+4 ; double buffer
hdma_buffer:    .res 1         ; indicates which buffer
hdma_last0:     .res 1         ; indicates when to rebuild the gradient
hdma_last1:     .res 1

.enum
	POST_OFF    = 1
	POST_NONE   = 2
	POST_UPDATE = 3
	POST_DOUBLE = 4
	; added SNES
	POST_SNES_ATT = 5
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
	; FastROM enable
	lda #1
	sta a:$420D
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
	lda #%10010100
	sta a:$2131 ; CGADSUB subtract from BG3+OBJ only
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
	; set up BG1 CHR and nametable for textured ground
	jsr snes_texture_init
	lda #1
	sta snes_graphics ; on by default
	; begin
	lda #$8F
	sta snes_inidisp ; initialize INIDISP mirror with forced blanking on
	lda #$80 ; NMI on, automatic joypad off (will be turned on by _input_setup)
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
	bcc @post_off
	cmp #POST_SNES_ATT+1
	bcc @post_main
	; otherwise POST_OFF
@post_off:
	lda #$8F
	sta a:$2100 ; INIDISP screen off
	sta snes_inidisp
	jmp @end
@post_main:
	jsr snes_post_send
	stz a:_ppu_send_count
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
	ldx #33*2
	stx a:$4305 ; 33 colours
	ldx #1
	stx a:$420B
	; OBJ palettes
	ldx #128
	stx a:$2121 ; CGADD = 128
	lda #.loword(snes_palette+(128*2))
	sta a:$4302
	ldx #80*2
	stx a:$4305 ; 80 colours
	ldx #1
	stx a:$420B
	sep #$20
	.a8
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
	; HDMA channel 7 for gradient
	lda #$03
	sta $4370 ; DMA pattern: +0, +0, +1, +1
	lda #$21
	sta $4371 ; CGADD $2121 + CGDATA $2122
	lda hdma_buffer
	and #1
	bne :+
		lda #<hdma_gradient0
		sta $4372
		lda #>hdma_gradient0
		bra :++
	:
		lda #<hdma_gradient1
		sta $4372
		lda #>hdma_gradient1
	:
	sta $4373
	lda #^hdma_gradient0
	sta $4374
	; INIDISP screen on
	lda #$0F
	sta a:$2100
	sta snes_inidisp
	stz z:ppu_post_mode
@end:
	ldx #0
	lda snes_inidisp
	bmi :+ ; forced blank. don't enable HDMA
	lda snes_graphics
	bne :+
		stz a:$212D ; TS off
		bra :++
	:
		lda #%00000001
		sta a:$212D ; TS BG1
		ldx #$80
	:
	stx a:$420C ; HDMA on or off depending on snes_graphics setting
	jsr input_update
	jsl sound_update_long ; updates nes_apu
	jsr spc_update
	rtl

; =====
; Input
; =====

mouse_sense_cycle: ; should only be called when auto-read is disabled
	.a8
	lda #1
	sta a:$4016
	lda a:$4016
	lda a:$4017
	stz a:$4016
	rts

mouse_poll: ; X = port 0,1
	.a8
	.i8
	ldy #16
	:
		lda a:$4016, X
		lsr
		rol z:input_temp+4
		rol z:input_temp+5
		nop
		nop
		nop
		nop ; margin of safety for Hyperkin mouse clone which expects at least 170 cycles between reads
		dey
		bne :-
	rts

autoread_wait: ; wait until $4212.1 to clear indicating autoread is finished
	.a8
:
	lda a:$4212 ; HVBJOY
	and #1
	bne :-
	rts

input_update: ; runs during NMI to complement automatic hardware poll
	.a8
	.i8
	jsr autoread_wait
	rep #$20
	.a16
	lda a:$4218 ; JOY1
	sta z:input_temp+0
	lda a:$421A ; JOY2
	sta z:input_temp+2
	sep #$20
	.a8
	ldx z:mouse_index
	cpx #2
	bcs @no_mouse
	ldy #5 ; short delay for Hyperkin mouse clone safety which expects at least 336 cycles between the 16th and 17th read
	:
		dey
		bne :-
	jsr mouse_poll
	; cycle mouse if requested
	lda z:mouse_sense_cycles
	beq :++
	:
		jsr mouse_sense_cycle
		dec z:mouse_sense_cycles
		bne :-
	:
	rts
@no_mouse:
	ldx z:multitap_on
	beq @end
	; note: we could poll $4017 one extra time here to read the 17th bits indicating whether controllers 2+3 are connected (not needed for this game)
	lda #$7F
	sta a:$4201 ; WRIO selects controllers 4/5
	ldy #16
	:
		lda a:$4017
		lsr
		rol z:input_temp+6
		rol z:input_temp+7
		lsr
		rol z:input_temp+8
		rol z:input_temp+9
		dey
		bne :-
	; note: again we could check the 17th bits to check for controllers 4+5 (not needed for this game)
	lda #$FF
	sta a:$4201 ; WRIO reset for next frame
	rep #$20
	.a16
	lda a:$421E ; JOY4 is controller 3
	sta z:input_temp+4
	sep #$20
	.a8
@end:
	rts

snes_input_setup: ; $4200=$80 (NMI on, auto-read off)
	.a8
	.i8
	lda #$80
	sta a:$4200 ; NMI on, auto-read off
	;
	; 1. detect mouse (just look for 0001 signature on 16-bit report from $4106/$4017)
	;
	lda #1
	sta a:$4016
	stz a:$4016
	ldx #1
	jsr mouse_poll
	lda z:input_temp+4
	sta z:input_temp+6
	lda z:input_temp+5
	sta z:input_temp+7
	ldx #0
	jsr mouse_poll
	; 5:4 = $4016 report, 7:6 = $4017 report
	lda #2
	sta mouse_index ; mouse_index 2 = no mouse detected
	ldx #2
@mouse_check: ; check port X/2 for mouse (counting down so that lowest port wins)
	lda input_temp+4, X
	and #$0F
	cmp #1
	bne :+ ; missing signature
		txa
		lsr ; X/2
		sta mouse_index
	:
	dex
	dex
	cpx #4
	bcc @mouse_check
	;
	; 2. cycle sensitivity once, then set mouse to medium sensitivity setting
	;
	ldx mouse_index
	cpx #2
	bcs @multitap_check
	lda #4 ; maximum 4 attempts (3 should be sufficient, the 4th is for luck)
	sta z:input_temp+6 ; temporary counter
	:
		jsr mouse_sense_cycle
		ldx mouse_index
		jsr mouse_poll
		lda mouse_index
		asl
		tax
		lda z:input_temp+4, X
		and #$30
		cmp #$10 ; medium setting
		beq :+
		dec z:input_temp+6
		bne :-
	:
	stz z:multitap_on ; mouse does not allow multitap
	bra @finish
	;
	; 3. detect multitap
	;
@multitap_check:
	lda #$FF
	sta a:$4201 ; reset WRIO (should be this at startup)
	lda #1
	sta a:$4016
	jsr @read_4017_d1x8
	cmp #$FF
	bne @multitap_off
	stz a:$4016
	jsr @read_4017_d1x8
	cmp #$FF
	beq @multitap_off
@multitap_on:
	lda #1
	sta z:multitap_on
	bra @finish
@multitap_off:
	stz a:$4016
	stz z:multitap_on
	bra @finish
@read_4017_d1x8:
	ldx #8
	:
		lda a:$4017
		lsr
		lsr
		rol z:input_temp+2
		dex
		bne :-
	lda z:input_temp+2
	rts
@finish:
	; NMI on, auto-read on
	lda #$81
	sta a:$4200
	rtl

snes_input_poll: ; copies NMI polled results
	.a8
	.i8
	ldx z:mouse_index
	cpx #2
	bcc @mouse
	stz z:_mouse1
	stz z:_mouse2
	stz z:_mouse3
	bra @mouse_end
@mouse:
	txa
	asl
	tax
	lda z:input_temp+0, X
	sta z:_mouse1
	lda z:input_temp+4
	bpl :+ ; convert signed magnitude to two's complement
		eor #$7F
		clc
		adc #1
	:
	sta z:_mouse3
	lda z:input_temp+5
	bpl :+
		eor #$7F
		clc
		adc #1
	:
	sta z:_mouse2
@mouse_end:
	stz z:_gamepad
	stz z:gamepad_axlr
	ldx #4 ; default 2 controllers check
	lda z:multitap_on
	beq :+
		ldx #10 ; 5 controllers check
	:
	stx z:tmp1
	ldx #0
@add_controller:
	lda z:input_temp+0, X
	and #$0F
	bne @next ; signature should be 0000
	lda z:input_temp+1, X
	cmp #$FF
	beq @next ; $FF is invalid (up+down+left+right), could mean disconnected?
	ora z:_gamepad
	sta z:_gamepad ; combine with other controllers
	lda z:input_temp+0, X
	and #$F0
	ora z:gamepad_axlr
	sta z:gamepad_axlr
@next:
	inx
	inx
	cpx z:tmp1
	bcc @add_controller
	; L+R+SELECT+START to reset
	lda z:_gamepad
	and #$30
	cmp #$30
	bne :+
	lda z:gamepad_axlr
	and #$30
	cmp #$30
	bne :+
		jml reset_stub
	:
	; forward A/X to Y/B
	lda z:gamepad_axlr
	and #$80 ; A
	beq :+
		lsr ; remap to Y
		tsb z:_gamepad
	:
	lda z:gamepad_axlr
	and #$40 ; X
	beq :+
		asl ; remap to B
		tsb z:_gamepad
	:
	; L/R enable/disable SNES graphics
	lda z:gamepad_axlr
	and #$10 ; R
	beq :+
		lda #1
		sta z:snes_graphics
	:
	lda z:gamepad_axlr
	and #$20 ; L
	beq :+
		stz z:snes_graphics
	:
	; done
	rtl

snes_mouse_sense:
	.a8
	.i8
	inc z:mouse_sense_cycles ; cycle mouse sensitivity during next NMI
	rtl

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

; textures for BG1

texture_chr:
	.incbin "texture.chr"
	TEXTURE_CHR_SIZE = * - texture_chr

texture_pal:
	.incbin "texture.pal"

texture_nmt:
; $00-0F $00-0F
; $10-1F $10-1F
; $20-2F $20-2F
; $30-3F $30-3F
; then the same but shifted by 8
.repeat 128, I
	.byte ((I & ~31) >> 1) | ((I+0) & 15), 1 << 2
.endrepeat
.repeat 128, I
	.byte ((I & ~31) >> 1) | ((I+8) & 15), 1 << 2
.endrepeat

texture_nmt_blank: ; row of blank tiles
.repeat 32
	.byte $40, 1<<2
.endrepeat

snes_texture_init:
	.a8
	.i8
	lda #$80
	sta $2115 ; VMAIN increment on $2119
	rep #$20
	.a16
	; DMA to load CHR
	lda #VRAM_CHR_BG1
	sta $2116 ; VMADD
	ldx #%00000001 ; 2-to-2, no increment
	stx $4300
	ldx #$18 ; $2118 VMDATA
	stx $4301
	lda #.loword(texture_chr)
	sta $4302
	ldx #^texture_chr
	stx $4304
	lda #TEXTURE_CHR_SIZE
	sta $4305
	ldx #1
	stx $420B
	; setup nametable (4 copies of 512 bytes)
	lda #VRAM_NMT_BG1
	sta $2116 ; VMADD
	ldy #4
	:
		lda #.loword(texture_nmt)
		sta $4302
		lda #512
		sta $4305
		ldx #1
		stx $420B
		dey
		bne :-
	; blank row 2 of texture nametable (prevents texture on score)
	lda #VRAM_NMT_BG1+(2*32)
	sta $2116
	;lda #.loword(texture_blank) ; already follows texture_chr
	;sta $4302
	lda #64
	sta $4305
	ldx #1
	stx $420B
	sep #$30
	.a8
	.i8
	stz $2115 ; VMAIN default increment on $2118
	lda #$FF
	sta snes_texture_last
	rts
@byte_00:
	.byte $00

snes_texture_palette:
	.a8
	.i8
	ldx _snes_texture
	stx snes_texture_last
	rep #$30
	.a16
	.i16
	; copy: texture_pal+(32*_snes_texture) -> snes_palette+32
	txa
	and #3
	asl
	asl
	asl
	asl
	asl
	clc
	adc #.loword(texture_pal)
	tax
	ldy #.loword(snes_palette+32)
	lda #32-1
	phb
	.byte $54, ^snes_palette, ^texture_pal ; ca65 MVN syntax pre-V2.18 is inconsistent, doing it manually
	plb
	sep #$30
	.a8
	.i8	
	rts

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
snes_oam_convert: ; vhp..ccc -> vhPPccc0 (duplicate+negate priority bit, select OBJ page 0)
	.repeat 256, I
		.byte ((I & $E0) | ((I>>1) & $10) | ((I & $07) << 1) | $00) ^ $30
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

snes_ppu_fill_2119: ; ptr1=addr, ptr2=count; tmp1=data
	.a8
	.i8
	rep #$10
	.i16
	ldx z:ptr1
	stx a:$2116
	lda #$80
	sta a:$2115 ; VMAIN increment on $2119
	lda z:tmp1
	ldx z:ptr2
	:
		sta a:$2119 ; VMDATAH
		dex
		bne :-
	stz a:$2115 ; VMAIN default increment on $2118
	sep #$10
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

snes_post_remap: ; A=source POST enum, translates PPU update, returns SNES POST enum.
	cmp #POST_UPDATE
	beq @update
	cmp #POST_DOUBLE
	beq @keep
	cmp #POST_NONE
	beq @keep
	; everything else assumed POST_OFF
	lda #POST_OFF
@keep:
	rts ; NON/DOUBLE don't need repack
@update:
	lda a:_ppu_send_addr+1
	cmp #$20
	bcc @chr ; < $2000 = CHR
	and #$03
	cmp #$03
	bne @nmt
	lda a:_ppu_send_addr+0
	cmp #$C0
	bcs @att ; >= $23C0 / 27C0... = ATT
@nmt: 
	; NMT needs no repack
	lda #POST_UPDATE
	rts
@att:
	lda a:_ppu_send_count
	jsr snes_repack_att
	rep #$20
	.a16
	lda a:_ppu_send_addr
	jsr snes_redirect_att
	sta a:_ppu_send_addr
	sep #$20
	.a8
	lda #POST_SNES_ATT
	rts
@chr:
	; _ppu_send_count assumed 8
	; _ppu_send_addr assumed a multiple of 8
	rep #$20
	.a16
	lda a:_ppu_send_addr
	lsr
	pha
	and #7
	beq :+
		pla
		eor #$0800 ; every second tile goes to second page
		bra :++
	:
		pla
	:
	and #$FFF8 ; remove "4" from second page address
	sta a:_ppu_send_addr
	sep #$20
	.a8
	lda #POST_UPDATE
	rts

snes_build_hdma_gradient: ; takes about 60% of a frame, but doesn't happen often (and never during gameplay)
	.a8
	.i8
	; flip buffer
	lda hdma_buffer
	eor #1
	and #1
	sta hdma_buffer
	; ptr1/ptr2: fetch target colours: palette 0 = top, palette 16 = bottom
	ldx _palette+0
	stx hdma_last0
	lda snes_pal0, X ; gggrrrrr
	sta ptr1+0
	lda snes_pal1, X ; 0bbbbbgg
	sta ptr1+1
	ldx _palette+16
	stx hdma_last1
	lda snes_pal0, X
	sta ptr2+0
	lda snes_pal1, X
	sta ptr2+1
	; _mx,_nx, _ox = starting RGB
	rep #$20
	.a16
	lda ptr1
	xba
	and #$1F00
	sta _mx ; _mx = red << 8
	lda ptr1
	asl
	asl
	asl
	and #$1F00
	sta _nx ; _nx = green << 8
	lda ptr1
	lsr
	lsr
	and #$1F00
	sta _ox ; _ox = blue << 8
	; _px, _ux, _vx = RGB increment per line
	lda ptr2
	xba
	and #$1F00
	sec
	sbc _mx
	jsr @div_gradient
	sta _px ; 256 * ((target red - start red) / lines)
	lda ptr2
	asl
	asl
	asl
	and #$1F00
	sec
	sbc _nx
	jsr @div_gradient
	sta _ux
	lda ptr2
	lsr
	lsr
	and #$1F00
	sec
	sbc _ox
	jsr @div_gradient
	sta _vx
	; if all are 0 add an artificial gradient
	lda _px
	ora _ux
	ora _vx
	bne @build
	ldx #0
	.assert (_nx = _mx+2), error, "temporary gradient variables are not contiguous"
	.assert (_ox = _nx+2), error, "temporary gradient variables are not contiguous"
	.assert (_ux = _px+2), error, "temporary gradient variables are not contiguous"
	.assert (_vx = _ux+2), error, "temporary gradient variables are not contiguous"
	@artificial:
		lda _mx, X
		cmp #$1000
		bcs :+
			lda #$0004
			bra :++
		:
			lda #.loword(-$0004)
		:
		sta _px, X
		inx
		inx
		cpx #6
		bcc @artificial
	lda _mx
@build:
	; build HDMA buffer
	phb
	ldx #^@dither
	phx
	plb
	rep #$10
	.i16
	ldx #0
	lda hdma_buffer
	and #1
	beq :+
		ldx #(hdma_gradient1-hdma_gradient0)
	:
	lda #GRADIENT_TOP-1 ; hold first colour for this many lines
	and #$00FF
	sta hdma_gradient0+0, X
	;stz hdma_gradient0+1, X ; initialized as 0, sets CGADD
	lda ptr1
	sta hdma_gradient0+3, X ; CGCOL for scanline 0
	lda #$80 | GRADIENT_SPLIT
	sta hdma_gradient0+5, X ; do 120 lines
	lda #$80 | (240-(GRADIENT_SPLIT+GRADIENT_TOP))
	sta hdma_gradient0+6+(GRADIENT_SPLIT*4), X
	txa
	clc
	adc #6
	tax
	ldy #GRADIENT_SPLIT
	jsr @row_gradient
	inx ; skip line count byte
	ldy #(240-(GRADIENT_SPLIT+GRADIENT_TOP))
	jsr @row_gradient
	sep #$30
	plb
	.a8
	.i8
	rts
@dither: ; used bit-reversed line counter Y as dither compare
	.byte 255
	.repeat 128, I
		.byte 255-(((I&1)<<7)|((I&2)<<5)|((I&4)<<3)|((I&8)<<1)|((I&16)>>1)|((I&32)>>3)|((I&64)>>5)|((I&128)>>7))
	.endrepeat
.macro DITHER
	xba
	cmp @dither, Y
	bcc :+
		inc
	:
	xba
	and #$1F00
.endmacro
@row_gradient: ; build Y rows of gradient starting at X position, clobbers ptr3
	.a16
	.i16
	lda _px
	clc
	adc _mx
	sta _mx
	lda _ux
	clc
	adc _nx
	sta _nx
	lda _vx
	clc
	adc _ox
	sta _ox
	DITHER
	;and #$1F00
	asl
	asl
	sta ptr3   ; 0bbbbb00.00000000
	lda _nx
	DITHER
	;and #$1F00
	lsr
	lsr
	lsr
	ora ptr3
	sta ptr3   ; 0bbbbbgg.ggg00000
	lda _mx
	DITHER
	;and #$1F00
	xba
	ora ptr3   ; 0bbbbbgg.gggrrrrr
	sta hdma_gradient0+2, X
	inx
	inx
	inx
	inx
	dey
	bne @row_gradient
	rts
@div_gradient:
	.a16
	.i8
	GRADIENT_DIV = 240-GRADIENT_TOP
	ldy #0 ; Y=0 no invert
	cmp #$8000
	bcc :+
		eor #$FFFF
		inc
		iny ; Y=1 invert (to keep division unsigned)
	:
	sta $4204 ; WRDIVL/H
	ldx #GRADIENT_DIV
	stx $4206 ; WRDIVB
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	lda $4214 ; RDDIV
	dey
	bne :+ ; undo invert
		eor #$FFFF
		inc
	:
	rts

snes_post_send: ; A = update type
	.a8
	.i8
	cmp #POST_UPDATE
	beq @update
	cmp #POST_DOUBLE
	beq @double
	cmp #POST_SNES_ATT
	beq @att
	rts ; unknown
@update:
	lda a:ppu_2000
	and #%00000100
	beq :+
		lda #$01
		sta a:$2115 ; VMAIN increment 32 bytes after $2118
	:
	jsr @dma_common
	rep #$20
	.a16
	lda a:_ppu_send_addr
	sta a:$2116 ; VMADD
	lda #.loword(_ppu_send)
	sta a:$4302
	ldx a:_ppu_send_count
	beq :+
		stx a:$4305
		ldx #1
		stx a:$420B ; DMA
	:
	sep #$20
	.a8
	stz a:$2115 ; VMAIN default increment after $2118
	rts
@double:
	jsr @dma_common
	rep #$20
	.a16
	lda a:_ppu_send_addr
	sta a:$2116 ; VMADD
	lda #.loword(_ppu_send)
	sta a:$4302
	ldx #32
	stx a:$4305 ; 32 bytes
	ldx #1
	stx a:$420B ; DMA
	lda a:_ppu_send_addr
	eor #$0400 ; switch horizontal nametable
	sta a:$2116 ; VMADD
	ldx #32
	stx a:$4305 ; 32 bytes
	ldx #1
	stx a:$420B ; DMA
	sep #$20
	.a8
	rts
@att:
	lda #$80
	sta a:$2115 ; VMAIN increment after $2119
	jsr @dma_common
	lda #$19
	sta a:$4301 ; $2119 VMDATAH
	rep #$20
	.a16
	lda a:_ppu_send_addr
	sta a:$2116 ; VMADD
	lda #.loword(snes_repack)
	sta a:$4302
	lda a:_ppu_send_count
	and #$00FF
	asl
	asl
	asl
	asl
	sta a:$4305 ; count * 16
	beq :+
		ldx #1
		stx a:$420B ; DMA
	:
	sep #$20
	.a8
	stz a:$2115 ; VMAIN default increment after $2118
	rts
@dma_common:
	ldx #0
	stx a:$4300 ; 1-to-1 no increment
	stz a:$4306 ; less than 256 bytes
	ldx #$18
	stx a:$4301 ; $2118 VMDATAL
	ldx #$7E
	stx a:$4304 ; RAM bank
	rts

snes_ppu_apply: ; note: not used for OBJ CHR
	lda #POST_UPDATE
	jsr snes_post_remap
	jsr snes_post_send
	stz a:_ppu_send_count
	rtl

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
	; palette 16 isn't used but is useful for debugging the HDMA gradient
	PALETTE_TRANSLATE  0,  0
	PALETTE_TRANSLATE 16, 32
	; BG is really 1bpp, only needs 1 colour per palette
	PALETTE_TRANSLATE  0+1,  0+1
	PALETTE_TRANSLATE  0+3,  0+3
	PALETTE_TRANSLATE  4+2,  4+1
	PALETTE_TRANSLATE  8+1,  8+1
	PALETTE_TRANSLATE  8+3,  8+3
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
	; extra OBJ palette 4 for tee ground to accept colour math
	PALETTE_TRANSLATE 24+2,192+4
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
	; rebuild HDMA gradient if changed
	lda snes_graphics
	beq @post
	lda _palette+0
	cmp hdma_last0
	bne :+
	lda _palette+16
	cmp hdma_last1
	beq :++
	:
		jsr snes_build_hdma_gradient
	:
	lda _snes_texture
	cmp snes_texture_last
	beq :+
		jsr snes_texture_palette
	:
@post:
	lda z:ppu_post_mode
	jsr snes_post_remap
	sta z:snes_post_mode
	plb
	wai
	rtl
