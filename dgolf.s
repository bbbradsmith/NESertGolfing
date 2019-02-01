;-
; dgolf.s
; NESert Golfing, by Brad Smith 2019
; http://rainwarrior.ca
;

.macpack longbranch

; ===
; RAM
; ===

.segment "ZEROPAGE"
_ptr:          .res 2 ; shared pointer for C interface
_seed:         .res 4 ; random number seed (only using low 3 bytes)
ppu_post_mode: .res 1
sound_ptr:     .res 2
_input:        .res 16
mouse_index:   .res 1
fourscore_off: .res 1
_gamepad:      .res 1
_mouse1:       .res 1
_mouse2:       .res 1
_mouse3:       .res 1

; convenient index/temporary values
_i:    .res 1
_j:    .res 1
_k:    .res 1
_l:    .res 1
_mx:   .res 2
_nx:   .res 2
_ox:   .res 2
_px:   .res 2
_ux:   .res 2
_vx:   .res 2

; nesert golfing

_floor_column:      .res 1
_weather_tile:      .res 1
_weather_attribute: .res 1 ; bits 2-4 are speed of descent
_weather_wind_dir:  .res 1 ; wind direction: 0, 1, or -1
_weather_wind_p:    .res 1 ; wind probability 0 = never, 255 = almost 1 per frame
_weather_rate_min:  .res 1 ; minimum wait until next spawn
_weather_rate_mask: .res 1 ; prng mask add until next spawn
weather_wait:       .res 1 ; current wait until next spawn
weather_next:       .res 1 ; next spawn index
_hole:              .res 1 ; current hole (0 for ending)
_ocean_attribute:   .res 1 ; starting byte of ocean attribute (255 before last hole is built)
_tx:                .res 2 ; temporary X
_ty:                .res 2 ; temporary Y
_tsx:               .res 2 ; signed temporary X
_tsy:               .res 2 ; signed temporary Y

_ball_x:            .res 4
_ball_y:            .res 4
_ball_vx:           .res 4
_ball_vy:           .res 4

_balls_x:           .res 16
_balls_y:           .res 4
_balls_fx = _balls_x+0
_balls_lx = _balls_x+1
_balls_hx = _balls_x+2
_balls_wx = _balls_x+1

_norm_x:            .res 2
_norm_y:            .res 2

; sound

apu_out:    .res 16 ; last values written to APU
apu_sqh:    .res 2  ; last written values to $4003/$4007
sound_pos:  .res 1
sound_seed: .res 2
snow_pitch: .res 1
snow_time:  .res 1

.segment "BSS"

CSTACK_SIZE = 128

.align 256
_floor_y:      .res 512 ; floor height
_floor_a:      .res 512 ; floor angle
cstack:        .res 128 ; CC65 internal C stack

_floor_render: .res 64  ; floor render buffer

temp: .res 4

.segment "STACK"
sound:           .res 2
ppu_2000:        .res 1
ppu_2001:        .res 1
ppu_2005x:       .res 2 ; second byte is high bit (redundant in $2000)
ppu_2005y:       .res 2 ; second byte is high bit (redundant in $2000)
_ppu_send_addr:  .res 2
_ppu_send_count: .res 1
_ppu_send:       .res 64
_palette:        .res 32

.segment "OAM"
.align 256
_oam: .res 256

.exportzp _ptr
.exportzp _seed
.exportzp _input, _gamepad, _mouse1, _mouse2, _mouse3
.exportzp _i,_j,_k,_l
.exportzp _mx,_nx,_ox,_px
.exportzp _ux,_vx

.exportzp _floor_column
.exportzp _weather_tile
.exportzp _weather_attribute
.exportzp _weather_wind_dir
.exportzp _weather_wind_p
.exportzp _weather_rate_min
.exportzp _weather_rate_mask
.exportzp _hole
.exportzp _ocean_attribute
.exportzp _tx
.exportzp _ty
.exportzp _tsx
.exportzp _tsy
.exportzp _ball_x
.exportzp _ball_y
.exportzp _ball_vx
.exportzp _ball_vy
.exportzp _balls_x
.exportzp _balls_fx
.exportzp _balls_lx
.exportzp _balls_hx
.exportzp _balls_wx
.exportzp _balls_y
.exportzp _norm_x
.exportzp _norm_y

.export _floor_render
.export _floor_y
.export _floor_a

.export _ppu_send_addr
.export _ppu_send_count
.export _ppu_send
.export _palette
.export _oam

.enum
	POST_OFF    = 1
	POST_NONE   = 2
	POST_UPDATE = 3
	POST_DOUBLE = 4
.endenum

; cc65 temporaries
.importzp sp, sreg, regsave
.importzp ptr1, ptr2, ptr3, ptr4
.importzp tmp1, tmp2, tmp3, tmp4
; cc65 library functions
.import popa ; A = byte from cstack, Y=0, sp+=1
.import popax ; A:X = two bytes from cstack, Y=0, sp+=2

; ==============
; NESert Golfing
; ==============

OCEAN_FLOOR = 224 ; must be multiple of 8 and match parallel definition in dgolf.c

.export _layers_chr
.export _sprite_chr
.export _LAYERS_CHR_SIZE
.export _SPRITE_CHR_SIZE

.export _read_slope
.export _read_norm_x
.export _read_norm_y
.export _floor_render_prepare
.export _weather_shift
.export _weather_animate
.export _fmult

.export _hole

.segment "RODATA"

_layers_chr: .incbin "layers.chr"
_LAYERS_CHR_SIZE: .word * - _layers_chr

_sprite_chr: .incbin "sprite.chr"
_SPRITE_CHR_SIZE: .word * - _sprite_chr

.include "temp/slopes.inc"
;slope_y0/1
;norm_x0/1
;norm_y0/1

.segment "CODE"

_read_slope:
	tay
	ldx slope_y1, Y
	lda slope_y0, Y
	rts
_read_norm_x:
	tay
	ldx norm_x1, Y
	lda norm_x0, Y
	rts
_read_norm_y:
	tay
	ldx norm_y1, Y
	lda norm_y0, Y
	rts

.proc _floor_render_prepare
	; A = 0,1,2,3,4,5, 6+
	;     phases:
	;         0 - build render data (32 bytes of CHR tile, 30 bytes of nametable)
	;         1-4 - prepare CHR tiles for send
	;         5 - prepare nametable column for send, increment column
	;         6-7 ocean attributes on final hill
	cmp #6
	jcs floor_send_ocean
	cmp #0
	beq floor_build
	cmp #5
	bcs floor_send_nmt
floor_send_chr: ; A = 1-4
	tay
	; ppu_send_addr =
	;    vertical: ((A-1) * 256) + 0x800 +
	;    horizontal: ((column & 15) * 16) +
	;    slice: (column & 16) * (16 * 4)
	;    plane: (column & 32) / 4
	clc ; vertical
	adc #$07
	sta _ppu_send_addr+1
	lda _floor_column ; slice
	and #16
	lsr
	lsr
	clc
	adc _ppu_send_addr+1
	sta _ppu_send_addr+1
	lda _floor_column ; horizontal
	;and #15 ; implicit in << 4
	asl
	asl
	asl
	asl
	sta _ppu_send_addr+0
	lda _floor_column ; plane
	and #32
	lsr
	lsr
	ora _ppu_send_addr+0
	sta _ppu_send_addr+0
	; read position = (A-1) * 8
	dey
	tya
	asl
	asl
	asl
	tax
	ldy #0
	:
		lda _floor_render, X
		sta _ppu_send, Y
		inx
		iny
		cpy #8
		bcc :-
	;ldy #8
	sty _ppu_send_count
	lda #0 ; horizontal update
	jsr _ppu_direction
	rts
floor_send_nmt: ; A = 5
	lda _floor_column
	and #31
	sta _ppu_send_addr+0
	lda _floor_column
	and #32
	lsr
	lsr
	lsr
	ora #$20
	sta _ppu_send_addr+1
	; copy nametable data
	ldx #0
	:
		lda _floor_render+32, X
		sta _ppu_send, X
		inx
		cpx #30
		bcc :-
	;ldx #30
	stx _ppu_send_count
	lda #1 ; vertical update
	jsr _ppu_direction
	; increment column
	inc _floor_column
	lda _floor_column
	and #63
	sta _floor_column
	rts
floor_build: ; A = 0
src = ptr1
min = tmp1
mint = tmp2
tile = tmp3
pass = tmp4
	; src = floor_y + (column * 8)
	;lda #0
	sta src+1
	lda _floor_column
	.repeat 3
		asl
		rol src+1
	.endrepeat
	clc
	adc #<_floor_y
	sta src+0
	lda #>_floor_y
	adc src+1
	sta src+1
	; find minimum
	ldy #0
	lda #$FF
	sta min
	@min_loop:
		lda (src), Y
		cmp min
		bcs :+
			sta min
		:
		iny
		cpy #8
		bcc @min_loop
	; min = OCEAN_FLOOR is interpreted as ocean
	lda min
	cmp #OCEAN_FLOOR
	jeq floor_build_ocean
	; round down to nearest tile above the minimum (max 28 tiles down)
	;lda min
	and #%11111000
	cmp #(28*8)
	bcc :+
		lda #(28*8)
	:
	sta min
	lsr
	lsr
	lsr
	sta mint
	; generate CHR plane
	ldy #0
	@chr_loop:
		ldx min
		lda (src), Y
		sta pass
		.repeat 64, I
			cpx pass
			rol _floor_render + I
			inx
		.endrepeat
		iny
		cpy #8
		jcc @chr_loop
	; calculate first tile to use
	lda _floor_column
	and #15
	ora #$80
	sta tile
	lda _floor_column
	and #16
	asl
	asl
	ora tile
	sta tile
	; generate nametable strip
	lda #0 ; blank until mint
	ldx #0
	:
		cpx mint
		bcs :+
		sta _floor_render+32, X
		inx
		jmp :-
	:
	ldy #4 ; 4 tiles
	lda tile
	:
		sta _floor_render+32, X
		inx
		clc
		adc #16
		dey
		bne :-
	lda #$03 ; filled until bottom
	:
		cpx #32
		bcs :+
		sta _floor_render+32, X
		inx
		jmp :-
	:
	rts
floor_build_ocean:
	; the ocean is just tiles of $03 at the bottom
	lda #0
	tax
	:
		sta _floor_render, X
		inx
		cpx #(32+(OCEAN_FLOOR/8))
		bcc :-
	lda #$03
	:
		sta _floor_render, X
		inx
		cpx #64
		bcc :-
	rts
floor_send_ocean:
	; fills the bottom row of attributes to colour the ocean
	tax
	lda _ocean_attribute
	cmp #255
	bne :+
		rts
	:
	tay ; Y = _ocean_attribute
	lda #8
	sta _ppu_send_count
	lda #<$23F8
	sta _ppu_send_addr+0
	cpx #7
	bcs :+
		lda #>$23F8 ; screen 1
		sta _ppu_send_addr+1
		lda #%10101010 ; base attribute
		jmp :++
	:
		lda #>$27F8 ; screen 2
		sta _ppu_send_addr+1
		tya
		sec
		sbc #8
		and #15
		tay ; shift ocean coordinate by -8 bytes
		lda #%11111111 ; base attribute
	:
	; fill with base attribute
	ldx #0
	:
		sta _ppu_send, X
		inx
		cpx #8
		bcc :-
	; write 6 attribute bytes with ocean colour
	ldx #0
	:
		lda #%00000000 ; ocean attribute
		sta _ppu_send, Y
		iny
		tya
		and #15 ; wrap at 16 bytes (the extra 8 bytes is a dummy for other screen)
		tay
		inx
		cpx #6
		bcc :-
	lda #0 ; horizontal update
	jsr _ppu_direction
	rts
.endproc

_weather_shift:
	; A = value to add to all weather particle X coordinates
	sta tmp1
	ldx #(32*4)
	:
		lda tmp1
		clc
		adc _oam + 3, X
		sta _oam + 3, X
		inx
		inx
		inx
		inx
		bne :-
	rts

.proc _weather_animate
	; spawn a tile if appropriate
	lda _weather_rate_min ; min rate of 0 = no weather
	beq animate
	lda weather_wait
	beq spawn
	dec weather_wait
	jmp animate
spawn:
	ldy weather_next
	cpy #(32*4)
	bcs :+
		ldy #(32*4)
	:
	lda _oam + 0, Y
	cmp #240
	bcc spawn_defer ; particle active, try again next frame
	lda #0 ;Y
	sta _oam + 0, Y
	lda _weather_tile
	sta _oam + 1, Y
	lda _weather_attribute
	sta _oam + 2, Y
	jsr _prng ; X
	sta _oam + 3, Y
	; set time until next spawn
	jsr _prng
	and _weather_rate_mask
	clc
	adc _weather_rate_min
	sta weather_wait
spawn_defer:
	; set next spawn index
	iny
	iny
	iny
	iny
	sty weather_next
animate:
	ldx #(32*4)
	; iterate through particles
animate_loop:
	lda _oam + 0, X
	cmp #240
	bcs animate_next ; inactive
	; vertical fall (add speed stored in attribute bits)
	lda _oam + 2, X
	and #%00011100
	lsr
	lsr
	tay ; becomes a multiplier for wind
	adc _oam + 0, X
	sta _oam + 0, X
	; wind
	:
		jsr prng1
		cmp _weather_wind_p
		bcs :+
			lda _oam + 3, X
			clc
			adc _weather_wind_dir
			sta _oam + 3, X
		:
		dey ; multiply wind by fall velocity
		bne :--
	;
	; collide with floor
	lda _oam + 3, X
	clc
	adc ppu_2005x+0
	sta ptr1+0
	lda #0
	adc ppu_2005x+1
	and #1
	.assert (<_floor_y) = 0, error, "_floor_y must be page aligned."
	clc
	adc #>_floor_y
	sta ptr1+1 ; ptr1 = _floor_y + ((particle X + scroll X) & 511)
	;ldy #0
	lda (ptr1), Y
	cmp _oam + 0, X
	bcc @hit
	bne @miss
	@hit:
		lda #240
		sta _oam + 0, X
	@miss:
animate_next:
	inx
	inx
	inx
	inx
	bne animate_loop
	rts
.endproc

.proc _fmult
	; A:X operand a (16-bit)
	; stack = operand b (16-bit)
ba = ptr1+0
dc = ptr1+1
fe = ptr2+0
hg = ptr2+1
out0 = tmp1
out1 = tmp2
out2 = tmp3
mix  = tmp4
sign = ptr3
	; retrieve operands, negate and remember is signed
	sta ba
	stx dc
	lda #0
	sta sign
	cpx #$80
	bcs :+
		lda #0
		jmp :++
	:
		lda #0
		sec
		sbc ba
		sta ba
		lda #0
		sbc dc
		sta dc
		lda #1
	:
	sta sign
	jsr popax
	sta fe
	stx hg
	cpx #$80
	bcc :+
		lda #0
		sec
		sbc fe
		sta fe
		lda #0
		sbc hg
		sta hg
		inc sign
		lda fe
	:
	; unsigned nibble long-multiplcation
	;          d  c  b  a
	;       *  h  g  f  e
	; -------------------
	;          e--c  e--a
	;       e--d  e--b
	;       f--c  f--a
	;    f--d  f--b
	; g--d  g--b
	;    g--c  g--a
	; h--c  h--a
	;    h--b
	;    |<-keep->|        16 bit result
	;    out2  out1  out0
	;
	; 1. e x dcba
	tax ; A = fe
	lda asl4, X
	sta mix ; e0
	lda ba
	and #$0F
	ora mix
	tax
	lda nmult0, X
	sta out0 ; e*a
	lda dc
	and #$0F
	ora mix
	tax
	lda nmult0, X
	sta out1 ; e*c
	lda fe
	and #$0F
	sta mix ; 0e
	lda ba
	and #$F0
	ora mix
	tax
	lda nmult4, X
	clc
	adc out0
	sta out0
	lda nmult8, X
	adc out1
	sta out1 ; e*b (note: carry to out2 isn't possible)
	lda dc
	and #$F0
	ora mix
	tax
	lda nmult4, X
	;clc
	adc out1
	sta out1
	lda nmult8, X
	adc #0
	sta out2 ; e*d
	; 2. f x dcba
	lda fe
	and #$F0
	sta mix ; f0
	lda ba
	and #$0F
	ora mix
	tax
	lda nmult4, X
	;clc
	adc out0
	sta out0
	lda nmult8, X
	adc out1
	sta out1
	lda out2
	adc #0
	sta out2 ; f*a
	lda dc
	and #$0F
	ora mix
	tax
	lda nmult4, X
	;clc
	adc out1
	sta out1
	lda nmult8, X
	adc out2
	sta out2 ; f*c
	ldx fe
	lda lsr4, X
	sta mix ; 0f
	lda ba
	and #$F0
	ora mix
	tax
	lda nmult0, X
	clc
	adc out1
	sta out1 ; f*b + carry
	lda dc
	and #$F0
	ora mix
	tax
	lda nmult0, X
	adc out2
	sta out2 ; f*d
	; 3. g x dcba
	lda hg
	and #$0F
	sta mix ;0g
	lda ba
	and #$F0
	ora mix
	tax
	lda nmult4, X
	clc
	adc out1
	sta out1
	lda nmult8, X
	adc out2
	sta out2 ; g*b
	lda dc
	and #$F0
	ora mix
	tax
	lda nmult4, X
	clc
	adc out2
	sta out2 ; g*d
	ldx hg
	lda asl4, X
	sta mix ; g0
	lda ba
	and #$0F
	ora mix
	tax
	lda nmult0, X
	clc
	adc out1
	sta out1 ; g*a + carry
	lda dc
	and #$0F
	ora mix
	tax
	lda nmult0, X
	adc out2
	sta out2 ; g*c
	; 4. h x dcba
	lda hg
	and #$F0
	sta mix ; h0
	lda ba
	and #$0F
	ora mix
	tax
	lda nmult4, X
	clc
	adc out1
	sta out1
	lda nmult8, X
	adc out2
	sta out2 ; h*a
	lda dc
	and #$0F
	ora mix
	tax
	lda nmult4, X
	clc
	adc out2
	sta out2 ; h*c
	ldx hg
	lda lsr4, X
	sta mix ; 0h
	lda ba
	and #$F0
	ora mix
	tax
	lda nmult0, X
	clc
	adc out2
	;sta out2 ; h*b
	ldy sign
	cpy #1
	beq :+
		tax ; A = out2
		lda out1
		rts
	:
		; negate the result if sign parity did not match
		sta out2
		lda #0
		sec
		sbc out0
		;sta out0
		lda #0
		sbc out1
		sta out1
		lda #0
		sbc out2
		tax
		lda out1
		rts
	;

.segment "ALIGN"
.align 256
nmult0: ; ab = 4-bit a * b
	.repeat 256, I
		.byte (I&15) * (I>>4)
	.endrepeat
nmult4: ; ab = 4-bit a * b << 4
	.repeat 256, I
		.byte <(((I&15) * (I>>4)) << 4)
	.endrepeat
nmult8: ; ab = 4-bit a * b >> 4
	.repeat 256, I
		.byte  (((I&15) * (I>>4)) >> 4)
	.endrepeat
asl4: ; arithmetic shift left 4 bits
	.repeat 256, I
		.byte <(I<<4)
	.endrepeat
lsr4: ; logical shift right 4 bits
	.repeat 256, I
		.byte (I>>4)
	.endrepeat
.endproc

; =========
; Utilities
; =========

.export _prng
.export _prng1
.export _mouse_sense
.export _input_setup
.export _input_poll

.segment "CODE"

_prng:
	ldx #8
prngx:
	lda _seed+0
:
	asl
	rol _seed+1
	rol _seed+2
	bcc :+
	eor #$1B
:
	dex
	bne :--
	sta _seed+0
	;ldx #0 ; clear high bits of return value
	rts

_prng1:
	ldx #0
prng1:
	lda _seed+0
	asl
	rol _seed+1
	rol _seed+2
	bcc :+
	eor #$1B
:
	sta _seed+0
	rts

input_poll_raw:
	; strobe
	ldy #1
	sty $4016
	dey
	sty $4016
	; read 4 bytes each from 4 data lines
input_poll_raw_strobed:
	ldx #0
@poll_byte:
	ldy #8
	:
		lda $4016
		ror
		rol _input+0, X
		ror
		rol _input+2, X
		lda $4017
		ror
		rol _input+1, X
		ror
		rol _input+3, X
		dey
		bne :-
	inx
	inx
	inx
	inx
	cpx #16
	bcc @poll_byte
	rts

_mouse_sense:
	lda #1
	sta $4016
	lda $4016
	lda $4017
	lda #0
	sta $4016
	rts

_input_setup:
	; if present, mouse needs to be initialized by cycling the sensitivity
	jsr _mouse_sense
	; detect four-score:
	;   There are signatures in the 3rd byte, but I'm ignoring this for a simpler test:
	;   as long as the device reports $00 when not in use, assume no conflict.
	;   An unknown device might be able to affect the controls in a weird, but benign way,
	;   but conveniently this overlay works very well with the extra buttons on an SNES Pad.
	lda #$FF
	sta fourscore_off
	jsr input_poll_raw
	;lda _input+8
	;cmp #$10 ; signature
	;bne :+
	lda _input+4
	bne :+ ; no buttons pressed
		lda #$80
		eor fourscore_off
		sta fourscore_off
	:
	;lda _input+9
	;cmp #$20
	;bne :+
	lda _input+5
	bne :+
		lda #$40
		eor fourscore_off
		sta fourscore_off
	:
	; detect for mouse on all 4 lines:
	lda #4
	sta mouse_index ; 4 = no mouse detected
	ldx #3 ; count down from 3-0 so that lowest index mouse is used
	@mouse_detect:
		lda #0
		sta temp, X
		stx tmp1
		jsr input_poll_raw
		ldx tmp1
		;lda _input+0, X
		;bne @fail ; first byte is 0
		lda _input+4, X
		and #$0F
		cmp #1
		bne @fail ; missing signature
		lda _input+4, X
		lsr
		lsr
		lsr
		lsr
		and #3
		cmp #3
		bcs @fail ; invalid sensitivity value
		@pass:
			lda #1
			sta temp, X ; mouse detected
			stx mouse_index ; mouse selected
		@fail:
		dex
		cpx #4
		bcc @mouse_detect
	; attempt to cycle to medium sensitivity setting
	lda #4 ; maximum 4 attempts (3 should be sufficient, the 4th is for luck)
	sta tmp1
	:
		jsr _mouse_sense
		jsr _input_poll
		lda _mouse1
		and #$30
		cmp #$10 ; medium setting
		beq :+
		dec tmp1
		bne :-
	:
	rts

_input_poll:
	jsr input_poll_raw
	; combine gamepad inputs
	ldx #0
	stx _gamepad
	:
		lda _input, X
		cmp #$FF ; unplugged?
		beq :+
			ora _gamepad
			sta _gamepad
		:
		inx
		cpx #4
		bcc :--
	lda mouse_index
	cmp #4
	bcc @mouse
	@no_mouse:
		; no mouse leaves the possibility of four-score
		; (treated as if duplicates of ports 2/3 if not $FF)
		bit fourscore_off
		bmi :+
		lda _input+4
		cmp #$FF
		beq :+
			ora _gamepad
			sta _gamepad
		:
		bit fourscore_off
		bvs :+
		lda _input+5
		cmp #$FF
		beq :+
			ora _gamepad
			sta _gamepad
		:
		lda #0
		sta _mouse1
		sta _mouse2
		sta _mouse3
		rts
	@mouse:
		;lda mouse_index
		tax
		lda _input+4, X
		sta _mouse1
		lda _input+8, X
		bpl :+ ; convert signed magnitude to two's complement
			eor #$7F
			clc
			adc #1
		:
		sta _mouse2
		lda _input+12, X
		bpl :+
			eor #$7F
			clc
			adc #1
		:
		sta _mouse3
		rts
	;

; =====
; Sound
; =====

.export _sound_play

.segment "RODATA"
sound_silent: .byte $FE, $FF

rain_table:
.byte $F, $F, $F, $E, $E, $E, $E, $D

apu_init:
.byte %00110000 ; $4000 SQ0 volume/duty 0, disable length counter
.byte %01111111 ; $4001 SQ0 sweep disable
.byte %00000000 ; $4002 SQ0 frequency low
.byte %00001000 ; $4003 SQ0 frequency high, length counter
.byte %00110000 ; $4004 SQ1 volume/duty 0, disable length counter
.byte %01111111 ; $4005 SQ1 sweep disable
.byte %00000000 ; $4006 SQ1 frequency low
.byte %00001000 ; $4007 SQ1 frequency high, length counter
.byte %10000000 ; $4008 TRI halt
.byte %00000000 ; $4009 unused
.byte %00000000 ; $400A TRI frequency low
.byte %00001000 ; $400B TRI frequency high, length counter
.byte %00110000 ; $400C NSE volume 0, disable length counter
.byte %00000000 ; $400D unused
.byte %00000000 ; $400E NSE frequency, period
.byte %00001000 ; $400F NSE length counter

.macro APU_INIT addr
	.assert (addr >= $4000 && addr < $4010), error, "APU_INIT parameter range: $4000-$400F"
	lda apu_init + (addr - $4000)
	sta addr
.endmacro

.macro APU_COPY addr
	.assert (addr >= $4000 && addr < $4010), error, "APU_COPY parameter range: $4000-$400F"
	lda apu_out + (addr - $4000)
	sta addr
.endmacro

.macro APU_OUT_RESET addr
	.assert (addr >= $4000 && addr < $4010), error, "APU_OUT_RESET parameter range: $4000-$400F"
	lda apu_init + (addr - $4000)
	sta apu_out + (addr - $4000)
.endmacro

.segment "CODE"

sound_prng:
	lda sound_seed+0
	asl
	rol sound_seed+1
	bcc :+
		eor #$D7
	:
	sta sound_seed+0
	rts

rain_freq:
	jsr sound_prng
	and #7
	tax
	lda #$F
	sec
	sbc rain_table, X
	rts

snow_freq:
	ldx snow_time
	bne @finish
		; drunk walk from pitches $9-$F
		jsr sound_prng
		and #31
		clc
		adc #32
		tax ; X = snow_time
		ldy snow_pitch
		jsr sound_prng
		and #1
		beq @dec_pitch
		@inc_pitch:
			iny
			cpy #$10
			bcc :+
				ldy #$0E
			:
			jmp @end_pitch
		@dec_pitch:
			dey
			cpy #$09
			bcs :+
				ldy #$0A
			:
		@end_pitch:
		sty snow_pitch
	@finish:
	dex
	stx snow_time
	lda #$F
	sec
	sbc snow_pitch
	rts

_sound_play:
	; A:X = pointer to sound
	; called from main thread
	ldy sound+1
	bne :+ ; sound is already pending, not safe to rewrite
		sta sound+0
		stx sound+1
	:
	rts

sound_init:
	; initialize APU in a specific order
	lda #0
	sta $4015 ; silence/reset all channels
	APU_INIT $4000 ; constant volume 0 / halt all channels before enabling any
	APU_INIT $4004
	APU_INIT $400C
	APU_INIT $4008
	lda #%00001111
	sta $4015 ; turn on 4 channels (not DMC)
	APU_INIT $4001 ; disable sweep
	APU_INIT $4005
	APU_INIT $4002 ; low frequency
	APU_INIT $4006
	APU_INIT $400A
	APU_INIT $400E
	APU_INIT $4003 ; high frequency, length counter reload
	APU_INIT $4007
	APU_INIT $400B
	APU_INIT $400F
	ldx #0
	:
		lda apu_init, X
		sta apu_out, X ; store last-written value to apu_out
		inx
		cpx #$10
		bcc :-
	lda #0
	:
		sta $4000, X ; 0 to DMC registers
		inx
		cpx #$14
		bcc :-
	sta apu_sqh+0
	sta apu_sqh+1
	; initialize other variables
	sta snow_pitch
	sta snow_time
	lda #1
	sta sound_seed+0
	sta sound_seed+1
	lda #<sound_silent
	sta sound_ptr+0
	lda #>sound_silent
	sta sound_ptr+1
	rts

sound_update:
	; called from NMI
	; thread safe transfer of "sound" to "sound_ptr"
	lda sound+1
	beq :+
		sta sound_ptr+1
		lda sound+0
		sta sound_ptr+0
		lda #0 ; 0 in sound+1 signals safe to write again
		sta sound+0
		sta sound+1
	:
	; play sound
	ldy #0
	@sound_read:
		lda (sound_ptr), Y
		iny
		cmp #$FF ; end
		beq @sound_end
		cmp #$FE ; note-off
		beq @sound_note_off
		cmp #$FD ; next frame
		beq @sound_increment
	@sound_register:
		tax
		lda (sound_ptr), Y
		iny
		sta apu_out, X
		jmp @sound_read
	@sound_note_off:
		APU_OUT_RESET $4000
		APU_OUT_RESET $4004
		APU_OUT_RESET $4008
		APU_OUT_RESET $400C
		APU_OUT_RESET $4002
		APU_OUT_RESET $4006
		APU_OUT_RESET $400A
		APU_OUT_RESET $400E
		APU_OUT_RESET $4003
		APU_OUT_RESET $4007
		APU_OUT_RESET $400B
		APU_OUT_RESET $400F
		jmp @sound_read
	@sound_end:
		dey
	@sound_increment:
		tya
		clc
		adc sound_ptr+0
		sta sound_ptr+0
		bcc :+
			inc sound_ptr+1
		:
	; weather overlay
	lda _weather_rate_min
	beq sound_deliver_apu ; particles actively falling
	lda apu_out + $C
	and #$0F ; if existing SFX is using noise, skip this
	bne sound_deliver_apu
		lda apu_out + $E
		pha
		lda apu_out + $C
		pha
		lda #%00110001
		sta apu_out + $C ; volume 1
		lda _weather_tile
		cmp #$38 ; rain
		beq @rain
		@snow:
			;jsr snow_freq
			;jmp @apply_weather
			lda #%00110000
			sta apu_out + $C ; didn't like snow sounds, maybe silent is better
			jmp @apply_weather
		@rain:
			jsr sound_prng
			and #63
			clc
			adc #4
			cmp _weather_rate_min
			bcs :+
				lda #%00110000
				sta apu_out + $C
			:
			jsr rain_freq
		@apply_weather:
		sta apu_out + $E ; weather frequency
		jsr sound_deliver_apu
		pla
		sta apu_out + $C
		pla
		sta apu_out + $E
		rts
	;
sound_deliver_apu:
	;APU_COPY $400F
	APU_COPY $400E
	;APU_COPY $400D
	APU_COPY $400C
	APU_COPY $400B
	APU_COPY $400A
	;APU_COPY $4009
	APU_COPY $4008
	lda apu_out+7
	cmp apu_sqh+1 ; prevent phase reset
	beq :+
		sta $4007
		sta apu_sqh+1
	:
	APU_COPY $4006
	;APU_COPY $4005
	APU_COPY $4004
	lda apu_out+3
	cmp apu_sqh+0 ; prevent phase reset
	beq :+
		sta $4003
		sta apu_sqh+0
	:
	APU_COPY $4002
	;APU_COPY $4001
	APU_COPY $4000
	rts

; =====================
; NES hardware handling
; =====================

.export _ppu_latch
.export _ppu_direction
.export _ppu_write
.export _ppu_load
.export _ppu_fill
.export _ppu_ctrl
.export _ppu_mask
.export _ppu_scroll_x
.export _ppu_scroll_y
.export _ppu_post
.export _ppu_profile
.export _ppu_apply_direction
.export _ppu_apply

.import _main
.import popa

.segment "CODE"

_ppu_latch:
	bit $2002
	stx $2006
	sta $2006
	rts

_ppu_direction:
	asl
	asl
	and #%00000100
	pha
	lda ppu_2000
	and #%11111011
	sta ppu_2000
	pla
	ora ppu_2000
	sta ppu_2000
	rts

_ppu_write:
	sta $2007
	rts

_ppu_load:
	; A:X = count
	stx tmp1
	tax
	cpx #0
	beq @page
	; finish incomplete page first
	ldy #0
	:
		lda (_ptr), Y
		sta $2007
		iny
		dex
		bne :-
	tya
	clc
	adc _ptr+0
	sta _ptr+0
	lda #0
	adc _ptr+1
	sta _ptr+1
@page: ; remaining 256 byte pages
	;     X = 0
	txa ; A = 0
	cmp tmp1
	beq @done
	tay ; Y = 0, tmp1 = remaining page count
	:
		lda (_ptr), Y
		sta $2007
		iny
		bne :-
	inc _ptr+1
	dec tmp1
	jmp @page
@done:
	rts

_ppu_fill:
	; A:X = count
	; stack = fill value
	sta tmp1
	lda ppu_2000 ; set increment by 1 mode
	and #%11111011
	sta $2000
	jsr popa
	ldy tmp1
	cpy #0
	beq @no_partial
@page:
	:
		sta $2007
		dey
		bne :-
@no_partial:
	cpx #0
	beq @done
	dex
	ldy #0
	jmp @page
@done:
	rts

_ppu_ctrl:
	and #%00111000
	pha
	lda ppu_2000
	and #%11000111
	sta ppu_2000
	pla
	ora ppu_2000
	sta ppu_2000
	rts

_ppu_mask:
	sta ppu_2001
	rts

_ppu_scroll_x:
	; A:X = scroll
	sta ppu_2005x+0
	txa
	and #1
	sta ppu_2005x+1
	pha
	lda ppu_2000
	and #%11111110
	sta ppu_2000
	pla
	ora ppu_2000
	sta ppu_2000
	rts

_ppu_scroll_y:
	; A:X = scroll
	sta ppu_2005y+0
	txa
	and #1
	sta ppu_2005y+1
	asl
	pha
	lda ppu_2000
	and #%11111101
	sta ppu_2000
	pla
	ora ppu_2000
	sta ppu_2000
	rts

_ppu_post:
	sta ppu_post_mode
	:
		lda ppu_post_mode
		bne :-
	rts

_ppu_profile:
	ora ppu_2001
	sta $2001
	rts

_ppu_apply_direction:
	jsr _ppu_direction
	lda ppu_2000
	sta $2000
	rts

_ppu_apply:
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

nmi:
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

irq:
	rti

reset:
	; already done in reset_stub
	;sei       ; disable maskable interrupts
	;lda #0
	;sta $2000 ; disable non-maskable interrupt
	lda #0
	sta $2001 ; rendering off
	sta $4010 ; disable DMC IRQ
	sta $4015 ; disable APU sound
	lda #$40
	sta $4017 ; disable APU IRQ
	cld       ; disable decimal mode
	ldx #$FF
	txs       ; setup tack
	; wait for vblank #1
	bit $2002
	:
		bit $2002
		bpl :-
	; preserve PRNG seed (keeps randomness across reset)
	lda _seed+2
	pha
	lda _seed+1
	pha
	lda _seed+0
	pha
	lda #0 ; clear RAM
	tax
	:
		sta $0000, X
		;sta $0100, X ; don't clear stack yet
		sta $0200, X
		sta $0300, X
		sta $0400, X
		sta $0500, X
		sta $0600, X
		sta $0700, X
		inx
		bne :-
	; restore PRNG seed
	pla
	sta _seed+0
	pla
	sta _seed+1
	pla
	ora #$80 ; make sure at least 1 bit of seed is set
	sta _seed+2
	; clear stack, separately wipe OAM
	lda #0
	:
		sta $0100, X
		inx
		bne :-
	lda #$FF
	:
		sta _oam, X
		inx
		bne :-
	; wait for vblank #2
	:
		bit $2002
		bpl :-
	; initialize internal variables
	jsr sound_init
	lda #%00011110 ; No emphasis, no greyscale, BG and sprite shown, no hidden column
	sta ppu_2001
	lda #%10001000 ; NMI on, 8-pixel sprites, BG page 0, Sprite page 1
	sta ppu_2000
	sta $2000 ; turn NMI on permanently
	; initialize CC65 and enter main()
	jsr cc65_init
	jsr _main
	jmp ($FFFC)

; ==========
; CC65 setup
; ==========

; simplified version of cc65 libsrc/nes/crt0.s

.import copydata ; cc65 "DATA" segment setup
.importzp sp ; cc65 C stack pointer
.export __STARTUP__: absolute = 1

.segment "CODE"
cc65_init:
	jsr copydata
	lda #<(cstack + CSTACK_SIZE)
	sta sp+0
	lda #>(cstack + CSTACK_SIZE)
	sta sp+1
	rts

; =======
; NES ROM
; =======

.segment "HEADER"

INES_MAPPER     = 2 ; UNROM
INES_MIRROR     = 1 ; horizontal nametables
INES_PRG_16K    = 2 ; 32K
INES_CHR_8K     = 0
INES_BATTERY    = 0
INES2           = %00001000 ; NES 2.0 flag for bit 7
INES2_SUBMAPPER = 0
INES2_PRGRAM    = 0
INES2_PRGBAT    = 0
INES2_CHRRAM    = 7 ; 8K
INES2_CHRBAT    = 0
INES2_REGION    = 2 ; 0=NTSC, 1=PAL, 2=Dual

; iNES 1 header
.byte 'N', 'E', 'S', $1A ; ID
.byte <INES_PRG_16K
.byte INES_CHR_8K
.byte INES_MIRROR | (INES_BATTERY << 1) | ((INES_MAPPER & $f) << 4)
.byte (<INES_MAPPER & %11110000) | INES2
; iNES 2 section
.byte (INES2_SUBMAPPER << 4) | (INES_MAPPER>>8)
.byte ((INES_CHR_8K >> 8) << 4) | (INES_PRG_16K >> 8)
.byte (INES2_PRGBAT << 4) | INES2_PRGRAM
.byte (INES2_CHRBAT << 4) | INES2_CHRRAM
.byte INES2_REGION
.byte $00 ; VS system
.byte $00, $00 ; padding/reserved
.assert * = 16, error, "NES header must be 16 bytes."

.segment "STUB"
reset_stub:
	sei       ; disable maskable interrupts
	lda #0
	sta $2000 ; disable non-maskable interrupt
	sta @zero ; setup up UxROM bank 0
	jmp reset
	@zero: .byte 0

.segment "VECTORS"
.word nmi
.word reset_stub
.word irq

; end of file
