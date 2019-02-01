;
; blend.s
; arbitrary 5-step colour blend for NES palettes
; http://rainwarrior.ca
;

; uint8 blend50(uint8 a, uint8 b); // 50% a, 50% b
; uint8 blend25(uint8 a, uint8 b); // 75% a, 25% b
.export _blend50
.export _blend25

; Intended for a 4-step fade from one colour to any other:
;   1. a
;   2. blend25(a,b)
;   3. blend50(a,b)
;   4. blend25(b,a)
;   5. b
;
; blends from colour to colour will take the shortest path of hue rotation
; blends against white or black will progress vertically without hue change
; blends from grey to a colour will immediately take on the colour's hue
;
; a,b must be 0-63, input is not sanitized
; fastcall (default) calling convention is required

;
; Implementation:
;

.import popa ; used to retrieve argument from CC65 C-stack (A=result, Y=0, sp+=1)
.importzp tmp1, tmp2, tmp3, tmp4 ; CC65 ZP temporaries
hue0 = tmp1
hue1 = tmp2
val0 = tmp3
val1 = tmp4

; "normalized" colour palette
; low nybble 1-12 is hue, 0 for greys
; high nybble is intensity
;   for colours: intensity is shifted down a row as 1-4, 0 becomes black, and 5 becomes white
;   for greys: intensity 0-5 is monotonically increasing from 0F black to 30 white (including 2D/3D)
palette_normalize:
.byte $20, $11, $12, $13, $14, $15, $16, $17, $18, $19, $1A, $1B, $1C, $00, $00, $00
.byte $30, $21, $22, $23, $24, $25, $26, $27, $28, $29, $2A, $2B, $2C, $00, $00, $00
.byte $50, $31, $32, $33, $34, $35, $36, $37, $38, $39, $3A, $3B, $3C, $10, $00, $00
.byte $50, $41, $42, $43, $44, $45, $46, $47, $48, $49, $4A, $4B, $4C, $40, $00, $00

; reverse of normalize, returns to original colours
palette_denormalize:
.byte $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
.byte $2D, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $2D, $2D, $2D
.byte $00, $11, $12, $13, $14, $15, $16, $17, $18, $19, $1A, $1B, $1C, $00, $00, $00
.byte $10, $21, $22, $23, $24, $25, $26, $27, $28, $29, $2A, $2B, $2C, $10, $10, $10
.byte $3D, $31, $32, $33, $34, $35, $36, $37, $38, $39, $3A, $3B, $3C, $3D, $3D, $3D
.byte $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30

blend_common:
	; A = color 1
	; stack = color 0
	tax
	lda palette_normalize, X
	and #$F
	sta hue1
	lda palette_normalize, X
	and #$F0
	sta val1
	jsr popa
	tax
	lda palette_normalize, X
	and #$F
	sta hue0
	lda palette_normalize, X
	and #$F0
	sta val0
	rts

.proc _blend50
	; A = color 1
	; stack = color 0
	jsr blend_common
	; blend value 50%
	lda val0
	clc
	adc val1
	lsr
	and #$F0
	sta val0
	; blend hue
	lda hue0
	beq grey0
	lda hue1
	beq grey1
colours: ; both are colours, blend using shortest path around hue ring
	sec
	sbc hue0
	bcc @hue0_high
@hue1_high:
	cmp #7
	bcs @hue_blend_wrapped
	@hue_blend_straight:
		lda hue0
		clc
		adc hue1
		lsr
		jmp hue
	@hue_blend_wrapped:
		lda #12
		clc
		adc hue0
		;clc
		adc hue1
		lsr
		cmp #13
		bcc hue
		;sec
		sbc #12
		jmp hue
	;
@hue0_high:
	cmp #(256-6)
	bcs @hue_blend_straight
	jmp @hue_blend_wrapped
grey0: ; if either is grey, favour keeping colour
	lda hue1
	jmp hue
grey1:
	lda hue0
	;jmp @hue
hue:
	; val0 = val << 4
	; A = hue
	ora val0
	tax
	lda palette_denormalize, X
	rts
.endproc

.proc _blend25
	; A = color 1
	; stack = color 0
	jsr blend_common
	; blend value 75% vs 25%
	lda val0
	lsr val0
	lsr val1
	;clc
	adc val0
	;clc
	adc val1
	lsr
	;clc
	adc #$07 ; round to nearest
	and #$F0
	sta val0
	; blend hue
	lda hue0
	beq grey0
	lda hue1
	beq grey1
colours: ; both are colours, blend using shortest path around hue ring
	sec
	sbc hue0
	bcc @hue0_high
@hue1_high:
	cmp #7
	bcs @hue0_blend_wrapped
	@hue_blend_straight:
		lda hue0
		asl
		;clc
		adc hue0
		; unoptimized:
		;clc
		;adc hue1
		;clc
		;adc #1 ; round
		; optimized:
		sec ; round
		adc hue1
		;
		lsr
		lsr ; ((hue0*3)+hue1)/4
		jmp hue
	@hue0_blend_wrapped:
		lda hue0
		asl
		;clc
		adc hue0
		;clc
		adc #(12*3) + 1
		;clc
		adc hue1
		lsr
		lsr ;(((hue0+12)*3)+hue1)/4
		cmp #13
		bcc hue
		;sec
		sbc #12
		jmp hue
	;
@hue0_high:
	cmp #(256-6)
	bcs @hue_blend_straight
	@hue1_blend_wrapped:
		lda hue0
		asl
		;clc
		adc hue0
		;clc
		adc hue1
		;clc
		adc #12 + 1
		lsr
		lsr ;((huge0*3)+(hue1+12))/4
		cmp #13
		bcc hue
		;sec
		sbc #12
		jmp hue
	;
grey0: ; if either is grey, favour keeping colour
	lda hue1
	jmp hue
grey1:
	lda hue0
	;jmp @hue
hue:
	; val0 = val << 4
	; A = hue
	ora val0
	tax
	lda palette_denormalize, X
	rts
.endproc

;
; http://rainwarrior.ca
;
