; Lizard NES SPC
; ==============
;
; Brad Smith, 2022
; https://rainwarrior.ca
;
; SPC simulator of NES APU
;
; First initialization:
;   After uploading to SPC and running at $0200, wait for the IO1-3 registers to read 17,18,19.
;   Write 16 to IO0 to set off a first frame and wait for echo on IO0. Will later read 17 on IO1 when ready.
;
; To reset, write $FF to IO0, control will return to SPC BIOS loader.
;   No acknowledgement will be written to IO0 in this case. Registers will be cleared before re-entering the BIOS loader.
;
; Each frame:
;   1. Wait for IO1 to read as 17.
;   2. Write a new value to IO3.
;   3. Write xx = 0-15 to IO0 to send that value to NES APU $40xx.
;   4. Wait for IO0 read to echo the IO0 write to confirm.
;   5. Repeat 2-4 for each NES APU register to update.
;   6. Write 16 to IO0 to let the SPC process the frame, go and do something else for a while and return to 1.
;
; If a frame is not sent for more than 10 seconds, a watchdog timer will reset the SPC program to the BIOS IPL.
; This is to prevent the sound from hanging forever if there is some interruption in the CPU side program.
; This can be disabled by defining WATCHDOG as 0 below.
;
; Doesn't support:
; - DPCM
; - Enevelopes / length counter / etc.
; - Sweep unit
; - $4015 bits clear will temporarily mute a channel, but they do not clear the length counter
; - Triangle must be silenced/unsilenced with $4008 (or $4015)
;
; Channels:
;   0 = square 0
;   1 = square 1
;   2 = triangle
;   3 = noise: SNES hardware noise or sample-based periodic noise
;   4 = noise complement: sample based complement to SNES hardware noise (fills in low-end of spectrum)
; Each channel has several possible samples selected by "octave".
;   Actual selection is based on arbitrary pitch ranges, not necessarily at octaves.
;   A change of octave will require a sample restart (KON).

.define WATCHDOG 1

.memorymap
DEFAULTSLOT 0
SLOTSIZE $FDC0
SLOT 0 $0200
.endme

.rombankmap
BANKSTOTAL 1
BANKSIZE $FDC0
BANKS 1
.endro

.enum $00
	nes_apu:    DSB $20 ; last NES APU state received
	old_vol:    DSB 4   ; current gain
	old_oct:    DSB 4   ; current sample "octave" selection
	old_att:    DSB 2   ; current atten (noise)
	new_vol:    DSB 4   ; next gain to apply
	new_oct:    DSB 4
	new_att:    DSB 2
	new_pit0:   DSB 5   ; next pitch
	new_pit1:   DSB 5
	new_src:    DSB 5   ; next src
	new_flg:    DB
	new_non:    DB
	ptr:        DW      ; multi-purpose pointer
	.if WATCHDOG
	watchdog:   DSB 2   ; timeout watchdog
	.endif
.ende

.bank 0 slot 0
.org 0

;
; $0200 entry point and command processing loop
;

start:
	call !do_init
	; initialized
	mov $F7, #19
	mov $F6, #18
	mov $F5, #17 ; 17 = ready for commands
	mov Y, #0
loop:
	cmp X, $F4 ; CPUIO0 = new command issue (value may be unstable on first cycle of change)
	.if !WATCHDOG
		beq loop
	.else
		bne command
		inc Y
		bne loop
		inc watchdog+0
		bne loop
		inc watchdog+1
		cmp watchdog+1, #$0F ; ~10 second timeout for watchdog
		bcc loop
		bra reset
	command:
	.endif
	mov X, $F4 ; second read will be stable
	cmp X, #$20
	bcs frame ; done receiving commands for now, execute frame
	; else X = 00-1F, APU writes
	mov A, $F7 ; CPUIO3 = value to write (written before CPUIO0, stable now)
	mov nes_apu+X, A
	; acknowledge by echoing to CPUIO0
	mov $F4, X
	bra loop
frame:
	cmp X, #$FF
	bcs reset
	mov $F5, #0 ; 0 = not-ready
	mov $F4, X ; acknowledge, now that not-ready is signalled
	call !do_frame
	mov $F5, #17 ; 17 = ready for commands
	mov X, #$20
	.if WATCHDOG
		mov Y, #0 ; reset watchdog timer
		mov watchdog+0, Y
		mov watchdog+1, Y
	.endif
	bra loop
reset:
	di ; redundant interrupt disable
	; soft reset DSP
	mov $F2, #$6C ; FLG
	mov $F3, #%11100000 ; soft reset, mute, echo disable
	; clear registers and return to BIOS loader
	mov $F1, #%10110000
	mov $F4, #0
	mov $F5, #0
	mov $F6, #0
	mov $F7, #0
	jmp !$FFC0

;
; initialize variables and all sound registers
;

do_init:
	; soft reset DSP
	mov $F2, #$6C ; FLG
	mov $F3, #%11100000 ; soft reset, mute, echo disable
	; clear input registers
	mov $F1, #%10110000
	mov $F4, #0
	; clear the BIOS' load address to finish erasing ZP
	mov $00, #0
	mov $01, #0
	; set initial state
	mov X, #0
@reginit:
	mov A, !regs_init+X
	bmi +
	mov $F2, A
	mov A, !regs_init+1+X
	mov $F3, A
	inc X
	inc X
	bra @reginit
+
	ret

regs_init:
	.db $6C, %01100000 ; FLG soft-reset off, mute, echo disable
	.db $00, VOL_SQU, $01, VOL_SQU ; main volume (attenuation) for voices 0-4
	.db $10, VOL_SQU, $11, VOL_SQU
	.db $20, VOL_TRI, $21, VOL_TRI
	.db $30, VOL_NSE, $31, VOL_NSE
	.db $40, VOL_NSE, $41, VOL_NSE
	.db $50,   0, $51,   0 ; main volume 0 for voices 5-7
	.db $60,   0, $61,   0
	.db $70,   0, $71,   0
	.db $02,   0, $03,   0 ; P 0 voices 0-4
	.db $12,   0, $13,   0
	.db $22,   0, $23,   0
	.db $32,   0, $33,   0
	.db $42,   0, $43,   0
	.db $05, $00           ; ADSR set to gain only voices 0-4
	.db $15, $00
	.db $25, $00
	.db $35, $00
	.db $45, $00
	.db $07,   0           ; GAIN set to 0 for voices 0-4
	.db $17,   0
	.db $27,   0
	.db $37,   0
	.db $47,   0
	.db $5D, >src_directory
	.db $04, SRC_SQU       ; SRCN for each voice
	.db $14, SRC_SQU
	.db $24, SRC_TRI
	.db $34, SRC_NSP
	.db $44, SRC_NSC
	.db $0C, $7F, $1C, $7F ; MVOL main volume full
	.db $2C,   0, $3C,   0 ; EVOL echo volume 0
	.db $2D,   0           ; PMON pitch modulation off
	.db $3D, %00001000     ; NON noise on voice 3
	.db $4D,   0           ; EON echo off all voices
	.db $4C, %00011111     ; KON voices 0-4
	.db $6C, %00111111 ; FLG unmute, echo disable, noise pitch highest
	.db $FF

;
; translate nes_apu changes into SNES sound registers
;

do_frame:
	; To test muting:
	;mov A, nes_apu+$15
	;and A, #%11111111
	;mov nes_apu+$15, A
	;
	; calculate new gain
	;
	mov A, nes_apu+$00 ; square 0
	and A, #$0F
	asl A
	asl A
	asl A
	mov new_vol+0, A
	mov A, nes_apu+$04 ; square 1
	and A, #$0F
	asl A
	asl A
	asl A
	mov new_vol+1, A
	mov A, nes_apu+$08 ; triangle (assume $4008 is used to turn it on/off)
	and A, #$7F
	beq +
	mov A, #$7F
+
	mov new_vol+2, A
	mov A, nes_apu+$0C ; noise
	and A, #$0F
	asl A
	asl A
	asl A
	mov new_vol+3, A
	; recalculate muting via $4015
	mov A, nes_apu+$15
	and A, #%00000001
	bne +
	mov new_vol+0, #0
+
	mov A, nes_apu+$15
	and A, #%00000010
	bne +
	mov new_vol+1, #0
+
	mov A, nes_apu+$15
	and A, #%00000100
	bne +
	mov new_vol+2, #0
+
	mov A, nes_apu+$15
	and A, #%00001000
	bne +
	mov new_vol+3, #0
+
	;
	; calculate new pitch/octave/source/noise-attenuation
	;
	; square 0
	mov Y, nes_apu+$02
	mov A, nes_apu+$03
	mov X, #0
	call !new_pitch_squ
	mov A, nes_apu+$00
	and A, #$C0 ; duty
	asl A
	rol A
	rol A
	mov X, A
	mov A, !src_squ+X
	clrc
	adc A, new_oct+0
	mov new_src+0, A
	; square 1
	mov Y, nes_apu+$06
	mov A, nes_apu+$07
	mov X, #1
	call !new_pitch_squ
	mov A, nes_apu+$04
	and A, #$C0
	asl A
	rol A
	rol A
	mov X, A
	mov A, !src_squ+X
	clrc
	adc A, new_oct+1
	mov new_src+1, A
	; triangle
	mov Y, nes_apu+$0A
	mov A, nes_apu+$0B
	mov X, #2
	call !new_pitch_squ
	mov A, #SRC_TRI
	clrc
	adc A, new_oct+2
	mov new_src+2, A
	; noise
	mov A, nes_apu+$0E
	and A, #$0F
	mov X, A
	mov A, !flg_nse+X
	or A, #%00100000 ; echo disable
	mov new_flg, A
	mov A, X
	asl A
	mov X, A
	mov A, !pitch_nsc+0+X ; pitch for complementary noise sample
	mov new_pit0+4, A
	mov A, !pitch_nsc+1+X
	mov new_pit1+4, A
	mov A, nes_apu+$0E
	and A, #$80
	bne + ; regular noise
		mov A, !atten_nse+1+X ; hardware noise has lower volume than sample noise
		mov new_att+0, A
		mov A, !atten_nse+0+x
		mov new_att+1, A
		mov new_non, #%00001000
		bra ++
	+ ; periodic noise
		mov A, !atten_nse+0+X
		mov new_att+0, A
		mov new_att+1, #0 ; mute complementary noise for periodic noise sample
		mov new_non, #0 ; switch to sample instead of hardware noise
	++
	mov A, X
	asl A
	mov X, A
	mov A, !pitch_sample_nsp+0+X ; pitch/octave for periodic noise sample
	mov new_pit0+3, A
	mov A, !pitch_sample_nsp+1+X
	mov new_pit1+3, A
	mov A, !pitch_sample_nsp+2+X
	mov new_oct+3, A
	clrc
	adc A, #SRC_NSP
	mov new_src+3, A
	;
	; update channels
	;
	; square 0
	mov Y, new_vol+0
	cmp Y, old_vol+0
	bcs + ; if gain down: do it first
		mov A, #$07
		movw $F2, YA ; GAIN
	+
	mov Y, new_pit0+0
	mov A, #$02
	movw $F2, YA ; P
	mov Y, new_pit1+0
	inc A
	movw $F2, YA
	mov Y, new_src+0
	inc A
	movw $F2, YA ; SRC ($04)
	mov Y, new_oct+0
	cmp Y, old_oct+0
	beq +
		mov Y, #%00000001
		mov A, #$4C
		movw $F2, YA ; KON if octave changed
	+
	mov Y, new_vol+0 ; final gain
	mov A, #$07
	movw $F2, YA ; GAIN
	; square 1
	mov Y, new_vol+1
	cmp Y, old_vol+1
	bcs +
		mov A, #$17
		movw $F2, YA ; GAIN
	+
	mov Y, new_pit0+1
	mov A, #$12
	movw $F2, YA ; P
	mov Y, new_pit1+1
	inc A
	movw $F2, YA
	mov Y, new_src+1
	inc A
	movw $F2, YA ; SRC ($14)
	mov Y, new_oct+1
	cmp Y, old_oct+1
	beq +
		mov Y, #%00000010
		mov A, #$4C
		movw $F2, YA ; KON
	+
	mov Y, new_vol+1
	mov A, #$17
	movw $F2, YA ; GAIN
	; triangle
	mov Y, new_vol+2
	bne +
		mov Y, #$BF
		mov A, #$27
		movw $F2, YA ; GAIN ramp down first
	+
	mov Y, new_pit0+2
	mov A, #$22
	movw $F2, YA ; P
	mov Y, new_pit1+2
	inc A
	movw $F2, YA
	mov Y, new_src+2
	inc A
	movw $F2, YA ; SRC ($24)
	mov Y, new_oct+2
	cmp Y, old_oct+2
	beq +
		mov Y, #%00000100
		mov A, #$4C
		movw $F2, YA ; KON
	+
	mov Y, new_vol+2
	beq +
		mov Y, #$DF
		mov A, #$27
		movw $F2, YA ; GAIN ramp up last
	+
	; noise
	mov Y, new_vol+3
	cmp Y, old_vol+3
	bcs +
		mov A, #$37
		movw $F2, YA ; GAIN down first
		mov A, #$47
		movw $F2, YA ; GAIN
	+
	mov Y, new_att+0
	cmp Y, new_att+0
	bcs +
		mov A, #$30
		movw $F2, YA ; VOL down first
		inc A
		movw $F2, YA
	+
	mov Y, new_att+1
	cmp Y, new_att+1
	bcs +
		mov A, #$40
		movw $F2, YA ; VOL
		inc A
		movw $F2, YA
	+
	mov Y, new_pit0+3
	mov A, #$32
	movw $F2, YA ; P
	mov Y, new_pit1+3
	inc A
	movw $F2, YA
	mov Y, new_src+3
	inc A
	movw $F2, YA ; SRC ($34)
	mov Y, new_pit0+4
	mov A, #$42
	movw $F2, YA ; P
	mov Y, new_pit1+4
	inc A
	movw $F2, YA
	mov Y, new_oct+3
	cmp Y, old_oct+3
	beq +
		mov Y, #%00001000
		mov A, #$4C
		movw $F2, YA ; KON if octave changed
	+
	mov Y, new_flg
	mov A, #$6C
	movw $F2, YA ; FLG
	mov Y, new_non
	mov A, #$3D
	movw $F2, YA ; NON
	mov Y, new_vol+3
	mov A, #$37
	movw $F2, YA ; GAIN
	mov A, #$47
	movw $F2, YA ; GAIN
	mov Y, new_att+0
	mov A, #$30
	movw $F2, YA ; VOL
	inc A
	movw $F2, YA ; VOL
	mov Y, new_att+1
	mov A, #$40
	movw $F2, YA ; VOL
	inc A
	movw $F2, YA ; VOL
	;
	; copy new to old
	;
	mov A, new_vol+0
	mov old_vol+0, A
	mov A, new_vol+1
	mov old_vol+1, A
	mov A, new_vol+2
	mov old_vol+2, A
	mov A, new_vol+3
	mov old_vol+3, A
	mov A, new_oct+0
	mov old_oct+0, A
	mov A, new_oct+1
	mov old_oct+1, A
	mov A, new_oct+2
	mov old_oct+2, A
	mov A, new_oct+3
	mov old_oct+3, A
	mov A, new_att+0
	mov old_att+0, A
	mov A, new_att+1
	mov old_att+1, A
	ret

new_pitch_squ:
	; updates new_pit0/1 + new_oct
	; A:Y = NES pitch (high 5 bits ignored), X = channel, clobbers A/Y
	and A, #$07
	mov ptr+1, A
	mov A, Y
	asl A
	rol ptr+1
	asl A
	rol ptr+1
	;clrc
	adc A, #<pitch_sample_squ
	mov ptr+0, A
	mov A, ptr+1
	adc A, #>pitch_sample_squ
	mov ptr+1, A
	mov Y, #0
	mov A, [ptr]+Y
	mov new_pit0+X, A
	inc Y
	mov A, [ptr]+Y
	mov new_pit1+X, A
	inc Y
	mov A, [ptr]+Y
	mov new_oct+X, A
	ret

; generated tables
.include "snes.spc.tab.inc"
