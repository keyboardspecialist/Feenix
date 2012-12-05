;6502 math routines
enum $0000
	div_num 	dsb 1
	div_denom 	dsb 1
ende

;8bit division with 8bit result with denominator
;registers affected: A, X
macro div_8bit n, d
	lda #n
	sta div_num
	lda #d
	sta div_denom
	lda #$00
	ldx #$07
	clc
--	rol div_num
	rol
	cmp div_denom
	bcc +
	sbc div_denom
+	dex
	bpl --
	rol div_num
endm

;not the fastest, consider lookup table
macro mul_32_16bit m, l
	lda #$00
	sta m
	lda l
	asl a
	rol m
	asl a
	rol m
	asl a
	rol m
	asl a
	rol m
	asl a
	rol m
endm

macro asl_2
	asl a
	asl a
endm

macro asl_3
	asl a
	asl a
	asl a
endm

macro asl_4
	asl a
	asl a
	asl a
	asl a
endm

macro asl_5
	asl a
	asl a
	asl a
	asl a
	asl a
endm

macro asl_6
	asl a 
	asl a
	asl a
	asl a
	asl a
	asl a
endm

macro asl_7
	asl a 
	asl a
	asl a
	asl a
	asl a
	asl a
	asl a
endm


macro lsr_2
	lsr a
	lsr a
endm

macro lsr_3
	lsr a
	lsr a
	lsr a
endm

macro lsr_4
	lsr a
	lsr a
	lsr a
	lsr a
endm

macro lsr_5
	lsr a
	lsr a
	lsr a
	lsr a
	lsr a
endm

macro lsr_6
	lsr a
	lsr a
	lsr a
	lsr a
	lsr a
	lsr a
endm

macro lsr_7
	lsr a
	lsr a
	lsr a
	lsr a
	lsr a
	lsr a
	lsr a
endm