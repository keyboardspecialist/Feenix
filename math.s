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
macro mul_32_16bit m
	pha
	lda #$00
	sta m
	pla
	asl 
	rol m
	asl 
	rol m
	asl 
	rol m
	asl 
	rol m
	asl 
	rol m
endm

macro asl_2
	asl 
	asl 
endm

macro asl_3
	asl 
	asl 
	asl 
endm

macro asl_4
	asl 
	asl 
	asl 
	asl 
endm

macro asl_5
	asl 
	asl 
	asl 
	asl 
	asl 
endm

macro asl_6
	asl
	asl
	asl
	asl
	asl
	asl
endm

macro asl_7
	asl
	asl
	asl
	asl
	asl
	asl
	asl
endm


macro lsr_2
	lsr
	lsr
endm

macro lsr_3
	lsr
	lsr
	lsr
endm

macro lsr_4
	lsr
	lsr
	lsr
	lsr
endm

macro lsr_5
	lsr
	lsr
	lsr
	lsr
	lsr
endm

macro lsr_6
	lsr
	lsr
	lsr
	lsr
	lsr
	lsr
endm

macro lsr_7
	lsr
	lsr
	lsr
	lsr
	lsr
	lsr
	lsr
endm