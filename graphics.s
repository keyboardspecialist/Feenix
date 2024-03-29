;handy gfx macros
macro load_palette pal
	lda PPUSTATUS	;read ppu status, reset hi/lo latch
	lda #$3F
	sta PPUADDR		;write hi byte of addr
	lda #$00
	sta PPUADDR		;write lo byte
	ldx #$00
-p	lda pal, x 		;load palette byte
	sta PPUDATA		;write to ppu
	inx
	cpx #$20		;write 32 bytes
	bne -p
endm

;offsets beginning at $0200
macro load_sprite_4 spr, offset
	ldx #$00 ;reset y for our loop
-s	lda spr, x
	sta offset, x
	inx
	cpx #$10
	bne -s
endm

;_nt_data_ptr is address to nametable data, _nt is which table to write to
macro load_nametable _nt_data_ptr, _nt
	lda #<_nt_data_ptr
	sta nt_data_lo
	lda #>_nt_data_ptr
	sta nt_data_hi
	
	lda #_nt
	sta nametable
	lda #$1D
	sta row_num
-	jsr DrawNewRow	;draw bg column
	dec row_num
	bpl -
endm

macro load_attribute _atr_data_ptr, _nt
	lda #<_atr_data_ptr
	sta atr_data_lo
	lda #>_atr_data_ptr
	sta atr_data_hi
	
	lda #_nt
	sta nametable
	lda #$00
	sta row_num
-a	jsr DrawNewAttributes	;draw attributes
	
	lda row_num
	clc
	adc #$04	
	sta row_num
	cmp #$20
	bne -a
endm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;	GRAPHICS ROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DrawNewRow:
	lda row_num
	mul_32_16bit row_hi	;mul row_num by 32 and store in row_hi/A
;	sec
;	sbc #$20
	sta row_lo			;store row_lo-1 to get first offscreen row
	
	
	lda nametable	;calc new row addr using current nametable
;	eor #%00000010		;invert nt
	asl_2			;shift up, A = $00 or $02		;$00 or $04
	clc 
	adc #$20		;add hi byte of nametable addr ($2000)
	adc row_hi
	sta row_hi		;new addr = $20 or $28 
	
	lda row_num		;row number * 32 = row data offset
	asl_5
;	sec
;	sbc #$20
	sta source_lo
	lda row_num
	lsr_3
	sta source_hi
	
	lda source_lo	;row data start + offs = addr to load row data from
	clc
	adc nt_data_lo
	sta source_lo
	lda source_hi
	adc nt_data_hi
	sta source_hi
	
DrawRow:	
	lda PPUSTATUS	;reset latch
	lda row_hi
	sta PPUADDR
	lda row_lo
	sta PPUADDR
	ldx #$20		;32
	ldy #$00
DrawRowLoop:
	lda (source_lo), y
	sta PPUDATA
	iny
	dex
	bne DrawRowLoop
	
	rts 
	
	
DrawNewAttributes:
	lda nametable
;	eor #%00000010		;invert nt bit
	asl_2			;shift, $00 or $08
	clc 
	adc #$23		;add hi byte of attribute base addr $23C0
	sta row_hi		;new addr = $23 or $2B 
	
	lda row_num
	asl				;row * 2
	and #$F8		;mask 3 low bits for 8 byte boundary
	sta source_lo
	clc
	adc #$C0		;lo addr byte
	sta row_lo		
	
	lda #$00
	sta source_hi 
	
	lda source_lo	;row data start + offset = addr to load row data from 
	clc
	adc atr_data_lo
	sta source_lo
	lda source_hi
	clc
	adc atr_data_hi
	sta source_hi
	
	ldy #$00
	lda PPUSTATUS		;reset latch 
	lda row_hi
	sta PPUADDR			;write the hi byte of row addr
	lda row_lo	
	sta PPUADDR			;write lo byte
DrawNewAttributesLoop:
	lda (source_lo), y	
	sta PPUDATA			;write attribute byte
	iny
	cpy #$08
	bne DrawNewAttributesLoop
DrawNewAttributesLoopDone:

	rts