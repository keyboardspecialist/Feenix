;; Feenix NES
;; Remake of the infamous Phoenix 89 clone
;;
;; Jason Sobotka
;	header defines
	PRG_COUNT = 1 
	CHR_COUNT = 1
	MIRRORING = 0001b	;horz


;	Macro defines
macro push_regs
	pha
	txa
	pha
	tya
	pha
endm

macro pull_regs
	pla
	tay
	pla
	tax
	pla
endm


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	db "NES", 1Ah		;NES header, standard NROM
	db PRG_COUNT		;16KB PRG ROM
	db CHR_COUNT		;8KB CHR ROM
	db $00|MIRRORING 	;mapper 0, horz mirror
	dsb 9, $00			;pad
	
	include "feenix\math.s"
	include "feenix\nes.s"
	
;	temp and local vars, $00-$0F
	enum $0000
	mul_temp		dsb 1	;temp for quick multiply by constant
	ende
	
;	global mem vars
	enum $0010
	scroll			dsb 1	;per row scroll val
	scroll_pages	dsb 1	;per screen scroll val
	scroll_flag		dsb 1	;flag when we're ready to scroll
	nametable		dsb 1	;current nametable
	row_lo			dsb 1	;row lo address
	row_hi			dsb 1	;row hi address
	source_lo		dsb 1	;data lo address
	source_hi		dsb 1	;data hi address
	row_num			dsb 1	;which row to draw
	pad_state		dsb 1	;controller pad state
	p1_x			dsb 1	;player 1 X location
	p1_y			dsb 1	;player 1 Y location
	num_bullets		dsb 1	;number of fired bullets on screen
	vram_update_ready	dsb 1	;notify nmi that its ready to draw
	ende
	
	SCROLL_SPD = 01h
	PLAYER_SPD = 02h
	
	BULLET_MEM_OFFS = 10h	;starting offset for bullets in sprite mem
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;first code bank
	.base $10000-(PRG_COUNT*$4000)
	
;	standard system reset code	
RESET:
	sei	;disable IRQs
	cld	;disable decimal mode, 2A03 doesn't have it
	ldx #$40
	stx $4017	;disable apu frame irq
	ldx #$FF
	txs		;setup stack
	inx		;X = 0
	stx PPUCTRL ;disable NMI
	stx PPUMASK ;disable rendering
	stx $4010 ;disable DMC IRQs
	
;	wait for PPU to fire up
vblankwait1:
	bit PPUSTATUS
	bpl vblankwait1

;	prep our memory while waiting
;	clear to $00, sprite memory $FE
clrmem:
	lda #$00
	sta $0000, x
	sta $0100, x
	sta $0300, x
	sta $0400, x
	sta $0500, x
	sta $0600, x
	sta $0700, x
	lda #$FE
	sta $0200, x ;move sprites off screen
	inx
	bne clrmem
	
;	more waiting
vblankwait2:
	bit PPUSTATUS
	bpl vblankwait2
	

loadPalettes:
	lda PPUSTATUS	;read ppu status, reset hi/lo latch
	lda #$3F
	sta PPUADDR	;write hi byte of addr
	lda #$00
	sta PPUADDR	;write lo byte
	ldx #$00
loadPalettesLoop:
	lda palette, x ;load palette byte
	sta PPUDATA		;write to ppu
	inx
	cpx #$20		;write 32 bytes
	bne loadPalettesLoop
	
spriteLoad:
	ldx #$00 ;reset x for our loop
spritesLoop:
	lda p1_ship, x
	sta $0200, x
	inx
	cpx #$10
	bne spritesLoop
	
initNametables:
	lda #$01
	sta nametable
	lda #$00
	sta scroll
	sta row_num
initNametablesLoop:
	jsr DrawNewRow	;draw bg column
	inc row_num
	lda row_num		;repeat for first nametable
	cmp #$1E		;30
	bne initNametablesLoop
	
	lda nametable
	beq initNametablesDone
	
	lda #$00
	sta nametable
	sta row_num
	sta scroll
	jmp initNametablesLoop	;loop again to fill second name table
initNametablesDone:

initAttributes:
	lda #$01
	sta nametable
	lda #$00
	sta scroll
	sta row_num
initAttrLoop:
	jsr DrawNewAttributes	;draw attributes
	lda scroll				;go to next row
	clc
	adc #$20
	sta scroll
	
	lda row_num			;repeat for first nametable
	clc
	adc #$04
	sta row_num
	cmp #$20
	bne initAttrLoop
	
	lda #$01
	sta nametable
	lda #$EF		
	sta scroll
	jsr DrawNewAttributes
initAttributesDone:
	
	lda #$1D
	sta row_num
	
	lda #%10010000 ;enable NMI, sprites from pattern table 0, bg from table 1
	sta PPUCTRL
	
	lda #%00011110 ;enable sprites, enable background, no clipping on left side
	sta PPUMASK
	
	lda #$01
	sta scroll_flag
	lda #$00
	sta pad_state
	lda #$80
	sta p1_x
	sta p1_y
	
_mainCPUThread:
	jmp _mainCPUThread
	
	
	
	
;;;	START NMI ;;;;;;
;;;;;;;;;;;;;;;;;;;;

NMI:
	push_regs
	
	lda scroll_flag
	and #%00000001
	beq skip_scroll
	lda scroll	
	sec
	sbc #SCROLL_SPD
	sta scroll
	bne NTSwapCheck
	lda #$EF
	sta scroll


	
NTSwapCheck:
	lda scroll
	cmp #$EF
	bne NTSwapCheckDone
NTSwap:
	lda nametable	;load current nametable
	eor #$01		;excl or of bit 0 will flip bit
	sta nametable	;0->1, 1->0
	inc scroll_pages
NTSwapCheckDone:

NewAttrCheck:
	lda scroll
	and #%00011111	;32
	bne NewAttrCheckDone
	jsr DrawNewAttributes
NewAttrCheckDone:


NewRowCheck:
	lda scroll
	and #%00000111
	bne NewRowCheckDone 
	jsr DrawNewRow
	
	lda row_num
	clc
	adc #$01		;go to next row
	and #%00011111	;only 32 rows of data, throw away top bits
	sta row_num
NewRowCheckDone:

skip_scroll:
	lda #$00
	sta $2003 ;set lo byte of ram address
	lda #$02
	sta $4014 ;sprite DMA from $0200

	;;put gfx updates here
	;;;;;;;;;;;;;;;;;;;;;;
	
;PPU clean up and scroll
	lda #$00
	sta PPUADDR
	sta PPUADDR
	
	lda #$00
	sta PPUSCROLL	;no horz scroll, 0
	
	lda scroll
	sta PPUSCROLL	;vert scroll
	
	lda #%10010000
	ora nametable	;select correct nametable for bit 0
	sta PPUCTRL
	
	lda #%00011110	;enable sprites, bg, no clipping on left side
	sta PPUMASK
	
	jsr read_controller
	jsr move_player_x
	jsr move_player_y
	pull_regs
	rti
;;	END NMI ;;;;;;
;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;state order -> A B select state up down left right
read_controller:	;store controller state
	lda #$01
	sta $4016
	lda #$00
	sta $4016	;tell both controllers to latch buttons
	ldx #$08
store_btn_state:
	lda $4016	;reads next button each call
	lsr a
	rol pad_state
	dex
	bne store_btn_state
	rts
	
	
move_player_x:
	lda pad_state
	and #%00000001
	beq poll_left
	
	lda p1_x
	clc
	adc #PLAYER_SPD
	cmp #$F0
	bcs +
	sta p1_x
	sta $0203
	sta $020B
	clc
	adc #$08
	sta $0207
	sta $020F
	jmp mpx_finished
+	lda #$F0			;clamp to F0h
	sta p1_x
	jmp mpx_finished
poll_left:
	lda pad_state
	and #%00000010
	beq mpx_finished
	
	lda p1_x
	sec
	sbc #PLAYER_SPD
	cmp #$01
	bcc ++
	sta p1_x
	sta $0203
	sta $020B
	clc
	adc #$08
	sta $0207
	sta $020F
	jmp mpx_finished
++	lda #$01
	sta p1_x
mpx_finished:
	rts
	
	
move_player_y:
	lda pad_state
	and #%00000100
	beq poll_up
	
	lda p1_y
	clc
	adc #PLAYER_SPD
	cmp #$E0
	bcs +
	sta p1_y
	sta $0200
	sta $0204
	clc
	adc #$08
	sta $0208
	sta $020C
	jmp mpy_finished
+	lda #$E0			;clamp to F0h
	sta p1_y
	jmp mpy_finished
poll_up:
	lda pad_state
	and #%00001000
	beq mpy_finished
	
	lda p1_y
	sec
	sbc #PLAYER_SPD
	cmp #$10
	bcc ++
	sta p1_y
	sta $0200
	sta $0204
	clc
	adc #$08
	sta $0208
	sta $020C
	jmp mpy_finished
++	lda #$10
	sta p1_y
mpy_finished:
	rts
	
fire_bullet:
	lda pad_state
	and #%10000000 ;A
	beq fb_finish
	
	inc num_bullets
	lda p1_x
	clc
	adc #$08
	sta $0200+BULLET_MEM_OFFS
	
fb_finish:

DrawNewRow:
	mul_32_16bit row_hi, row_num	;mul row_num by 32 and store in row_hi/lo
	sta row_lo
	
	
	lda nametable	;calc new col addr using current nametable
	eor #$01		;invert lo bit
	asl_2			;shift up, A = $00 or $02		;$00 or $04
	clc 
	adc #$20		;add hi byte of nametable addr ($2000)
	adc row_hi
	sta row_hi		;new addr = $20 or $24 
	
	lda row_num		;row number * 32 = row data offset
	asl_5
	sta source_lo
	lda row_num
	lsr_3
	sta source_hi
	
	lda source_lo	;row data start + offs = addr to load col data from
	clc
	adc #<row_data
	sta source_lo
	lda source_hi
	adc #>row_data
	sta source_hi
	
DrawRow:
	lda #%00000000	;increment +1 mode
	sta PPUCTRL
	
	lda PPUSTATUS		;reset latch
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
	eor #$01		;invert lo bit
	asl_2			;shift, $00 or $02			;$00 or $04
	clc 
	adc #$23		;add hi byte of attribute base addr $23C0
	sta row_hi		;new addr = $23 or $27 
	
	lda scroll
	lsr_5
	adc #$C0		;lo addr byte
	sta row_lo		;attribute base + scroll / 32 
	
	lda row_num		;(row number / 4) * 8 = col data offset 
	and #%11111100
	asl a 
	sta source_lo
	lda row_num
	lsr_7
	sta source_hi 
	
	lda source_lo	;row data start + offset = addr to load row data from 
	clc
	adc #<attribData 
	sta source_lo
	lda source_hi
	adc #>attribData
	sta source_hi
	
	ldy #$00
	lda PPUSTATUS	;reset latch 
DrawNewAttributesLoop:
	lda row_hi
	sta PPUADDR		;write the hi byte of col addr
	lda row_lo	
	sta PPUADDR		;write lo byte
	lda (source_lo), y	; pointer, word addr of sourceLo and sourceHi
	sta PPUDATA
	
	iny
	cpy #$08
	beq DrawNewAttributesLoopDone
	jmp DrawNewAttributesLoop
DrawNewAttributesLoopDone:

	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
palette:
	db $0F,$16,$07,$28,  $0F,$01,$21,$31,  $0F,$16,$07,$28,  $0F,$16,$07,$28   ;;background palette
	db $0F,$10,$20,$11,  $0F,$01,$21,$2A,  $0F,$10,$20,$11,  $0F,$10,$20,$11   ;;sprite palette

p1_ship:
	;   vert tile attr horiz
	db $80, $00, $00, $80
	db $80, $01, $00, $88
	db $88, $10, $00, $80
	db $88, $11, $00, $88

p1_bullet:
	db $FE, $06, $01, $FE

p1_bullet_mem_offsets:
	db $04,$08,$0C,$10,$14,$18,$1C,$20
	
row_data:
	incbin "feenix\feenix.nam"
	
attribData:
	incbin "feenix\feenix.atr"



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	org $FFFA		;IRQ vector address
	dw NMI			;cpu jumps to NMI label per frame if enabled
	dw RESET		;called when cpu is turned on or reset
	dw 0			;external IRQ, not used
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;graphics
	incbin "feenix\feenix.chr" ;8KB gfx bin
	
	