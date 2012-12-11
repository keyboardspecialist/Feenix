;; Feenix NES
;; Remake of the infamous Phoenix 89 clone
;;
;; Jason Sobotka
;	header defines
	PRG_COUNT = 1 
	CHR_COUNT = 1
	MIRRORING = 0000b	;horz

	db "NES", 1Ah		;NES header, standard NROM
	db PRG_COUNT		;16KB PRG ROM
	db CHR_COUNT		;8KB CHR ROM
	db $00|MIRRORING 	;mapper 0, horz mirror
	dsb 9, $00			;pad
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
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
	nt_data_lo		dsb 1	;lo byte nametable data
	nt_data_hi		dsb 1	;hi byte nt data
	atr_data_lo		dsb 1	;lo byte attribute data
	atr_data_hi		dsb 1	;hi byte attribute data
	row_lo			dsb 1	;row lo address
	row_hi			dsb 1	;row hi address
	source_lo		dsb 1	;data lo address
	source_hi		dsb 1	;data hi address
	row_num			dsb 1	;which row to draw
	pad_state		dsb 1	;controller pad state
	p1_x			dsb 1	;player 1 X location
	p1_y			dsb 1	;player 1 Y location
	num_bullets		dsb 1	;number of fired bullets on screen
	vram_update_ready	dsb 1	;notify nmi that data is ready for ppu write
	game_state		dsb 1	;current state
	ende

	SCROLL_SPD = 02h
	PLAYER_SPD = 02h
	
	;game states
	STATE_MAIN_MENU = 01h
	STATE_INIT_LEVEL	= 02h
	STATE_PLAYING	= 04h
	
	BULLET_MEM_OFFS = 10h	;starting offset for bullets in sprite mem
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;first code bank
	.base $10000-(PRG_COUNT*$4000)
	
	include "feenix\nes.s"
	include "feenix\math.s"
	include "feenix\graphics.s"
	include "feenix\input.s"
	include "feenix\famitone_asm6.s"	;thanks to Shiru
	
;	standard system reset code	
RESET:
	sei			;disable IRQs
	cld			;disable decimal mode, 2A03 doesn't have it
	ldx #$40
	stx $4017	;disable apu frame irq
	ldx #$FF
	txs			;setup stack
	inx			;X = 0
	stx PPUCTRL ;disable NMI
	stx PPUMASK ;disable rendering
	stx $4010 	;disable DMC IRQs
	
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;; SYSTEM RESET FINISHED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	lda #$00		;init our variables
	sta scroll_flag
	sta row_num
	sta pad_state
	sta vram_update_ready
	
	lda #$01	;init our sound lib, non-zero for NTSC
	jsr FamiToneInit
	
	lda #$00
	sta scroll
	lda #STATE_MAIN_MENU
	sta game_state
	jsr init_main_menu

	lda #%10010000 ;enable NMI, sprites from pattern table 0, bg from table 1
	sta PPUCTRL
	
	lda #%00011010 ; enable background, no clipping on left side
	sta PPUMASK
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;; MAIN CPU THREAD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
_mainCPUThread:
	lda vram_update_ready
	bne _mainCPUThread		;wait for NMI to finish

	jsr FamiToneUpdate
	jsr read_controller
	
	lda game_state
	cmp #STATE_MAIN_MENU
	beq jmain_menu
	
	lda game_state
	cmp #STATE_INIT_LEVEL
	beq jinit_level_1

	lda game_state
	cmp #STATE_PLAYING
	beq jgameplay_loop
	
_end_state_update:
	lda #$01
	sta vram_update_ready
	jmp _mainCPUThread
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;; END MAIN CPU THREAD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
	
; springboard jump table
jmain_menu:
	jmp main_menu
jinit_level_1:
	jmp init_level_1
jgameplay_loop:
	jmp gameplay_loop

init_main_menu:
	load_palette main_menu_palette
	load_nametable menu_table, #$00
	load_attribute menu_attr, #$00
	
	ldx #<feenixmarch_module
	ldy #>feenixmarch_module
	jsr FamiToneMusicStart
imm_finished:
	rts
	
main_menu:	
	lda pad_state
	and #PAD_START 		;start
	beq mm_finished
	lda #STATE_INIT_LEVEL	;start pressed, let the games begin
	sta game_state
mm_finished:
	jmp _end_state_update
	
init_level_1:
	lda #$00			;disable PPU
	sta PPUCTRL
	sta PPUMASK
	
	load_palette level_1_palette
	load_nametable level_1_table, #$02
	load_attribute level_1_attr, #$02		
	load_sprite_4	p1_ship, $0200
	
	lda #$80
	sta p1_x
	sta p1_y
	lda #$01
	sta scroll_flag
	lda #$1E
	sta row_num
	lda #$00
	sta scroll
	lda #STATE_PLAYING
	sta game_state
	
	lda #%10010000 ;enable NMI, sprites from pattern table 0, bg from table 1
	sta PPUCTRL
	lda #%00011110 ; enable background, and sprites no clipping on left side
	sta PPUMASK
il1_finished:
	jmp _end_state_update
	
gameplay_loop:
	jsr move_player_x
	jsr move_player_y
gpl_finished:
	jmp _end_state_update
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;; NMI IRQ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	

NMI:
	push_regs
	
	lda vram_update_ready
	beq nmi_idle
	lda scroll_flag
	beq nmi_idle
	lda scroll
	sec
	sbc #SCROLL_SPD
	cmp #$EF
	bcs +
	sta scroll
	bne NTSwapDone
+	lda #$EF
	sta scroll
NTSwap:
	lda nametable	;load current nametable
	eor #%00000010
	sta nametable	;0->2, 2->0
	inc scroll_pages
NTSwapDone:

NewAttrCheck:
	lda scroll
	eor #%00001111	;updates every 16 pixels instead of 32 but should be fine
	and #%00001111
	bne NewAttrCheckDone
	jsr DrawNewAttributes
NewAttrCheckDone:


NewRowCheck:
	lda scroll
	eor #%00000111	;invert 3 low bits then and against them to see if its 7 or F
	and #%00000111
	bne NewRowCheckDone 
	dec row_num
	jsr DrawNewRow
	
	lda row_num
	bne NewRowCheckDone		;loop from 30->0
	lda #$1E
	sta row_num
NewRowCheckDone:

	lda #$00
	sta OAMADDR 	;set lo byte of ram address
	lda #$02
	sta OAMDMA 		;sprite DMA from $0200

nmi_idle:
	lda #$00
	sta vram_update_ready
;PPU clean up and scroll
	lda #$00
	sta PPUADDR
	sta PPUADDR
	
	lda #$00
	sta PPUSCROLL	;no horz scroll, 0
	
	lda scroll
	sta PPUSCROLL	;vert scroll
	
	lda #%10010000
	ora nametable	;select correct nametable
	sta PPUCTRL
	
	lda #%00011110	;enable sprites, bg, no clipping on left side
	sta PPUMASK
	
	pull_regs
	rti
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;; END NMI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	



	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;	DATA 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
main_menu_palette:
	db $0F,$11,$21,$3C,  $0F,$30,$30,$30,  $0F,$16,$07,$28,  $0F,$1A,$2A,$3B  ;background palette
	db $0F,$11,$21,$3C,  $0F,$11,$21,$3C,  $0F,$10,$20,$11,  $0F,$10,$20,$11   ;sprite palette

level_1_palette:
	db $0F,$16,$07,$27,  $0F,$01,$21,$31,  $0F,$11,$21,$3C,  $0F,$1A,$2A,$3B  ;background palette
	db $0F,$10,$20,$11,  $0F,$01,$21,$2A,  $0F,$10,$20,$11,  $0F,$10,$20,$11   ;sprite palette

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

menu_table:
	incbin "feenix\feenix_title.nam"
level_1_table:
	incbin "feenix\feenix.nam"

menu_attr:
	incbin "feenix\feenix_title.atr"
level_1_attr:
	incbin "feenix\feenix.atr"

	include "feenix\feenixmarch.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;	IRQS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	org $FFFA		;IRQ vector address
	dw NMI			;cpu jumps to NMI label per frame if enabled
	dw RESET		;called when cpu is turned on or reset
	dw 0			;external IRQ, not used
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;	CHARACTER ROM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	incbin "feenix\feenix.chr" ;8KB gfx bin
	
	