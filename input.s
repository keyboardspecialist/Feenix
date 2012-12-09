PAD_A	=	#%10000000
PAD_B	=	#%01000000
PAD_SEL	=	#%00100000
PAD_START	=	#%00010000
PAD_UP	=	#%00001000
PAD_DOWN	=	#%00000100
PAD_LEFT	=	#%00000010
PAD_RIGHT	=	#%00000001

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;state order -> A B select state up down left right
read_controller:	;store controller state
	lda #$01
	sta JOYPAD1
	lda #$00
	sta JOYPAD1		;tell both controllers to latch buttons
	ldx #$08
store_btn_state:
	lda JOYPAD1		;reads next button each call
	lsr 
	rol pad_state
	dex
	bne store_btn_state
	rts
	
	
move_player_x:
	lda pad_state
	and #PAD_RIGHT
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
	and #PAD_LEFT
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
	and #PAD_DOWN
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
+	lda #$E0			;clamp to E0h
	sta p1_y
	jmp mpy_finished
poll_up:
	lda pad_state
	and #PAD_UP
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
	and #PAD_A
	beq fb_finish
	
	inc num_bullets
	lda p1_x
	clc
	adc #$08
	sta $0200+BULLET_MEM_OFFS
	
fb_finish:
	rts